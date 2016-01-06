{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Backend.Compiler (compileCProgram, compileModuleToLLVM) where

import Latte.BNFC.AbsLatte
import qualified Latte.BNFC.AbsLatte as ABS
import Latte.BNFC.ErrM
import Latte.Internal.Type
import Latte.Internal.BuiltIn
import Latte.Internal.ASTInternal
import Latte.Frontend.TypeChecker

import LLVM.General.AST
import qualified LLVM.General.AST.Instruction as Instruction
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention as CallConv
import LLVM.General.AST.Global
import qualified  LLVM.General.AST.Global as AST.Global
import LLVM.General.AST.Linkage
import qualified LLVM.General.AST.IntegerPredicate as IntPred
import LLVM.General.AST.Type
import qualified LLVM.General.AST as AST
import LLVM.General.AST.Constant
import LLVM.General.Context
import LLVM.General.Module

import qualified Data.Map as Map
import Debug.Trace
import Data.Char
import Control.Applicative (Applicative) 
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Control.Monad.Trans.State as StateT

internalErrMsg = "Internal error during llvm compiling phase"

type CompilerEnv = Map.Map AST.Name AST.Operand
type BlocksEnv = Map.Map AST.Name AST.BasicBlock
type StringConstEnv = Map.Map String AST.Name

emptyEnv = Map.empty

data NumSupplier = NumSupplier [Int]

newNumSupplier :: NumSupplier
newNumSupplier = NumSupplier [ n | n <- [0..]]

data CompilerState = CompilerState {
  environment :: CompilerEnv,
  blocks :: BlocksEnv,
  supplier :: NumSupplier,
  glFunTypeEnv :: TypeEnv,
  stringConsts ::StringConstEnv,
  supplierStr :: NumSupplier
  }

defaultCompilerState = CompilerState {
  environment  = emptyEnv,
  blocks       = emptyEnv,
  supplier     = newNumSupplier,
  glFunTypeEnv = emptyEnv,
  stringConsts = emptyEnv,
  supplierStr  = newNumSupplier
  }

newtype CompilerType a = CompilerType (StateT CompilerState Err a)
  deriving (Functor, Applicative, Monad)

runCompilerType :: CompilerType a -> Err a
runCompilerType (CompilerType x) =
  evalStateT x defaultCompilerState

resetCompilerState ::  CompilerType ()
resetCompilerState = CompilerType $ modify $ (\s -> defaultCompilerState {
  glFunTypeEnv = glFunTypeEnv s,
  stringConsts = stringConsts s,
  supplierStr = supplierStr s
  })

putEnv :: CompilerEnv -> CompilerType ()
putEnv env = CompilerType $ modify $ \s -> s { environment = env }

getEnv :: CompilerType CompilerEnv
getEnv = CompilerType $ gets environment

endOfStringSign = "\00"

addStringConst :: String -> CompilerType Name
addStringConst str = do
  name <- getNewStringConstName
  let str' = str ++  endOfStringSign
  CompilerType $ modify $ (\s -> s { stringConsts = Map.insert str' name (stringConsts s) })
  return name

getRefToStringConst :: String -> CompilerType ([AST.Named AST.Instruction], AST.Operand)
getRefToStringConst str = do
  strConstEnv <- CompilerType $ gets stringConsts
  case (Map.lookup str strConstEnv) of
    Nothing -> do
      name <- addStringConst str
      ref <- getNewLocalName
      let getPtr = ref := Instruction.GetElementPtr True (ConstantOperand $
               GlobalReference (constStringType (length str)) name) [getIntConstOper 0, getIntConstOper 0] []
      return ([getPtr], LocalReference stringType ref)
    Just name -> do
      ref <- getNewLocalName
      let getPtr = ref := Instruction.GetElementPtr True (ConstantOperand $
                     GlobalReference (constStringType (length str)) name) [getIntConstOper 0, getIntConstOper 0] []
      return ([getPtr], LocalReference stringType ref)

getStrConstDefs :: CompilerType [AST.Definition]
getStrConstDefs = do
  strConstEnv <- CompilerType $ gets stringConsts
  return $ map (\(str, name) -> globalStringConst name str) (Map.toList strConstEnv)

addToEnv :: AST.Name -> AST.Operand -> CompilerType ()
addToEnv ident t = CompilerType $ modify $ (\s -> s { environment = Map.insert ident t (environment s) })

removeFromEnv :: AST.Name -> CompilerType ()
removeFromEnv ident = CompilerType $ modify $ (\s -> s { environment = Map.delete ident (environment s) })

lookupEnv :: AST.Name -> CompilerType AST.Operand
lookupEnv ident = do
  env <- getEnv
  maybe (fail internalErrMsg) (return . id) (Map.lookup ident env)

getRetType :: String -> CompilerType AST.Type
getRetType ident = do
  tenv <- CompilerType $ gets glFunTypeEnv
  case (Map.lookup ident tenv) of
    Just (t, _, _)  -> case t of
      TFun rType _ -> compileType rType
      _ -> fail internalErrMsg
    Nothing -> fail internalErrMsg

addBlock :: AST.BasicBlock -> CompilerType ()
addBlock bb@(AST.BasicBlock name _ _) =
  CompilerType $ modify $ \s -> s { blocks = Map.insert name bb $ blocks s }

getNewNr' :: CompilerState -> (Int, CompilerState)
getNewNr' s =
  (head num, s { supplier = NumSupplier $ tail num })
 where NumSupplier num = supplier s

getNewNr :: CompilerType Int
getNewNr = CompilerType $ state getNewNr'

getNewStrNr' :: CompilerState -> (Int, CompilerState)
getNewStrNr' s =
  (head num, s { supplierStr = NumSupplier $ tail num })
 where NumSupplier num = supplierStr s

getNewStrNr :: CompilerType Int
getNewStrNr = CompilerType $ state getNewNr'

getNewStringConstName :: CompilerType Name
getNewStringConstName = do
  newNr <- getNewStrNr
  return (Name $ ".str" ++ show newNr)

getNewLabel :: CompilerType Name
getNewLabel = do
  newNr <- getNewNr
  return (UnName $ fromIntegral newNr)

getNewLocalName :: CompilerType Name
getNewLocalName = do
  newNr <- getNewNr
  return (UnName $ fromIntegral newNr)

newModule :: String -> [AST.Definition] -> AST.Module
newModule name defs = defaultModule { moduleName = name, moduleDefinitions = defs }

newGlobalFunctionDef :: AST.Type -> AST.Name -> [AST.Parameter] -> [AST.BasicBlock] -> AST.Definition
newGlobalFunctionDef tRet name params body =
  GlobalDefinition $ functionDefaults {
    returnType = tRet,
    name = name,
    parameters = (params, False),
    basicBlocks = body
    }

newFunDecl :: AST.Type -> AST.Name -> [AST.Parameter] -> AST.Definition
newFunDecl tRet name params =
  GlobalDefinition $ functionDefaults {
    returnType = tRet,
    name = name,
    parameters = (params, False),
    basicBlocks = []
  }

data BasicBlockInfo = BasicBlockInfo {
  blockName :: AST.Name,
  blockBody :: [AST.Named AST.Instruction],
  blockTerminator :: Maybe (AST.Named AST.Terminator)
  } deriving (Show)

defaultBasicBlockInfo :: BasicBlockInfo
defaultBasicBlockInfo = BasicBlockInfo { blockName = Name "NO_NAME", blockBody = [], blockTerminator = Nothing }

funDeclForBuiltIns' :: ABS.Type -> String -> [(ABS.Type, String)] -> CompilerType AST.Definition
funDeclForBuiltIns' rType name args = do
  rType' <- compileType rType
  let name' = Name name
  params <- mapM (\(at, aname) -> do
    at' <- compileType at
    let aname' = Name aname
    return $ Parameter at' aname' []) args
  return $ newFunDecl rType' name' params

funDeclForBuiltIns :: CompilerType [AST.Definition]
funDeclForBuiltIns = mapM (\(t, n, a) -> funDeclForBuiltIns' t n a) builtInsDescs

compileBasicBlockInfo :: BasicBlockInfo -> CompilerType AST.BasicBlock
compileBasicBlockInfo bbi =
  case (blockTerminator bbi) of
    Just terminator -> return $ BasicBlock (blockName bbi) (blockBody bbi) terminator
    Nothing -> fail $ internalErrMsg ++ " compileBasicBlockInfo"

addInstrs :: BasicBlockInfo -> [AST.Named AST.Instruction] -> BasicBlockInfo
addInstrs bb instrs = bb { blockBody = (blockBody bb) ++ instrs }

defaultAddrSpace = AddrSpace 0

boolType = i1
charType = i8
charBits = 8
intType = i32
intBits = 32
stringType = PointerType charType defaultAddrSpace

constStringType :: Int -> AST.Type
constStringType size = ArrayType (fromIntegral size) charType

compileType :: ABS.Type -> CompilerType AST.Type
compileType x = case x of
  TType (TBuiltIn BIInt) -> return $ intType
  TType (TBuiltIn BIVoid) -> return VoidType
  TType (TBuiltIn BIBool) -> return boolType
  TType (TBuiltIn BIStr) -> return stringType
  _ -> fail $ internalErrMsg ++ " compileType"

getIntConstOper :: Integer -> Operand
getIntConstOper val = ConstantOperand $ Int { integerBits = intBits, integerValue = val }

getBoolConstOper :: Bool -> Operand
getBoolConstOper b = case b of
  True -> ConstantOperand $ Int { integerBits = 1, integerValue = 1 }
  False -> ConstantOperand $ Int { integerBits = 1, integerValue = 0 }

getCharConst :: Char -> Constant
getCharConst c = Int charBits $ fromIntegral $ ord c

getCharConstOper :: Char -> Operand
getCharConstOper c = ConstantOperand $ getCharConst c

defaultValue :: ABS.Type -> CompilerType ([AST.Named AST.Instruction], AST.Operand)
defaultValue x = case x of
  TType (TBuiltIn BIInt) -> return $ ([], getIntConstOper 0)
  TType (TBuiltIn BIVoid) -> fail internalErrMsg
  TType (TBuiltIn BIBool) -> return $ ([], getBoolConstOper False)
  TType (TBuiltIn BIStr) -> do
    ref <- getNewLocalName
    return ([ref := Instruction.GetElementPtr True (ConstantOperand $
     GlobalReference (constStringType 0) (Name ".str_default")) [getIntConstOper 0, getIntConstOper 0] []],
      LocalReference stringType ref)
  _ -> fail $ internalErrMsg ++ " compileType"

globalStringConst :: Name -> String -> Definition
globalStringConst name s =
  GlobalDefinition $ globalVariableDefaults {
    name = name,
    linkage = Private, --TODO should it be here
    hasUnnamedAddr = True, --TODO should it be here
    isConstant = True,
    AST.Global.type' = constStringType $ length s,
    initializer  = Just $ Array charType (map getCharConst s)
    }

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

--TODO - make this error print "ERROR\n" etc
compileModuleToLLVM :: AST.Module -> IO String
compileModuleToLLVM mod = withContext $ \context ->
  liftError $ withModuleFromAST context mod $ \m -> do
    compiled <- moduleLLVMAssembly m
    return compiled

compileCProgram' :: String -> CProgram -> CompilerType AST.Module
compileCProgram' name prog@(CProgram topdefs) = do
  CompilerType $ modify (\s -> s { glFunTypeEnv = getGlobalCDefsTypesEnv prog })
  decls <- funDeclForBuiltIns
  defs <- mapM compileCTopDef topdefs
  strConstDefs <- getStrConstDefs
  let strDefault = globalStringConst (Name ".str_default") ""
  return $ newModule name ([strDefault] ++ strConstDefs ++ decls ++ defs)

compileCProgram :: String -> CProgram -> Err AST.Module
compileCProgram name prog = runCompilerType $ compileCProgram' name prog

store :: AST.Operand -> AST.Operand -> CompilerType ([AST.Named AST.Instruction])
store address value = return $ [Do $ Store False address value Nothing 0 []]

storeVar :: String -> AST.Operand -> CompilerType [Named AST.Instruction]
storeVar ident value = do
  oper <- lookupEnv (Name ident)
  store oper value

alloc :: AST.Type -> CompilerType ([AST.Named AST.Instruction], AST.Operand)
alloc t = do
  ref <- getNewLocalName
  return ([ref := Alloca t Nothing 0 []], LocalReference (PointerType t defaultAddrSpace) ref)

load :: AST.Operand -> CompilerType ([Named AST.Instruction], AST.Operand)
load op = case op of
  LocalReference t name -> do
    ref <- getNewLocalName
    return ([ref := Load False op Nothing 0 []], LocalReference (pointerReferent t) ref)
  _ -> fail internalErrMsg
--TODO - pointerReferent t - monie miec tego pola jezeli to nie jest PointerType

loadVar :: String -> CompilerType ([Named AST.Instruction], AST.Operand)
loadVar ident = do
  oper <- lookupEnv (Name ident)
  load oper

allocAndStore :: AST.Type -> AST.Name -> Operand -> CompilerType [AST.Named AST.Instruction]
allocAndStore t name oper = do
  (inst, op) <- alloc t
  inst2 <- store op oper
  addToEnv name op
  return $ inst ++ inst2

callC :: AST.Name -> AST.Type -> [CTExpr] -> CompilerType ([Named AST.Instruction], AST.Operand)
callC name retType args = do
  compiled <- mapM compileCExpr args
  let compiledArgs = map snd compiled
      instrs = concatMap fst compiled
  ref <- getNewLocalName
  return (instrs ++ [ref := Call Nothing CallConv.C [] (Right $ ConstantOperand $ GlobalReference retType name)
    (map (\arg -> (arg, [])) compiledArgs) [] []], LocalReference (pointerReferent intType) ref)

compileCTopDef :: CTopDef -> CompilerType AST.Definition
compileCTopDef x = case x of
  CTDFnDef tret ident args block -> do
    resetCompilerState
    tret' <- compileType tret
    let name = Name ident
    params <- mapM (\(CArg atype aid) -> do
      atype' <- compileType atype
      let aname = Name aid
      return $ Parameter atype' aname []) args
    instrs <- fmap concat $ mapM (\(Parameter atype aname _) ->
      allocAndStore atype aname (LocalReference atype aname)) params
    label  <- getNewLabel
    let startBlock = defaultBasicBlockInfo { blockName = label, blockBody = instrs }
    (bblockis, _) <- compileCBlock startBlock block
    bblocks <- mapM compileBasicBlockInfo bblockis
    return $ newGlobalFunctionDef tret' name params bblocks

compileCBlock :: BasicBlockInfo -> CBlock -> CompilerType ([BasicBlockInfo], (Maybe BasicBlockInfo))
compileCBlock bb (CBlock stmts) = do
  oldEnv <- getEnv
  (bbsnew, bbnew) <- foldM (\(bbs, bbnow) -> \stmt -> do
     case bbnow of
       Nothing -> return (bbs, bbnow)
       Just now -> do
         (bbs', bbnow') <- compileCStmt now stmt
         return (bbs ++ bbs', bbnow')) ([], Just bb) stmts
  putEnv oldEnv
  return (bbsnew, bbnew)

compileCStmt :: BasicBlockInfo -> CStmt -> CompilerType ([BasicBlockInfo], (Maybe BasicBlockInfo))
compileCStmt bb x = case x of
  CSEmpty               -> return ([], Just bb)
  CSBlock block -> compileCBlock bb block
  CSExp expr            -> do
    (instrs, _) <- compileCExpr expr
    return ([], Just $ addInstrs bb instrs)
  CSRet expr          -> do
    (instrs, val) <- compileCExpr expr
    let bb' = addInstrs bb instrs
        bb'' = bb' { blockTerminator = Just $ Do $ Ret { returnOperand = Just val, metadata' = [] } }
    return ([bb''], Nothing)
  CSVRet              -> do
    let bb' = bb { blockTerminator = Just $ Do $ Ret { returnOperand = Nothing, metadata' = [] } }
    return ([bb'], Nothing)
  CSDecl t items -> do
    t' <- compileType t
    bb' <- foldM (\bb -> \item -> case item of
      CINoInit ident   -> do
        (instrs1, val) <- defaultValue t
        instrs2 <- allocAndStore t' (Name ident) val
        let instrs = (instrs1 ++ instrs2)
        --traceM (show instrs)
        return $ addInstrs bb instrs
      CIInit ident exp -> do
        (instrs1, val) <- compileCExpr exp
        instrs2 <- allocAndStore t' (Name ident) val
        return $ addInstrs bb (instrs1 ++ instrs2)
          ) bb items
    return ([], Just bb')
  CSAss ident expr -> do
    (instrs1, val) <- compileCExpr expr
    instrs2 <- storeVar ident val
    return ([], Just $ addInstrs bb (instrs1 ++ instrs2))
  CSCondElse expr stmt1 stmt2 -> do
    (instrs1, val) <- compileCExpr expr
    trueLabel <- getNewLabel
    let trueBlock = defaultBasicBlockInfo { blockName = trueLabel }
    (bbisT, mbbiT) <- compileCStmt trueBlock stmt1
    falseLabel <- getNewLabel
    let falseBlock = defaultBasicBlockInfo { blockName = falseLabel }
    (bbisF, mbbiF) <- compileCStmt falseBlock stmt2
    afterCond <- getNewLabel
    let bbisT' = maybe bbisT (\bbi -> bbisT ++ [bbi { blockTerminator = Just $ Do $ Br afterCond [] }]) mbbiT
        bbisF' = maybe bbisF (\bbi -> bbisF ++ [bbi { blockTerminator = Just $ Do $ Br afterCond [] }]) mbbiF
        nowTerm = Just $ Do $ CondBr val trueLabel falseLabel []
        bb' = (addInstrs bb instrs1) { blockTerminator = nowTerm}
    return ([bb'] ++ bbisT' ++ bbisF', Just $ defaultBasicBlockInfo { blockName = afterCond })
  CSCond expr stmt1 -> do
    (instrs1, val) <- compileCExpr expr
    trueLabel <- getNewLabel
    let trueBlock = defaultBasicBlockInfo { blockName = trueLabel }
    (bbisT, mbbiT) <- compileCStmt trueBlock stmt1
    afterCond <- getNewLabel
    let bbisT' = maybe bbisT (\bbi -> bbisT ++ [bbi { blockTerminator = Just $ Do $ Br afterCond [] }]) mbbiT
        nowTerm = Just $ Do $ CondBr val trueLabel afterCond []
        bb' = (addInstrs bb instrs1) { blockTerminator = nowTerm}
    return ([bb'] ++ bbisT', Just $ defaultBasicBlockInfo { blockName = afterCond })
  CSWhile expr stmt -> do
    condLabel <- getNewLabel
    let bb' = bb { blockTerminator = Just $ Do $ Br condLabel []}
        condBlock = defaultBasicBlockInfo { blockName = condLabel }
    (instrs, val) <- compileCExpr expr
    whileBodyLabel <- getNewLabel
    let whileBodyBlock = defaultBasicBlockInfo { blockName = whileBodyLabel }
    (bbis, mbbi) <- compileCStmt whileBodyBlock stmt
    afterWhileLabel <- getNewLabel
    let bbis' = maybe bbis (\bbi -> bbis ++ [bbi { blockTerminator = Just $ Do $ Br condLabel [] }]) mbbi
        condBlock' = condBlock { blockBody = instrs, blockTerminator =  Just $ Do $ CondBr val whileBodyLabel afterWhileLabel [] }
    return ([bb', condBlock'] ++ bbis', Just $ defaultBasicBlockInfo { blockName = afterWhileLabel } )

binOprs = Map.fromList [
  ("<", AST.ICmp IntPred.SLT),
  ("<=", AST.ICmp IntPred.SLE),
  (">", AST.ICmp IntPred.SGT),
  (">=", AST.ICmp IntPred.SGE),
  ("==", AST.ICmp IntPred.EQ),
  ("!=", AST.ICmp IntPred.NE),
  ("+", AST.Add False False),
  ("-", AST.Sub False False),
  ("*", AST.Mul False False),
  ("/", AST.SDiv False),
  ("%", AST.SRem),
  ("||", AST.And),
  ("&&", AST.Or),
  ("^", AST.Xor)
  ]

compileCExpr :: CTExpr -> CompilerType ([AST.Named AST.Instruction], AST.Operand)
compileCExpr (x, tx) = case x of
  CELit lit -> case lit of
    LInt val    -> return ([], getIntConstOper val)
    LTrue       -> return ([], getBoolConstOper True)
    LFalse      -> return ([], getBoolConstOper False)
    LString str -> getRefToStringConst str
  CEApp ident args -> do
    retType <- getRetType ident
    callC (Name ident) retType args
  CEVar ident -> loadVar ident
  CBinOp expr1 opr expr2 -> compileCBinOpr expr1 expr2 opr tx

compileCBinOpr :: CTExpr -> CTExpr -> String -> ABS.Type -> CompilerType ([AST.Named AST.Instruction], AST.Operand)
compileCBinOpr expr1 expr2 binOp retT = do
  cretT <- compileType retT
  let binOpF = (binOprs Map.! binOp)
  (instrs1, oper1) <- compileCExpr expr1
  (instrs2, oper2) <- compileCExpr expr2
  ref <- getNewLocalName
  let ass = ref := binOpF oper1 oper2 []
  return  (instrs1 ++ instrs2 ++ [ass], LocalReference cretT ref)
