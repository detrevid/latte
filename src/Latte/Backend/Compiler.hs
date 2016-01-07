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

import Control.Conditional
import qualified Control.Conditional as Cond
import qualified Data.Map as Map
import Debug.Trace
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative (Applicative)
import Control.Monad.State
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.State as StateT

internalErrMsg = "Internal error during llvm compiling phase"

type CompilerEnv = Map.Map AST.Name AST.Operand
type BlocksEnv = Map.Map AST.Name BasicBlockInfo
type StringConstEnv = Map.Map String AST.Name

emptyEnv = Map.empty

data NumSupplier = NumSupplier [Int] deriving (Show)

newNumSupplier :: NumSupplier
newNumSupplier = NumSupplier [ n | n <- [0..]]

data CompilerState = CompilerState {
  environment :: CompilerEnv,
  blocksEnv :: BlocksEnv,
  blocks :: [AST.Name],
  currentBlock :: AST.Name,
  supplier :: NumSupplier,
  glFunTypeEnv :: TypeEnv,
  stringConsts ::StringConstEnv,
  supplierStr :: NumSupplier
  } deriving (Show)

defaultCompilerState = CompilerState {
  environment  = emptyEnv,
  blocksEnv    = emptyEnv,
  blocks       = [],
  currentBlock = error "No current block",
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

addInstrsToBlock :: AST.Name -> [AST.Named AST.Instruction] -> CompilerType ()
addInstrsToBlock name instrs = do
  CompilerType $ modify (\s -> s { blocksEnv =
    (Map.update (\x -> Just ((flip addInstrs) instrs x)) name (blocksEnv s))})

addInstrsToCurrentBlock :: [AST.Named AST.Instruction] -> CompilerType ()
addInstrsToCurrentBlock instrs = do
  current <- getCurrentBlock
  addInstrsToBlock current instrs

setBlockTerminator :: AST.Name -> AST.Named AST.Terminator -> CompilerType ()
setBlockTerminator name term = do
  CompilerType $ modify (\s -> s { blocksEnv =
    (Map.update (\x -> Just (x { blockTerminator = Just term })) name (blocksEnv s)) })

setUnBlockTerminator :: AST.Name -> AST.Named AST.Terminator -> CompilerType ()
setUnBlockTerminator name term = do
  ifterm <- isBlockTerminated name
  if not ifterm
    then CompilerType $ modify (\s -> s { blocksEnv =
      (Map.update (\x -> Just (x { blockTerminator = Just term })) name (blocksEnv s)) })
    else return ()

setCurrentBlockTerminator :: AST.Named AST.Terminator -> CompilerType ()
setCurrentBlockTerminator term = do
  current <- getCurrentBlock
  setBlockTerminator current term

newBlock :: CompilerType AST.Name
newBlock = do
  name <- getNewLabel
  addNextBlock name
  let newBlock = defaultBasicBlockInfo { blockName = name }
  CompilerType $ modify (\s -> s { blocksEnv = Map.insert name newBlock (blocksEnv s) })
  return name

getBlock :: AST.Name -> CompilerType BasicBlockInfo
getBlock name = do
  benv <- CompilerType $ gets blocksEnv
  maybe (fail $ internalErrMsg ++ " getBlock") (return . id) $ Map.lookup name benv

isBlockTerminated :: AST.Name -> CompilerType Bool
isBlockTerminated name = do
  block <- getBlock name
  return $ not $ isNothing $ blockTerminator block

setCurrentBlock :: Name -> CompilerType ()
setCurrentBlock name = CompilerType $ modify (\s -> s { currentBlock = name })

getCurrentBlock :: CompilerType Name
getCurrentBlock = CompilerType $ gets currentBlock

isCurrentBlockTerminated :: CompilerType Bool
isCurrentBlockTerminated = do
  currentBlock <- getCurrentBlock
  isBlockTerminated currentBlock

removeCurrentBlock :: CompilerType ()
removeCurrentBlock = do
  currentBlock <- getCurrentBlock
  CompilerType $ modify (\s -> s {
    blocksEnv = Map.delete currentBlock (blocksEnv s),
    blocks = delete currentBlock (blocks s)})

addNextBlock :: Name -> CompilerType ()
addNextBlock name = CompilerType $ modify (\s -> s { blocks = (blocks s) ++ [name] })

getBasicBlocks :: CompilerType [AST.BasicBlock]
getBasicBlocks = do
  bl <- CompilerType $ gets blocks
  mapM (\b -> do
    bbi <- getBlock b
    compileBasicBlockInfo bbi) bl

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
  case (Map.lookup (str ++ endOfStringSign) strConstEnv) of
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
      _ -> fail $ internalErrMsg ++ "Just getRetType"
    Nothing -> fail $ internalErrMsg ++ "Nothing getRetType"

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
defaultBasicBlockInfo = BasicBlockInfo { blockName = error "Unnamed block", blockBody = [], blockTerminator = Nothing }

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

defaultValue :: ABS.Type -> CompilerType AST.Operand
defaultValue x = case x of
  TType (TBuiltIn BIInt) -> return $ getIntConstOper 0
  TType (TBuiltIn BIVoid) -> fail $  internalErrMsg ++ " defaultValue"
  TType (TBuiltIn BIBool) -> return $ getBoolConstOper False
  TType (TBuiltIn BIStr) -> do
    ref <- getNewLocalName
    addInstrsToCurrentBlock [ref := Instruction.GetElementPtr True (ConstantOperand $
      GlobalReference (constStringType 0) (Name ".str_default")) [getIntConstOper 0, getIntConstOper 0] []]
    return $ LocalReference stringType ref
  _ -> fail $ internalErrMsg ++ " defaultValue"

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

compileModuleToLLVM :: AST.Module -> IO String
compileModuleToLLVM mod = withContext $ \context ->
  Except.runExceptT >=> either (fail . ((++) "ERROR\n")) return $
    withModuleFromAST context mod $ \m -> do
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

store :: AST.Operand -> AST.Operand -> CompilerType ()
store address value = addInstrsToCurrentBlock [Do $ Store False address value Nothing 0 []]

storeVar :: String -> AST.Operand -> CompilerType ()
storeVar ident value = do
  oper <- lookupEnv (Name ident)
  store oper value

alloc :: AST.Type -> CompilerType AST.Operand
alloc t = do
  ref <- getNewLocalName
  addInstrsToCurrentBlock [ref := Alloca t Nothing 0 []]
  return $ LocalReference (PointerType t defaultAddrSpace) ref

load :: AST.Operand -> CompilerType AST.Operand
load op = case op of
  LocalReference t name -> do
    ref <- getNewLocalName
    addInstrsToCurrentBlock [ref := Load False op Nothing 0 []]
    return $ LocalReference (pointerReferent t) ref
  _ -> fail $ internalErrMsg ++ " load"
--TODO - pointerReferent t - monie miec tego pola jezeli to nie jest PointerType

loadVar :: String -> CompilerType AST.Operand
loadVar ident = do
  oper <- lookupEnv (Name ident)
  load oper

allocAndStore :: AST.Type -> AST.Name -> Operand -> CompilerType ()
allocAndStore t name oper = do
  op <- alloc t
  store op oper
  addToEnv name op
  return ()

callC :: AST.Name -> AST.Type -> [CTExpr] -> CompilerType AST.Operand
callC name retType args = do
  compiledArgs <- mapM compileCExpr args
  ref <- getNewLocalName
  addInstrsToCurrentBlock [ref := Call Nothing CallConv.C [] (Right $ ConstantOperand $
    GlobalReference retType name) (map (\arg -> (arg, [])) compiledArgs) [] []]
  return $ LocalReference (pointerReferent intType) ref

compileCTopDef :: CTopDef -> CompilerType AST.Definition
compileCTopDef x = case x of
  CTDFnDef tret ident args block -> do
    resetCompilerState
    entryLabel <- newBlock
    setCurrentBlock entryLabel
    tret' <- compileType tret
    let name = Name ident
    params <- mapM (\(CArg atype aid) -> do
      atype' <- compileType atype
      let aname = Name aid
      return $ Parameter atype' aname []) args
    mapM_ (\(Parameter atype aname _) -> allocAndStore atype aname (LocalReference atype aname)) params
    compileCBlock block
    unlessM (isCurrentBlockTerminated) removeCurrentBlock
    bblocks <- getBasicBlocks
    return $ newGlobalFunctionDef tret' name params bblocks

compileCBlock :: CBlock -> CompilerType ()
compileCBlock (CBlock stmts) = do
  oldEnv <- getEnv
  mapM_ (\stmt -> unlessM (isCurrentBlockTerminated) (compileCStmt stmt)) stmts
  putEnv oldEnv

compileCStmt :: CStmt -> CompilerType ()
compileCStmt x = case x of
  CSEmpty               -> return ()
  CSBlock block -> compileCBlock block
  CSExp expr            -> compileCExpr expr >> return ()
  CSRet expr          -> do
    val <- compileCExpr expr
    setCurrentBlockTerminator $ Do $ Ret { returnOperand = Just val, metadata' = [] }
    return ()
  CSVRet              -> do
    setCurrentBlockTerminator $ Do $ Ret { returnOperand = Nothing, metadata' = [] }
    return ()
  CSDecl t items -> do
    t' <- compileType t
    mapM_ (\item -> case item of
      CINoInit ident   -> do
        val <- defaultValue t
        allocAndStore t' (Name ident) val
      CIInit ident exp -> do
        val <- compileCExpr exp
        allocAndStore t' (Name ident) val
          ) items
    return ()
  CSAss ident expr -> do
    val <- compileCExpr expr
    storeVar ident val
  CSCondElse expr stmt1 stmt2 -> do
    val <- compileCExpr expr
    afterCondLabel <- getCurrentBlock
    trueLabel <- newBlock
    setCurrentBlock trueLabel
    compileCStmt stmt1
    afterTrue <- getCurrentBlock
    falseLabel <- newBlock
    setCurrentBlock falseLabel
    compileCStmt stmt2
    afterFalse <- getCurrentBlock
    setUnBlockTerminator afterCondLabel (Do $ CondBr val trueLabel falseLabel [])
    termAfterTrue <- isBlockTerminated afterTrue
    termAfterFalse <- isBlockTerminated afterFalse
    Cond.when (not $ termAfterTrue && termAfterFalse) (do
      afterIfLabel <- newBlock
      setUnBlockTerminator afterTrue (Do $ Br afterIfLabel [])
      setUnBlockTerminator afterFalse (Do $ Br afterIfLabel [])
      setCurrentBlock afterIfLabel
      )
    return ()
  CSWhile expr stmt -> do
    condLabel <- newBlock
    setCurrentBlockTerminator $ Do $ Br condLabel []
    setCurrentBlock condLabel
    val <- compileCExpr expr
    whileBodyLabel <- newBlock
    setCurrentBlock whileBodyLabel
    compileCStmt stmt
    afterWhileLabel <- newBlock
    setUnBlockTerminator whileBodyLabel $ Do $ Br condLabel []
    setUnBlockTerminator condLabel $ Do $ CondBr val whileBodyLabel afterWhileLabel []
    setCurrentBlock afterWhileLabel
    return ()

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
  ("||", AST.Or),
  ("&&", AST.And),
  ("^", AST.Xor)
  ]

compileCExpr :: CTExpr -> CompilerType AST.Operand
compileCExpr (x, tx) = case x of
  CELit lit -> case lit of
    LInt val    -> return $ getIntConstOper val
    LTrue       -> return $ getBoolConstOper True
    LFalse      -> return $ getBoolConstOper False
    LString str -> do
      (instrs, oper) <- getRefToStringConst str
      addInstrsToCurrentBlock instrs
      return oper
  CEApp ident args -> do
    retType <- getRetType ident
    oper <- callC (Name ident) retType args
    return oper
  CEVar ident -> loadVar ident
  CBinOp expr1 opr expr2 -> compileCBinOpr expr1 expr2 opr tx

compileCBinOpr :: CTExpr -> CTExpr -> String -> ABS.Type -> CompilerType AST.Operand
compileCBinOpr expr1 expr2 binOp retT = do
  cretT <- compileType retT
  let binOpF = (binOprs Map.! binOp)
  oper1 <- compileCExpr expr1
  case () of
    _ | binOp == "&&" || binOp == "||" -> do
    --TODO uzyc phi
        sol <- alloc cretT
        store sol oper1
        afterFirstExp <- getCurrentBlock
        continueCompLabel <- newBlock
        setCurrentBlock continueCompLabel
        oper2 <- compileCExpr expr2
        store sol oper2
        afterContinueCompLabel <- getCurrentBlock
        afterBinOpLabel <- newBlock
        setCurrentBlock afterBinOpLabel
        solLoad <- load sol
        if binOp == "&&"
          then setUnBlockTerminator afterFirstExp (Do $ CondBr oper1 continueCompLabel afterBinOpLabel [])
          else setUnBlockTerminator afterFirstExp (Do $ CondBr oper1 afterBinOpLabel continueCompLabel [])
        setUnBlockTerminator afterContinueCompLabel (Do $ Br afterBinOpLabel [])
        return solLoad
      | True -> do
        oper2 <- compileCExpr expr2
        ref <- getNewLocalName
        let ass = ref := binOpF oper1 oper2 []
        addInstrsToCurrentBlock [ass]
        return $ LocalReference cretT ref
