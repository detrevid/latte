{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Backend.Compiler (compileCProgram, compileModuleToLLVM) where

import Latte.BNFC.AbsLatte
import qualified Latte.BNFC.AbsLatte as ABS
import Latte.BNFC.ErrM
import Latte.Internal.Type
import Latte.Internal.BuiltIn
import Latte.Internal.ASTInternal

import LLVM.General.AST
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Instruction as Instruction
import LLVM.General.AST.AddrSpace
import qualified LLVM.General.AST.CallingConvention as CallConv
import LLVM.General.AST.Constant
import LLVM.General.AST.Global
import qualified  LLVM.General.AST.Global as AST.Global
import LLVM.General.AST.Linkage
import qualified LLVM.General.AST.IntegerPredicate as IntPred
import LLVM.General.AST.Type
import LLVM.General.Context
import LLVM.General.Module

import Control.Conditional
import qualified Control.Conditional as Cond
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import qualified Data.Int as HInt
import qualified Data.Traversable as Traversable
import Debug.Trace
import Control.Applicative (Applicative)
import Control.Monad.State
import qualified Control.Monad.Except as Except

internalErrMsg = "Internal error during llvm compiling phase"

type CompilerEnv = Map.Map AST.Name AST.Operand
type BlocksEnv = Map.Map AST.Name BasicBlockInfo
type StringConstEnv = Map.Map String AST.Name

emptyEnv = Map.empty

data NumSupplier = NumSupplier [Int] deriving (Show)

newNumSupplier :: NumSupplier
newNumSupplier = NumSupplier [ n | n <- [0..]]

data CompilerState = CompilerState {
  environment  :: CompilerEnv,
  blocksEnv    :: BlocksEnv,
  blocks       :: [AST.Name],
  currentBlock :: AST.Name,
  supplier     :: NumSupplier,
  glFunTypeEnv :: Map.Map String (AST.Type, [AST.Type]),
  classEnv     :: ClassEnv,
  stringConsts :: StringConstEnv,
  supplierStr  :: NumSupplier
  } deriving (Show)

defaultCompilerState = CompilerState {
  environment  = emptyEnv,
  blocksEnv    = emptyEnv,
  blocks       = [],
  currentBlock = error "No current block",
  supplier     = newNumSupplier,
  glFunTypeEnv = emptyEnv,
  classEnv     = emptyClassEnv,
  stringConsts = emptyEnv,
  supplierStr  = newNumSupplier
  }

newtype CompilerType a = CompilerType (StateT CompilerState Err a)
  deriving (Functor, Applicative, Monad, MonadState CompilerState)

runCompilerType :: CompilerType a -> Err a
runCompilerType (CompilerType x) = evalStateT x defaultCompilerState

resetCompilerState ::  CompilerType ()
resetCompilerState = CompilerType $ modify $ (\s -> defaultCompilerState {
  glFunTypeEnv = glFunTypeEnv s,
  classEnv     = classEnv s,
  stringConsts = stringConsts s,
  supplierStr  = supplierStr s
  })

putEnv :: CompilerEnv -> CompilerType ()
putEnv env = modify $ \s -> s { environment = env }

getEnv :: CompilerType CompilerEnv
getEnv = gets environment

addToEnv :: AST.Name -> AST.Operand -> CompilerType ()
addToEnv ident t = modify $ \s -> s { environment = Map.insert ident t (environment s) }

{-
removeFromEnv :: AST.Name -> CompilerType ()
removeFromEnv ident = modify $ \s -> s { environment = Map.delete ident (environment s) }
-}

lookupEnv :: AST.Name -> CompilerType AST.Operand
lookupEnv ident = do
  env <- getEnv
  maybe (fail internalErrMsg) (return . id) (Map.lookup ident env)

getClassInfo :: String -> CompilerType ClassInfo
getClassInfo ident = do
  mci <- gets $ (Map.lookup ident) . classEnv
  maybe (fail internalErrMsg) return mci

addNextBlock :: Name -> CompilerType ()
addNextBlock name = modify (\s -> s { blocks = (blocks s) ++ [name] })

getBlock :: AST.Name -> CompilerType BasicBlockInfo
getBlock name = do
  benv <- gets blocksEnv
  maybe (fail internalErrMsg) (return . id) $ Map.lookup name benv

newBlock :: CompilerType AST.Name
newBlock = do
  name <- getNewLabel
  addNextBlock name
  let newBlock = defaultBasicBlockInfo { blockName = name }
  modify $ \s -> s { blocksEnv = Map.insert name newBlock (blocksEnv s) }
  return name

addInstrsToBlock :: AST.Name -> [AST.Named AST.Instruction] -> CompilerType ()
addInstrsToBlock name instrs = do
  modify $ \s -> s { blocksEnv =
    (Map.update (\x -> Just $ (flip addInstrs) instrs x) name (blocksEnv s))}

isBlockTerminated :: AST.Name -> CompilerType Bool
isBlockTerminated name = do
  block <- getBlock name
  return $ not $ isNothing $ blockTerminator block

setBlockTerminator :: AST.Name -> AST.Named AST.Terminator -> CompilerType ()
setBlockTerminator name term = do
  modify (\s -> s { blocksEnv =
    (Map.update (\x -> Just (x { blockTerminator = Just term })) name (blocksEnv s)) })

setUnBlockTerminator :: AST.Name -> AST.Named AST.Terminator -> CompilerType ()
setUnBlockTerminator name term = do
  ifterm <- isBlockTerminated name
  if not ifterm
    then modify (\s -> s { blocksEnv =
      (Map.update (\x -> Just (x { blockTerminator = Just term })) name (blocksEnv s)) })
    else return ()

setCurrentBlock :: Name -> CompilerType ()
setCurrentBlock name = modify $ \s -> s { currentBlock = name }

getCurrentBlock :: CompilerType Name
getCurrentBlock = gets currentBlock

newCurrentBlock :: CompilerType AST.Name
newCurrentBlock = do
  name <- newBlock
  setCurrentBlock name
  return name

addInstrsToCurrentBlock :: [AST.Named AST.Instruction] -> CompilerType ()
addInstrsToCurrentBlock instrs = do
  current <- getCurrentBlock
  addInstrsToBlock current instrs

setCurrentBlockTerminator :: AST.Named AST.Terminator -> CompilerType ()
setCurrentBlockTerminator term = do
  current <- getCurrentBlock
  setBlockTerminator current term

isCurrentBlockTerminated :: CompilerType Bool
isCurrentBlockTerminated = do
  currentBlock <- getCurrentBlock
  isBlockTerminated currentBlock

getBasicBlocks :: CompilerType [AST.BasicBlock]
getBasicBlocks = do
  bl <- gets blocks
  mapM (\b -> do
    bbi <- getBlock b
    compileBasicBlockInfo bbi) bl

endOfStringSign = "\00"

addStringConst :: String -> CompilerType Name
addStringConst str = do
  name <- getNewStringConstName
  let str' = str ++  endOfStringSign
  modify $ (\s -> s { stringConsts = Map.insert str' name (stringConsts s) })
  return name

getElementPtr :: AST.Type -> Operand -> [Integer] -> CompilerType AST.Operand
getElementPtr t address indices = nameInstruction t $ Instruction.GetElementPtr True address (map intConstOper indices) []

constGlobalReference :: AST.Type -> AST.Name -> AST.Operand
constGlobalReference t n = ConstantOperand $ GlobalReference t n

refToStringConst :: String -> CompilerType AST.Operand
refToStringConst str = do
  strConstEnv <- gets stringConsts
  name <- maybe (addStringConst str) (return . id) (Map.lookup (str ++ endOfStringSign) strConstEnv)
  getElementPtr stringType (constGlobalReference (constStringType (length str)) name) [0, 0]

getStrConstDefs :: CompilerType [AST.Definition]
getStrConstDefs = do
  strConstEnv <- gets stringConsts
  return $ map (\(str, name) -> globalStringConst name str) (Map.toList strConstEnv)

getFunRetType :: String -> CompilerType AST.Type
getFunRetType ident = do
  tenv <- gets glFunTypeEnv
  case (Map.lookup ident tenv) of
    Just (rType, _)  -> return rType
    _ -> fail internalErrMsg

typeInfoToFunInfo :: TypeInfo -> CompilerType (AST.Type, [AST.Type])
typeInfoToFunInfo x = case x of
  (TFun rType args, _, _) -> do
    crType <- compileType rType
    cargs <- mapM compileType args
    return (crType, cargs)
  _ -> fail internalErrMsg

getNewNr' :: CompilerState -> (Int, CompilerState)
getNewNr' s =
  (head num, s { supplier = NumSupplier $ tail num })
 where NumSupplier num = supplier s

getNewNr :: CompilerType Int
getNewNr = state getNewNr'

getNewStrNr' :: CompilerState -> (Int, CompilerState)
getNewStrNr' s =
  (head num, s { supplierStr = NumSupplier $ tail num })
 where NumSupplier num = supplierStr s

getNewStrNr :: CompilerType Int
getNewStrNr = state getNewStrNr'

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

addInstrs :: BasicBlockInfo -> [AST.Named AST.Instruction] -> BasicBlockInfo
addInstrs bb instrs = bb { blockBody = (blockBody bb) ++ instrs }

compileBasicBlockInfo :: BasicBlockInfo -> CompilerType AST.BasicBlock
compileBasicBlockInfo bbi =
  case (blockTerminator bbi) of
    Just terminator -> return $ BasicBlock (blockName bbi) (blockBody bbi) terminator
    Nothing -> fail internalErrMsg

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

mallocIdent = "malloc"
mallocDecl = newFunDecl (PointerType i8 defaultAddrSpace) (Name mallocIdent) [Parameter i32 (UnName  0) []]

additionalCDecls = [mallocDecl]
additionalCDeclsEnv = Map.fromList [(mallocIdent, (PointerType i8 defaultAddrSpace, [i32]))]

defaultAddrSpace = AddrSpace 0

boolBits = 8
boolType = i8
charType = i8
charBits = 8
intType = i32
intBits = 32
maxInt = (toInteger (maxBound :: HInt.Int32))
stringType = PointerType charType defaultAddrSpace
pointerBits = 64

className :: String -> Name
className id = Name $ "class." ++ id

constStringType :: Int -> AST.Type
constStringType size = ArrayType (fromIntegral size) charType

compileType :: ABS.Type -> CompilerType AST.Type
compileType (TType x) = case x of
  TBuiltIn BIInt -> return $ intType
  TBuiltIn BIVoid -> return VoidType
  TBuiltIn BIBool -> return boolType
  TBuiltIn BIStr -> return stringType
  TClass (CType (Ident id)) -> return $ PointerType (NamedTypeReference $ className id) defaultAddrSpace
  _ -> fail internalErrMsg
compileType _ = fail internalErrMsg

bitsInByte = 4

typeBytes :: ABS.Type -> Integer
typeBytes (TType x) = case x of
  TBuiltIn BIInt  -> fromIntegral intBits `quot` bitsInByte
  TBuiltIn BIVoid -> 0
  TBuiltIn BIBool -> fromIntegral boolBits `quot` bitsInByte
  TBuiltIn BIStr  -> fromIntegral pointerBits `quot` bitsInByte
  TClass _        -> fromIntegral pointerBits `quot` bitsInByte
typeBytes _ = 0

classBytes :: String -> CompilerType Integer
classBytes ident = do
  cinfo <- getClassInfo ident
  let fieldsTypes = map fst $ Map.elems $ fst cinfo
      fieldsBytes = map typeBytes fieldsTypes
  return $ sum fieldsBytes

intConstOper :: Integer -> Operand
intConstOper val =
  ConstantOperand $ Int { integerBits = intBits, integerValue = val' }
 where val' = val `rem` (maxInt + 1)

getBoolConstOper :: Bool -> Operand
getBoolConstOper b = case b of
  True -> ConstantOperand $ Int { integerBits = boolBits, integerValue = 1 }
  False -> ConstantOperand $ Int { integerBits = boolBits, integerValue = 0 }

getCharConst :: Char -> Constant
getCharConst c = Int charBits $ fromIntegral $ ord c

{-getCharConstOper :: Char -> Operand
getCharConstOper c = ConstantOperand $ getCharConst c-}

defaultIntVal = intConstOper 0
defaultBoolVal = getBoolConstOper False

--TODO remove maybe
defaultValue :: ABS.Type -> CompilerType AST.Operand
defaultValue (TType x) = case x of
  TBuiltIn BIInt  -> return defaultIntVal
  TBuiltIn BIVoid -> fail internalErrMsg
  TBuiltIn BIBool -> return defaultBoolVal
  TBuiltIn BIStr  -> refToStringConst ""
  TClass _        -> do
    ct <- compileType (TType x)
    return $ ConstantOperand $ Null ct
defaultValue _ = fail internalErrMsg

globalStringConst :: Name -> String -> Definition
globalStringConst name s =
  GlobalDefinition $ globalVariableDefaults {
    name             = name,
    linkage          = Private,
    hasUnnamedAddr   = True,
    isConstant       = True,
    AST.Global.type' = constStringType $ length s,
    initializer      = Just $ Array charType (map getCharConst s)
    }

nameInstruction :: AST.Type -> AST.Instruction -> CompilerType AST.Operand
nameInstruction t inst = do
  ref <- getNewLocalName
  addInstrsToCurrentBlock [ref := inst]
  return $ LocalReference t ref

store :: AST.Operand -> AST.Operand -> CompilerType ()
store address value = addInstrsToCurrentBlock [Do $ Store False address value Nothing 0 []]

{-storeVar :: String -> AST.Operand -> CompilerType ()
storeVar ident value = do
  oper <- lookupEnv (Name ident)
  store oper value-}

alloc :: AST.Type -> CompilerType AST.Operand
alloc t = nameInstruction (PointerType t defaultAddrSpace) $ Alloca t Nothing 0 []

load :: AST.Operand -> CompilerType AST.Operand
load op = case op of
  LocalReference (PointerType pref _) _ -> nameInstruction pref $ Load False op Nothing 0 []
  _ -> fail internalErrMsg

loadVar :: String -> CompilerType AST.Operand
loadVar ident = do
  oper <- lookupEnv (Name ident)
  load oper

allocAndStore :: AST.Type -> AST.Name -> Operand -> CompilerType ()
allocAndStore t name oper = do
  op <- alloc t
  store op oper
  addToEnv name op

call :: String -> [AST.Operand] -> CompilerType AST.Operand
call name args = do
  retType <- getFunRetType name
  nameInstruction (pointerReferent intType) $ Call Nothing CallConv.C [] (Right $
    constGlobalReference retType (Name name)) (map (\arg -> (arg, [])) args) [] []

ret :: Maybe AST.Operand -> AST.Named AST.Terminator
ret mval = Do $ Ret { returnOperand = mval, metadata' = [] }

br :: AST.Name -> AST.Named AST.Terminator
br name = Do $ Br name []

condbr :: AST.Operand -> AST.Name -> AST.Name -> AST.Named AST.Terminator
condbr cond trueLabel falseLabel = Do $ CondBr cond trueLabel falseLabel []

trunc :: AST.Operand -> AST.Type -> CompilerType AST.Operand
trunc oper t = nameInstruction t $ AST.Trunc oper t []

zext :: AST.Operand -> AST.Type -> CompilerType AST.Operand
zext oper t = nameInstruction t $ AST.ZExt oper t []

bitcast :: AST.Operand -> AST.Type -> CompilerType AST.Operand
bitcast oper t = nameInstruction t $ AST.BitCast oper t []

compileModuleToLLVM :: AST.Module -> IO (Err String)
compileModuleToLLVM mod = withContext $ \context ->
  Except.runExceptT >=> either (return . Bad) (return . Ok) $
    withModuleFromAST context mod $ \m -> do
      compiled <- moduleLLVMAssembly m
      return compiled

compileCProgram :: String -> CProgramInfo -> Err AST.Module
compileCProgram pname prog = runCompilerType $ compileCProgram' pname prog

compileCProgram' :: String -> CProgramInfo -> CompilerType AST.Module
compileCProgram' pname (CProgram topdefs, cenv, ftenv) = do
  funenv <- fmap Map.fromList $ mapM (\(ident, tinfo) -> do
    funinfo <- typeInfoToFunInfo tinfo
    return (ident, funinfo)) $ Map.toList ftenv
  modify $ \s -> s { glFunTypeEnv = Map.union funenv additionalCDeclsEnv, classEnv = cenv }
  decls <- funDeclForBuiltIns
  defs <- mapM compileCTopDef topdefs
  strConstDefs <- getStrConstDefs
  return $ newModule pname (strConstDefs ++ decls ++ additionalCDecls ++ defs)

compileCTopDef :: CTopDef -> CompilerType AST.Definition
compileCTopDef x = case x of
  CTDFnDef tret ident args block -> do
    resetCompilerState
    newCurrentBlock
    tret' <- compileType tret
    let fname = Name ident
    params <- mapM (\(CArg atype aid) -> do
      atype' <- compileType atype
      let aname = Name aid
      return $ Parameter atype' aname []) args
    mapM_ (\(Parameter atype aname _) -> allocAndStore atype aname (LocalReference atype aname)) params
    compileCBlock block
    bblocks <- getBasicBlocks
    return $ newGlobalFunctionDef tret' fname params bblocks
  CTDCDef (CCDef id (CCBody decls)) -> do
    elemTypes <- mapM (\decl -> case decl of
                            CCVar t pidents -> do
                              t' <- compileType t
                              return $ replicate (length pidents) t') decls
    let elemTypes' = concat elemTypes
    return $ TypeDefinition (className id) (Just $ StructureType False elemTypes')

compileCBlock :: CBlock -> CompilerType ()
compileCBlock (CBlock stmts) = do
  oldEnv <- getEnv
  mapM_ (\stmt -> unlessM (isCurrentBlockTerminated) (compileCStmt stmt)) stmts
  putEnv oldEnv

compileCStmt :: CStmt -> CompilerType ()
compileCStmt x = case x of
  CSEmpty -> return ()
  CSBlock block -> compileCBlock block
  CSExp expr -> compileCExpr expr >> return ()
  CSRet expr -> do
    val <- compileCExpr expr
    setCurrentBlockTerminator $ ret $ Just val
  CSVRet -> setCurrentBlockTerminator $ ret Nothing
  CSDecl decl -> compileCDecl decl
  CSAss (ref, tref) expr -> do
    oper <- compileCRef ref tref
    val <- compileCExpr expr
    store oper val
  CSCondElse expr stmt1 stmt2 -> do
    val <- compileCExpr expr
    val' <- trunc val i1
    afterCondLabel <- getCurrentBlock
    trueLabel <- newCurrentBlock
    compileCStmt stmt1
    afterTrue <- getCurrentBlock
    falseLabel <- newCurrentBlock
    compileCStmt stmt2
    afterFalse <- getCurrentBlock
    setUnBlockTerminator afterCondLabel $ condbr val' trueLabel falseLabel
    termAfterTrue <- isBlockTerminated afterTrue
    termAfterFalse <- isBlockTerminated afterFalse
    Cond.when (not $ termAfterTrue && termAfterFalse) (do
      afterIfLabel <- newCurrentBlock
      setUnBlockTerminator afterTrue $ br afterIfLabel
      setUnBlockTerminator afterFalse $ br afterIfLabel
      )
    return ()
  CSWhile expr stmt -> do
    condLabel <- newBlock
    setCurrentBlockTerminator $ br condLabel
    setCurrentBlock condLabel
    val <- compileCExpr expr
    val' <- trunc val i1
    whileBodyLabel <- newCurrentBlock
    compileCStmt stmt
    afterWhileBodyLabel <- getCurrentBlock
    afterWhileLabel <- newCurrentBlock
    setUnBlockTerminator afterWhileBodyLabel $ br condLabel
    setUnBlockTerminator condLabel $ condbr val' whileBodyLabel afterWhileLabel
  CSRepeat stmt -> do
    repeatBodyLabel <- newBlock
    setCurrentBlockTerminator $ br repeatBodyLabel
    setCurrentBlock repeatBodyLabel
    compileCStmt stmt
    afterRepeatBodyLabel <- getCurrentBlock
    setUnBlockTerminator afterRepeatBodyLabel $ br repeatBodyLabel

compileCDecl :: CDecl -> CompilerType ()
compileCDecl (CDecl t items) = do
  t' <- compileType t
  mapM_ (\item -> case item of
    CINoInit ident   -> do
      val <- defaultValue t
      allocAndStore t' (Name ident) val
    CIInit ident exp -> do
      val <- compileCExpr exp
      allocAndStore t' (Name ident) val
        ) items

compileCExpr :: CTExpr -> CompilerType AST.Operand
compileCExpr (x, tx) = case x of
  CERef ref -> do
    oper <- compileCRef ref tx
    load oper
  CELit lit -> case lit of
    CLInt val -> return $ intConstOper val
    CLBool val -> return $ getBoolConstOper val
    CLString str -> refToStringConst str
  CEApp ident args -> do
    compiledArgs <- mapM compileCExpr args
    call ident compiledArgs
  CBinOp expr1 opr expr2 -> compileCBinOpr expr1 expr2 opr tx
  CENew (CNClass ident) -> do
    cbytes <- classBytes ident
    oper <- call mallocIdent ([intConstOper cbytes])
    let ctype = classType ident
    ctype' <- compileType ctype
    caddr <- bitcast oper ctype'
    initClass ident caddr
    return caddr
  CENull t -> do
    ct <- compileType t
    return $ ConstantOperand $ Null ct

initClass :: String -> AST.Operand -> CompilerType ()
initClass ident address = do
  (fields, _) <- getClassInfo ident
  Traversable.mapM (\(t, i) -> do
    field <- getField address t i
    val <- defaultValue t
    store field val) fields
  return ()

getField :: AST.Operand -> ABS.Type -> Integer -> CompilerType AST.Operand
getField address t fieldIndex = do
  ct <- compileType t
  getElementPtr (PointerType ct defaultAddrSpace) address [0, fieldIndex]

compileCRef :: CRef -> ABS.Type -> CompilerType AST.Operand
compileCRef x tx = case x of
  CRVar ident -> lookupEnv (Name ident)
  CRDot varid fieldIndex -> do
    oper <- loadVar varid
    getField oper tx fieldIndex

binOprs :: Map.Map String (Operand -> Operand -> InstructionMetadata -> Instruction)
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

i1RetOprs = ["<", "<=", ">", ">=", "==", "!=", "||", "&&", "^"]

compileCBinOpr :: CTExpr -> CTExpr -> String -> ABS.Type -> CompilerType AST.Operand
compileCBinOpr expr1 expr2 binOp retT = do
  ref <- compileCBinOpr' expr1 expr2 binOp retT
  if elem binOp i1RetOprs
    then zext ref boolType
    else return ref

compileCBinOpr' :: CTExpr -> CTExpr -> String -> ABS.Type -> CompilerType AST.Operand
compileCBinOpr' expr1 expr2 binOp retT = do
  cretT <- compileType retT
  let binOpF = (binOprs Map.! binOp)
  oper1 <- compileCExpr expr1
  case () of
    _ | elem binOp logOps -> do
        oper1' <- trunc oper1 i1
        sol <- alloc cretT
        store sol oper1
        afterFirstExp <- getCurrentBlock
        continueCompLabel <- newCurrentBlock
        oper2 <- compileCExpr expr2
        store sol oper2
        afterContinueCompLabel <- getCurrentBlock
        afterBinOpLabel <- newCurrentBlock
        if binOp == logAndOp
          then setUnBlockTerminator afterFirstExp $ condbr oper1' continueCompLabel afterBinOpLabel
          else setUnBlockTerminator afterFirstExp $ condbr oper1' afterBinOpLabel continueCompLabel
        setUnBlockTerminator afterContinueCompLabel (br afterBinOpLabel)
        load sol
      | True -> do
        oper2 <- compileCExpr expr2
        nameInstruction cretT $ binOpF oper1 oper2 []
