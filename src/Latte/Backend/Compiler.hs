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
import qualified LLVM.General.AST.CallingConvention as CallConv
import LLVM.General.AST.Constant
import qualified LLVM.General.AST.Constant as Constant
import LLVM.General.AST.Global
import qualified  LLVM.General.AST.Global as AST.Global
import LLVM.General.AST.Linkage
import qualified LLVM.General.AST.IntegerPredicate as IntPred
import LLVM.General.AST.Type
import LLVM.General.Context
import LLVM.General.Module

import Control.Conditional (unlessM)
import Control.Lens.Tuple
import Control.Lens ((^.))
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import qualified Data.Int as HInt
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
  funEnv       :: FunEnv,
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
  funEnv       = emptyEnv,
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
  funEnv       = funEnv s,
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
  maybe (fail internalErrMsg) return (Map.lookup ident env)

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
  tenv <- gets funEnv
  case (Map.lookup ident tenv) of
    Just finf -> return $ compileType $ functionReturnType finf
    _ -> fail $ internalErrMsg

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
  let rType' = compileType rType
      name' = Name name
      params = map (\(at, aname) -> Parameter (compileType at) (Name aname) []) args
  return $ newFunDecl rType' name' params

funDeclForBuiltIns :: CompilerType [AST.Definition]
funDeclForBuiltIns = mapM (\(t, n, a) -> funDeclForBuiltIns' t n a) builtInsDescs

boolBits = 8
boolType = i8
charType = i8
charBits = 8
intType = i32
intBits = 32
maxInt = (toInteger (maxBound :: HInt.Int32))
stringType = ptr charType
pointerBits = 64

className :: String -> Name
className id = Name $ "class." ++ id

constStringType :: Int -> AST.Type
constStringType size = ArrayType (fromIntegral size) charType

compileType :: ABS.Type -> AST.Type
compileType (TType x) = case x of
  TBuiltIn BIInt -> intType
  TBuiltIn BIVoid -> VoidType
  TBuiltIn BIBool -> boolType
  TBuiltIn BIStr -> stringType
  TClass (CType (Ident id)) -> ptr $ classTypeRef id
  TClass (CPType (PIdent(_, id))) -> ptr $ classTypeRef id
compileType (TFun tret targs) = FunctionType (compileType tret) (map compileType targs) False

classTypeRef :: String -> AST.Type
classTypeRef classId = NamedTypeReference $ className classId

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
  let fieldsTypes = map (^._1) $ Map.elems $ classFields cinfo
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

defaultValue :: ABS.Type -> CompilerType AST.Operand
defaultValue (TType x) = case x of
  TBuiltIn BIInt  -> return defaultIntVal
  TBuiltIn BIVoid -> fail internalErrMsg
  TBuiltIn BIBool -> return defaultBoolVal
  TBuiltIn BIStr  -> refToStringConst ""
  TClass _        -> return $ ConstantOperand $ Null $ compileType (TType x)
defaultValue _ = fail internalErrMsg

globalStringConst :: AST.Name -> String -> AST.Definition
globalStringConst name s =
  GlobalDefinition $ globalVariableDefaults {
    name             = name,
    linkage          = Private,
    hasUnnamedAddr   = True,
    isConstant       = True,
    AST.Global.type' = constStringType $ length s,
    initializer      = Just $ Array charType (map getCharConst s)
    }

objectClassDef = TypeDefinition (className objectClassId) (Just $ StructureType False [ptrToVTableType])

compileFunInfoType :: FunctionInfo -> Constant
compileFunInfoType finf =
  GlobalReference (compileType $ TFun (functionReturnType finf) (functionArgsTypes finf)) (Name $ functionName finf)

funInfoToVTableRec :: FunctionInfo -> Constant
funInfoToVTableRec finf = Constant.BitCast (compileFunInfoType finf) (ptr i8)

ptrToVTableType :: AST.Type
ptrToVTableType = ptr $ ptr $ FunctionType i32 [] True

vtableType :: Int -> AST.Type
vtableType size = ArrayType (fromIntegral size) (ptr i8)

vtableConst :: AST.Name -> [FunctionInfo] -> AST.Definition
vtableConst name funInfos =
  GlobalDefinition $ globalVariableDefaults {
    name             = name,
    linkage          = Private,
    hasUnnamedAddr   = True,
    isConstant       = True,
    AST.Global.type' = vtableType $ length funInfos,
    initializer      = Just $ Array (ptr i8) (map funInfoToVTableRec funInfos)
    }

classIdToVTableName :: String -> AST.Name
classIdToVTableName classId = Name $ "_VT_" ++ classId

classVTable :: String -> CompilerType AST.Operand
classVTable classId = do
  cinfo <- getClassInfo classId
  let vttype = vtableType $ Map.size $ classMethods cinfo
      oper = constGlobalReference vttype (classIdToVTableName classId)
  bitcast oper ptrToVTableType

virtualTables :: CompilerType [AST.Definition]
virtualTables = do
  cenv <- gets classEnv
  return $ map (\(classId, cinf) ->
    vtableConst (classIdToVTableName classId) (Map.elems $ classMethods cinf)) $ Map.toList cenv

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
alloc t = nameInstruction (ptr t) $ Alloca t Nothing 0 []

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

callOperand :: AST.Type -> AST.Operand -> [AST.Operand] ->  CompilerType AST.Operand
callOperand tret fun args =
 nameInstruction (pointerReferent tret) $ Call Nothing CallConv.C [] (Right $ fun) (map (\arg -> (arg, [])) args) [] []

call :: String -> [AST.Operand] -> CompilerType AST.Operand
call name args = do
  retType <- getFunRetType name
  callOperand retType (constGlobalReference retType (Name name)) args

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

bitcastClass :: AST.Operand -> String -> CompilerType AST.Operand
bitcastClass address tclassId = bitcast address $ compileType $ classType tclassId

compileModuleToLLVM :: AST.Module -> IO (Err String)
compileModuleToLLVM mod = withContext $ \context ->
  Except.runExceptT >=> either (return . Bad) (return . Ok) $
    withModuleFromAST context mod $ \m -> do
      compiled <- moduleLLVMAssembly m
      return compiled

compileCProgram :: String -> CProgramInfo -> Err AST.Module
compileCProgram pname prog = runCompilerType $ compileCProgram' pname prog

compileCProgram' :: String -> CProgramInfo -> CompilerType AST.Module
compileCProgram' pname (CProgram topdefs, cenv, fenv) = do
  modify $ \s -> s { funEnv = fenv, classEnv = cenv }
  vtables <- virtualTables
  decls <- funDeclForBuiltIns
  defs <- mapM compileCTopDef topdefs
  strConstDefs <- getStrConstDefs
  return $ newModule pname (strConstDefs ++ vtables ++ decls ++ [objectClassDef] ++ defs)

compileCFunDef :: CFunDef -> CompilerType AST.Definition
compileCFunDef (CFunDef tret ident args block) = do
  resetCompilerState
  newCurrentBlock
  let tret' = compileType tret
      fname = Name ident
      params    = map (\(CArg atype aid) -> Parameter (compileType atype) (Name aid) []) args
  mapM_ (\(Parameter atype aname _) -> allocAndStore atype aname (LocalReference atype aname)) params
  compileCBlock block
  bblocks <- getBasicBlocks
  return $ newGlobalFunctionDef tret' fname params bblocks

compileCTopDef :: CTopDef -> CompilerType AST.Definition
compileCTopDef x = case x of
  CTDFnDef fdef -> compileCFunDef fdef
  CTDCDef (CCDef classId superClass (CCBody decls)) -> do
    let elemTypes = concatMap (\(CCVar t pidents) -> replicate (length pidents) (compileType t)) decls
        scref = maybe [] (\x -> [classTypeRef x]) superClass
        elemTypes' = scref ++ elemTypes
    return $ TypeDefinition (className classId) (Just $ StructureType False elemTypes')

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
  CSRet rtype expr -> setCurrentBlockTerminator . ret . Just =<< compileAndCastCExpr rtype expr
  CSVRet -> setCurrentBlockTerminator $ ret Nothing
  CSDecl decl -> compileCDecl decl
  CSAss (ref, tref) expr -> do
    oper <- compileCRef ref tref
    val <- compileAndCastCExpr tref expr
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
    when (not $ termAfterTrue && termAfterFalse) (do
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
  let t' = compileType t
  mapM_ (\item -> case item of
    CINoInit ident   -> do
      val <- defaultValue t
      allocAndStore t' (Name ident) val
    CIInit ident exp -> do
      val <- compileAndCastCExpr t exp
      allocAndStore t' (Name ident) val
        ) items

compileAndCastCExpr :: ABS.Type -> CTExpr -> CompilerType AST.Operand
compileAndCastCExpr t expr = do
  oper <- compileCExpr expr
  bitcast oper $ compileType t

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
    oper <- call (functionName mallocFI) ([intConstOper cbytes])
    let ctype = compileType $ classType ident
    caddr <- bitcast oper ctype
    initClass ident caddr
    return caddr
  CENull t -> return $ ConstantOperand $ Null $ compileType t
  CEMet varId metId args -> do
    compiledArgs <- mapM compileCExpr args
    varAddress <- loadVar varId
    let targs = map compileType $ map snd args
        tret = compileType tx
        ftype = FunctionType tret targs False
        ftypeptr = ptr $ ptr $ ftype
    vtableAddr <- bitcast varAddress $ ptr ftypeptr
    vtable <- load vtableAddr
    metAddr <- getElementPtr ftypeptr vtable [(fromIntegral metId)]
    met <- load metAddr
    callOperand tret met compiledArgs

initClass :: String -> AST.Operand -> CompilerType ()
initClass classId address = do
  cinfo <- getClassInfo classId
  vtptr <- bitcast address $ ptr ptrToVTableType
  cvt <- classVTable classId
  store vtptr cvt
  mapM_ (\(t, i, sc) -> do
    oper <- bitcastClass address sc
    field <- getField oper t i
    val <- defaultValue t
    store field val) $ Map.elems $ classFields cinfo

getField :: AST.Operand -> ABS.Type -> Integer -> CompilerType AST.Operand
getField address t fieldIndex = getElementPtr (ptr $ compileType t) address [0, fieldIndex]

compileCRef :: CRef -> ABS.Type -> CompilerType AST.Operand
compileCRef x tx = case x of
  CRVar varId -> lookupEnv (Name varId)
  CRDot varId fieldIndex fieldClass -> do
    oper <- loadVar varId
    oper' <- bitcastClass oper fieldClass
    getField oper' tx fieldIndex

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
  let cretT = compileType retT
      binOpF = (binOprs Map.! binOp)
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
