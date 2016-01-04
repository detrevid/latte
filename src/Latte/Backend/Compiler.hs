{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Backend.Compiler where

import Latte.BNFC.AbsLatte
import qualified Latte.BNFC.AbsLatte as ABS
import Latte.BNFC.ErrM

import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import LLVM.General.AST.Constant
import LLVM.General.Context
import LLVM.General.Module

import qualified Data.Map as Map
import Debug.Trace
import Control.Applicative (Applicative)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import qualified Control.Monad.Trans.State as StateT

internalErrMsg = "Internal error during llvm compiling phase"

type CompilerEnv = Map.Map String AST.Operand
type BlocksEnv = Map.Map AST.Name AST.BasicBlock

emptyEnv = Map.empty

data NumSupplier = NumSupplier [Int]

newNumSupplier :: NumSupplier
newNumSupplier = NumSupplier [ n | n <- [0..]]

data CompilerState = CompilerState {
  environment :: CompilerEnv,
  blocks :: BlocksEnv,
  supplier :: NumSupplier
  }

newtype CompilerType a = CompilerType (StateT CompilerState Err a)
  deriving (Functor, Applicative, Monad)

runCompilerType :: CompilerType a -> Err a
runCompilerType (CompilerType x) =
  evalStateT x (CompilerState { environment = emptyEnv, blocks = emptyEnv, supplier = newNumSupplier })

putEnv :: CompilerEnv -> CompilerType ()
putEnv env = CompilerType $ modify $ \s -> s { environment = env }

getEnv :: CompilerType CompilerEnv
getEnv = CompilerType $ gets environment

addBlock :: AST.BasicBlock -> CompilerType ()
addBlock bb@(AST.BasicBlock name _ _) =
  CompilerType $ modify $ \s -> s { blocks = Map.insert name bb $ blocks s }

getNewNr' :: CompilerState -> (Int, CompilerState)
getNewNr' s =
  (head num, s { supplier = NumSupplier $ tail num })
 where NumSupplier num = supplier s

getNewNr :: CompilerType Int
getNewNr = CompilerType $ state getNewNr'

getNewLabel :: CompilerType Name
getNewLabel = do
  newNr <- getNewNr
  return (Name $ "<label>" ++ show newNr)

getNewLocalName :: CompilerType Name
getNewLocalName = do
  newNr <- getNewNr
  return (Name $ show newNr)

newModule :: String -> [AST.Definition] -> AST.Module
newModule name defs = defaultModule { moduleName = name, moduleDefinitions = defs }

newGlobalFunctionDef :: AST.Type -> AST.Name -> [AST.Parameter] -> [AST.BasicBlock] -> AST.Definition
newGlobalFunctionDef tRet name param body =
  GlobalDefinition $ functionDefaults {
    returnType = tRet,
    name = name,
    parameters = (param, False),
    basicBlocks = body
    }

data BasicBlockInfo = BasicBlockInfo {
  blockName :: AST.Name,
  blockBody :: [AST.Named AST.Instruction],
  blockTerminator :: Maybe (AST.Named AST.Terminator)
  }

compileBasicBlockInfo :: BasicBlockInfo -> CompilerType AST.BasicBlock
compileBasicBlockInfo bbi =
  case (blockTerminator bbi) of
    Just terminator -> return $ BasicBlock (blockName bbi) (blockBody bbi) terminator
    Nothing -> fail internalErrMsg

addInstrs :: BasicBlockInfo -> [AST.Named AST.Instruction] -> BasicBlockInfo
addInstrs bb instrs = bb { blockBody = (blockBody bb) ++ instrs }

compileType :: ABS.Type -> CompilerType AST.Type
compileType x = case x of
  TType (TBuiltIn BIInt) -> return $ IntegerType 32
  --TODO add other builtins
  _ -> fail internalErrMsg

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

compileModuleToLLVM :: AST.Module -> IO String
compileModuleToLLVM mod = withContext $ \context ->
  liftError $ withModuleFromAST context mod $ \m -> do
    compiled <- moduleLLVMAssembly m
    return compiled

compileProgram' :: String -> Program -> CompilerType AST.Module
compileProgram' name prog@(Program topdefs) = do
  --TODO add topDeclaration for builtin functions
  defs <- mapM compileTopDef topdefs
  return $ newModule name defs

compileProgram :: String -> Program -> Err AST.Module
compileProgram name prog = runCompilerType $ compileProgram' name prog

compileTopDef :: TopDef -> CompilerType AST.Definition
compileTopDef x = case x of
  TDFnDef tret pid@(PIdent (pos, id)) args block -> do
    tret' <- compileType tret
    let name = Name id
    params <- mapM (\(Arg atype (PIdent (_, aid))) -> do
      atype' <- compileType atype
      let aname = Name aid
      return $ Parameter atype' aname []
      ) args
    label  <- getNewLabel
    let startBlock = BasicBlockInfo { blockName = label, blockBody = [], blockTerminator = Nothing }
    (bblockis, _) <- compileBlock startBlock block (Name "error")
    bblocks <- mapM compileBasicBlockInfo bblockis
    return $ newGlobalFunctionDef tret' name params bblocks

compileBlock :: BasicBlockInfo -> Block -> AST.Name -> CompilerType ([BasicBlockInfo], (Maybe BasicBlockInfo))
compileBlock bb (Block stmts) after = do
  oldEnv <- getEnv
  (bbsnew, bbnew) <- foldM (\(bbs, bbnow) -> \stmt -> do
     case bbnow of
       Nothing -> return (bbs, bbnow)
       Just now -> do
         (bbs', bbnow') <- compileStmt now after stmt
         return (bbs ++ bbs', bbnow')) ([], Just bb) stmts
  putEnv oldEnv
  return (bbsnew, bbnew)

compileStmt :: BasicBlockInfo -> AST.Name -> Stmt -> CompilerType ([BasicBlockInfo], (Maybe BasicBlockInfo))
compileStmt bb after x = case x of
  SEmpty               -> return ([], Just bb)
  SBlock block -> compileBlock bb block after
  --TODO does we really need Sexp to produce code?
  SExp expr            -> do
    (instrs, _) <- compileExpr expr
    return ([], Just $ addInstrs bb instrs)
  SRet tret expr       -> do
    (instrs, val) <- compileExpr expr
    let bb' = addInstrs bb instrs
        bb'' = bb' { blockTerminator = Just $ Do $ Ret { returnOperand = Just val, metadata' = []} }
    return ([bb''], Nothing)

compileExpr :: Expr -> CompilerType ([AST.Named AST.Instruction], AST.Operand)
compileExpr x = case x of
  ELit lit -> case lit of
    LInt val -> return ([], ConstantOperand $ Int { integerBits = 32, integerValue = val })
    --TODO other types
