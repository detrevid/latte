{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Frontend.TypeChecker where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.BNFC.PrintLatte
import Latte.Internal.BuiltIn
import Latte.Internal.Type

import Control.Applicative (Applicative)
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map as Map


internalErrMsg :: String
internalErrMsg = "Internal error during type checking phase"

type CheckerState = (TypeEnv, Int)

topLevelDepth = 0

newtype CheckerType a = CheckerType (StateT CheckerState Err a)
  deriving (Functor, Applicative, Monad)

runCheckerType :: CheckerType a -> Err a
runCheckerType (CheckerType x) = evalStateT x (emptyEnv, topLevelDepth)

putEnv' :: TypeEnv -> CheckerState -> CheckerState
putEnv' env (_, depth) =  (env, depth)

putEnv :: TypeEnv -> CheckerType ()
putEnv env = CheckerType $ modify $ putEnv' env

getEnv :: CheckerType TypeEnv
getEnv = CheckerType $ gets fst

addToEnv' :: String -> TypeInfo -> CheckerState -> CheckerState
addToEnv' id t (env, depth) =
  (env', depth)
 where env' = Map.insert id t env

addToEnv :: String -> TypeInfo -> CheckerType ()
addToEnv id t = CheckerType $ modify $ addToEnv' id t

removeFromEnv' :: String -> CheckerState -> CheckerState
removeFromEnv' id (env, depth) =
  (env', depth)
 where env' = Map.delete id env

removeFromEnv :: String ->  CheckerType ()
removeFromEnv id = CheckerType $ modify $ removeFromEnv' id

lookupTypeEnv' :: Monad m => TypeEnv -> String -> m (Maybe TypeInfo)
lookupTypeEnv' env id = return $ Map.lookup id env

lookupTypeEnv :: String -> CheckerType (Maybe TypeInfo)
lookupTypeEnv id = do
  env <- getEnv
  lookupTypeEnv' env id

getDepth :: CheckerType Int
getDepth = CheckerType $ gets snd

changeDepth' :: (Int -> Int) -> CheckerState -> CheckerState
changeDepth' f (env, depth) = (env, f depth)

incDepth :: CheckerType ()
incDepth = CheckerType $ modify $ changeDepth' (1+)

decDepth :: CheckerType ()
decDepth = CheckerType $ modify $ changeDepth' (1-)

--TODO - use it everywhere
--TODO - maybe add expression that has the bad type
typeError :: Position -> String -> Type -> Type -> String
typeError pos errMsg texpected tfound =
  "Type Error\n" ++
  show pos ++ "\n" ++
  errMsg ++ "\n" ++
  "Type expected: " ++ printTree texpected ++ "\n" ++
  "Type found: " ++ printTree tfound ++ "\n"

undecError :: Position -> VarId -> String
undecError pos id =
   show pos ++ "\n" ++ show id ++ " has not been declared\n"

redecError :: Position -> VarId -> Position -> String
redecError pos id decPos =
   show pos ++ "\n" ++ show id ++ " has been already declared in this block at: " ++ show decPos ++ "\n"

checkTypes' :: Program -> CheckerType ()
checkTypes' prog@(Program topdefs) = do
  putEnv $ addBuiltInsToTypeEnv emptyEnv
  addTopDefsToEnv prog
  mapM_ checkTypesTopDef topdefs

checkTypes :: Program -> Err ()
checkTypes prog =  runCheckerType $ checkTypes' prog

addTopDefsToEnv :: Program -> CheckerType ()
addTopDefsToEnv (Program tdefs) =
  mapM_ (\(TDFnDef tret pid@(PIdent (pos, id)) args block) -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing         -> do
        let targs = map (\(Arg t _) -> t) args
        addToEnv id ((TFun tret targs), pos, topLevelDepth)
      Just (t, decPos, depthDec)  -> fail $ redecError pos id decPos
      ) tdefs

checkTypesTopDef :: TopDef -> CheckerType ()
checkTypesTopDef tdef = case tdef of
  TDFnDef tret pid@(PIdent (pos, id)) args block -> do
    oldEnv <- getEnv
    incDepth
    depth <- getDepth
    mapM_ (\(Arg t (PIdent (pos, id))) -> addToEnv id (t, pos, depth)) args
    tblock <- checkTypesBlock block tret
    when (tblock /= tret) (fail $ typeError pos ("Function " ++ id ++
      " not always returns expected type.") tret tblock)
    decDepth
    putEnv oldEnv

checkTypesBlock :: Block -> Type -> CheckerType Type
checkTypesBlock (Block stmts) exRetType = do
  incDepth
  tstmts <- mapM ((flip checkTypesStmt) exRetType) stmts
  decDepth
  if any ((==) exRetType) tstmts
    then return exRetType
    else return typeVoid

checkIfRedeclared :: PIdent -> CheckerType ()
checkIfRedeclared (PIdent (pos, id)) = do
  depth <- getDepth
  mt <- lookupTypeEnv id
  case mt of
    Nothing         -> return ()
    Just (t, decPos, depthDec)  ->
      if depth == depthDec
        then fail $ redecError pos id decPos
        else return ()

checkTypesStmt :: Stmt -> Type -> CheckerType Type
checkTypesStmt x exRetType = case x of
  SEmpty -> return typeVoid
  SBlock block -> do
    odlEnv <- getEnv
    t <- checkTypesBlock block exRetType
    putEnv odlEnv
    return t
  SDecl t items -> do
    depth <- getDepth
    mapM (\item -> case item of
      INoInit pid@(PIdent (pos, id))   -> do
        checkIfRedeclared pid
        addToEnv id (t, pos, depth)
      IInit pid@(PIdent (pos, id)) exp -> do
        checkIfRedeclared pid
        texp <- checkTypesExpr exp
        when (texp /= t) $ fail $ typeError pos ("Initialization of variable " ++
          show id ++ ".\n" ++ "Expression: " ++ printTree exp) t texp
        addToEnv id (t, pos, depth)
          ) items
    return typeVoid
  SAss pid@(PIdent (pos, id)) expr -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing      -> fail $ undecError pos id
      Just (t, _, _)  -> do
        texpr <- checkTypesExpr expr
        when (texpr /= t) (fail $ typeError pos "Bad expression type after assignment sign." t texpr)
        return typeVoid
  SDIncr pid@(PIdent (pos, id)) _ -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing      -> fail $ undecError pos id
      Just (t, _, _)  ->
        if t /= typeInt
          then fail $ typeError pos ("Variable " ++ show id) typeInt  t
          else return typeVoid
  SRet (TRet (pos, _)) expr -> do
    texpr <- checkTypesExpr expr
    when (texpr /= exRetType) (fail $ typeError pos "Bad return type." exRetType texpr)
    return texpr
  SVRet (TRet (pos, _)) -> do
    when (typeVoid  /= exRetType) (fail $ typeError pos "Bad return type." exRetType typeVoid)
    return typeVoid
  SCond (TIf (pos, _)) expr stmt -> do
    texpr <- checkTypesExpr expr
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in if condition." typeBool texpr)
    checkTypesStmt stmt exRetType
    return typeVoid
  SCondElse (TIf (pos, _)) expr stmt1 stmt2 -> do
    texpr <- checkTypesExpr expr
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in if condition." typeBool texpr)
    tretif <- checkTypesStmt stmt1 exRetType
    tretel <- checkTypesStmt stmt2 exRetType
    if (tretif == typeVoid || tretel == typeVoid)
      then return typeVoid
      else return exRetType
  SWhile (TWhile (pos, _)) expr stmt -> do
    texpr <- checkTypesExpr expr
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in while condition." typeBool texpr)
    checkTypesStmt stmt exRetType
    return typeVoid
  SExp expr -> do
    checkTypesExpr expr
    return typeVoid

checkTypesExpr :: Expr -> CheckerType Type
checkTypesExpr exp = case exp of
  EVar pid@(PIdent (pos, id)) -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing      -> fail $ undecError pos id
      Just (t, _, _)  -> return t
  ELit lit -> case lit of
    LInt _    -> return typeInt
    LTrue     -> return typeBool
    LFalse    -> return typeBool
    LString _ -> return typeString
  EApp pid@(PIdent (pos, id)) exps -> do
    mt <- lookupTypeEnv id
    texps <- mapM (checkTypesExpr) exps
    case mt of
      Nothing      -> fail $ undecError pos id
      Just (t, decPos, _)  -> case t of
        TFun treturn targs -> do
          when (length targs /= length texps) (fail $ "Type Error\n" ++ show pos ++ "\nFunction " ++ show id ++
            " declared at " ++ show decPos ++ " used with bad number of arguments" ++ "\nNumber expected: " ++
            show (length targs) ++ "\nNumber found: " ++ show (length texps) ++ "\n")
          mapM_ (\(ta, te, i) -> do
            when (ta /= te) $ fail $ typeError pos ("Function " ++ show id ++ " declared at " ++ show decPos ++
              " argument no. " ++ show i) ta te
            return ()) (zip3 targs texps [1..])
          return treturn
        _ -> fail $ show pos ++ "\n" ++ id ++ " declared at " ++ show decPos ++ "is not a function\n" ++
               "\nType found: " ++ show t
  EUnOp opr exp -> do
    texp <- checkTypesExpr exp
    case opr of
      ONeg (TMinus (pos, opr)) ->
        if texp == typeInt
          then return typeInt
          else fail $ "Type Error\n" ++ show pos ++ "\nUnary operator " ++ show opr ++ " applied to" ++
            "non-integer expression.\n" ++ show exp ++ "\nType found:" ++ show texp
      ONot (TExclM (pos, opr))  ->
        if texp == typeBool
          then return typeBool
          else fail $ "Type Error\n" ++ show pos ++ "\nUnary operator " ++ show opr ++ " applied to" ++
            "non-boolean expression.\n" ++ show exp ++ "\nType found:" ++ show texp
        --TODO wydziel kod tych metod
  EMul expr1 (TMulOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeInt typeInt
  EAdd expr1 addop expr2 -> case addop of
    OPlus (TPlus info) -> do
      texpr1 <- checkTypesExpr expr1
      if texpr1 == typeInt
        then checkTypesBinOp info expr1 expr2 typeInt typeInt
        else checkTypesBinOp info expr1 expr2 typeString typeString
    OMinus (TMinus info) -> checkTypesBinOp info expr1 expr2 typeInt typeInt
  ERel expr1 (TRelOp info@(pos, opr)) expr2 ->
    if opr == eqOp
      then do
         texpr1 <- checkTypesExpr expr1
         texpr2 <- checkTypesExpr expr2
         when (texpr1 /= texpr2) $ fail $ "Type Error\n" ++ show pos ++ "\nBinary operator " ++ show opr ++
                " applied to expression.\n" ++ show expr2 ++ "Type expected: " ++ show texpr1 ++ "\nType found: " ++
                show texpr2
         return typeBool
      else checkTypesBinOp info expr1 expr2 typeInt typeBool
  EAnd expr1 (TLogAndOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeBool typeBool
  EOr expr1 (TLogOrOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeBool typeBool


checkTypesBinOp :: ((Int, Int), String) -> Expr -> Expr -> Type -> Type -> CheckerType Type
checkTypesBinOp (pos, opr) expl expr eType rtype = do
  texpl <- checkTypesExpr expl
  when (texpl /= eType) $ fail $ "Type Error\n" ++ show pos ++ "\nBinary operator " ++ show opr ++
     " applied to expression.\n" ++ show expl ++ "\nType expected: " ++ show eType ++ "\nType found: " ++ show texpl
  texpr <- checkTypesExpr expr
  when (texpr /= eType) $ fail $ "Type Error\n" ++ show pos ++ "\nBinary operator " ++ show opr ++
       " applied to expression.\n" ++ show expr ++ "Type expected: " ++ show eType ++ "\nType found: " ++ show texpr
  return rtype
