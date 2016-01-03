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

type CheckerState = TypeEnv

newtype CheckerType a = CheckerType (StateT CheckerState Err a)
  deriving (Functor, Applicative, Monad)

runCheckerType :: CheckerType a -> Err a
runCheckerType (CheckerType x) = evalStateT x emptyEnv

putEnv' :: TypeEnv -> CheckerState -> ((), CheckerState)
putEnv' env _ =  ((), env)

putEnv :: TypeEnv -> CheckerType ()
putEnv env = CheckerType $ state $ putEnv' env

getEnv' :: CheckerState -> (TypeEnv, CheckerState)
getEnv' env = (env, env)

getEnv :: CheckerType TypeEnv
getEnv = CheckerType $ state getEnv'

addToEnv' :: String -> TypeInfo -> CheckerState -> ((), CheckerState)
addToEnv' id t env =
  ((), env')
 where env' = Map.insert id t env

addToEnv :: String -> TypeInfo -> CheckerType ()
addToEnv id t = CheckerType $ state $ addToEnv' id t

removeFromEnv' :: String -> CheckerState -> ((), CheckerState)
removeFromEnv' id env =
  ((), env')
 where env' = Map.delete id env

removeFromEnv :: String ->  CheckerType ()
removeFromEnv id = CheckerType $ state $ removeFromEnv' id

lookupTypeEnv' :: Monad m => TypeEnv -> String -> m (Maybe TypeInfo)
lookupTypeEnv' env id = return $ Map.lookup id env

lookupTypeEnv :: String -> CheckerType (Maybe TypeInfo)
lookupTypeEnv id = do
  env <- getEnv
  lookupTypeEnv' env id

--class Typeable a where
--  checkTypes :: CheckerType a

--TODO - use it everywhere
--TODO - maybe add expressionthat has the bad type
typeError :: Position -> String -> Type -> Type -> String
typeError pos errMsg texpected tfound =
  "Type Error\n" ++
  show pos ++ "\n" ++
  errMsg ++ "\n" ++
  "Type expected: " ++ printTree texpected ++ "\n" ++
  "Type found: " ++ printTree tfound ++ "\n"

--checkType :: Position -> String -> Type -> Type -> CheckerType ()
--checkType pos errMsg texpected tfound = do
--  when (tfound  /= texpected) (fail $ typeError pos "Bad return type." texpected tfound)


undecVarError :: Position -> VarId -> String
undecVarError pos id =
   show pos ++ "\nVariable " ++ show id ++ " has not been declared"

--checkTypesProgram :: Program -> CheckerType ()
--checkTypesProgram prog =

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
    let targs = map (\(Arg t _) -> t) args
    addToEnv id ((TFun tret targs), pos)) tdefs

checkTypesTopDef :: TopDef -> CheckerType ()
checkTypesTopDef tdef = case tdef of
  TDFnDef tret pid@(PIdent (pos, id)) args block -> do
    oldEnv <- getEnv
    mapM_ (\(Arg t (PIdent (pos, id))) -> addToEnv id (t, pos)) args
    tblock <- checkTypesBlock block tret
    when (tblock /= tret) (fail $ typeError pos ("Function " ++ id ++
      " not always returns expected type.") tret tblock)
    putEnv oldEnv

checkTypesBlock :: Block -> Type -> CheckerType Type
checkTypesBlock (Block stmts) exRetType = do
  tstmts <- mapM ((flip checkTypesStmt) exRetType) stmts
  if any ((==) exRetType) tstmts
    then return exRetType
    else return typeVoid

checkTypesStmt :: Stmt -> Type -> CheckerType Type
checkTypesStmt x exRetType = case x of
  SEmpty -> return typeVoid
  SBlock block -> do
    odlEnv <- getEnv
    t <- checkTypesBlock block exRetType
    putEnv odlEnv
    return t
  SDecl t items -> do
    mapM (\item -> case item of
      INoInit (PIdent (pos, id))   -> addToEnv id (t, pos)
      IInit (PIdent (pos, id)) exp -> do
        texp <- checkTypesExpr exp
        when (texp /= t) $ fail $ typeError pos ("Initialization of variable " ++
          show id ++ ".\n" ++ "Expression: " ++ printTree exp) t texp
        addToEnv id (t, pos)
          ) items
    return typeVoid
--   SAss pid@(PIdent (_, id)) (TAss (pos, _)) expr -> do
--     mt <- lookupTypeEnv id
--     case mt of
--       Nothing      -> fail $ undecVarError pos id
--       Just (t, _)  -> do
--         texpr <- checkTypesExpr expr
--         when (texpr /= t) (fail $ typeError pos "Bad expression type after assignment sign." t texpr)
--         return typeVoid
  SAss pid@(PIdent (pos, id)) expr -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing      -> fail $ undecVarError pos id
      Just (t, _)  -> do
        texpr <- checkTypesExpr expr
        when (texpr /= t) (fail $ typeError pos "Bad expression type after assignment sign." t texpr)
        return typeVoid
  SDIncr pid@(PIdent (pos, id)) _ -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing      -> fail $ undecVarError pos id
      Just (t, _)  ->
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
      Nothing      -> fail $ undecVarError pos id
      Just (t, _)  -> return t
  ELit lit -> case lit of
    LInt _    -> return typeInt
    LTrue     -> return typeBool
    LFalse    -> return typeBool
    LString _ -> return typeString
  EApp pid@(PIdent (pos, id)) exps -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing      -> fail $ undecVarError pos id
      Just (t, decPos)  -> case t of
        TFun treturn targs -> do
          texps <- mapM (checkTypesExpr) exps
          when (length targs /= length texps) (fail $ "Type Error\n" ++ show pos ++ "\nFunction " ++ show id ++
            " declared at " ++ show decPos ++ " used with bad number of arguments" ++ "\nNumber expected: " ++
            show (length targs) ++ "\nNumber found: " ++ show (length texps) ++ "\n")
          mapM_ (\(ta, te, i) -> do
            when (ta /= te) $ fail $ "Type Error\n" ++ show pos ++ "\nFunction " ++ show id ++
              " declared at " ++ show decPos ++ " argument no. " ++ show i ++ "\nType expected: " ++
              show ta ++ "\nType found: " ++ show te ++ "\n"
            return ()) (zip3 targs texps [1..])
          return treturn
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
  --EAdd expr1 (TAddOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeInt
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
  --ERel expr1 relop expr2 -> case relop of
  --  UnOp (TMinus info) -> checkTypesBinOp info expr1 expr2 typeBool
  --  UnOp (TExclM info) -> checkTypesBinOp info expr1 expr2 typeBool
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
