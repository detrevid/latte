{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Frontend.TypeChecker (checkTypes, getGlobalDefsTypesEnv, getGlobalCDefsTypesEnv) where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.BNFC.PrintLatte
import Latte.Internal.BuiltIn
import Latte.Internal.Type
import Latte.Internal.ASTInternal

import Control.Applicative (Applicative)
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.List


internalErrMsg :: String
internalErrMsg = "Internal error during type checking phase"

type CheckerState = (TypeEnv, Int)

topLevelDepth = 0

newtype CheckerType a = CheckerType (StateT CheckerState Err a)
  deriving (Functor, Applicative, Monad)

runCheckerType :: CheckerType a -> Err a
runCheckerType (CheckerType x) = evalStateT x (emptyTypeEnv, topLevelDepth)

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

lookupTypeEnv :: String -> CheckerType (Maybe TypeInfo)
lookupTypeEnv id = do
  env <- getEnv
  return $ Map.lookup id env

getDepth :: CheckerType Int
getDepth = CheckerType $ gets snd

changeDepth' :: (Int -> Int) -> CheckerState -> CheckerState
changeDepth' f (env, depth) = (env, f depth)

incDepth :: CheckerType ()
incDepth = CheckerType $ modify $ changeDepth' (1+)

decDepth :: CheckerType ()
decDepth = CheckerType $ modify $ changeDepth' (1-)

--TODO - use it everywhere
typeError :: Position -> String -> Maybe Expr -> Type -> Type -> String
typeError pos errMsg mexpr texpected tfound  =
  "Type Error\n" ++
  show pos ++ "\n" ++
  errMsg ++ "\n" ++
  exprStr ++
  "Type expected: " ++ printTree texpected ++ "\n" ++
  "Type found: " ++ printTree tfound ++ "\n"
 where exprStr = maybe "" (\exp -> "Expression: " ++ printTree exp ++ "\n") mexpr

undecError :: Position -> VarId -> String
undecError pos id =
   show pos ++ "\n" ++ show id ++ " has not been declared\n"

redecError :: Position -> VarId -> Position -> String
redecError pos id decPos =
   show pos ++ "\n" ++ show id ++ " has been already declared in this block at: " ++ show decPos ++ "\n"

checkTypes' :: Program -> CheckerType CProgram
checkTypes' prog@(Program topdefs) = do
  putEnv $ addBuiltInsToTypeEnv emptyTypeEnv
  addTopDefsToEnv prog
  ctopdefs <- mapM checkTypesTopDef topdefs
  return $ CProgram ctopdefs

checkTypes :: Program -> Err CProgram
checkTypes prog = runCheckerType $ checkTypes' prog

getGlobalDefsTypesEnv' :: Program -> CheckerType TypeEnv
getGlobalDefsTypesEnv' prog = do
  putEnv $ addBuiltInsToTypeEnv emptyTypeEnv
  addTopDefsToEnv prog
  getEnv

getGlobalCDefsTypesEnv' :: CProgram -> CheckerType TypeEnv
getGlobalCDefsTypesEnv' prog = do
  putEnv $ addBuiltInsToTypeEnv emptyTypeEnv
  addTopCDefsToEnv prog
  getEnv


getGlobalDefsTypesEnv :: Program -> TypeEnv
getGlobalDefsTypesEnv prog =
  case (runCheckerType $ getGlobalDefsTypesEnv' prog) of
    Ok env -> env
    Bad m -> emptyTypeEnv

getGlobalCDefsTypesEnv :: CProgram -> TypeEnv
getGlobalCDefsTypesEnv prog =
  case (runCheckerType $ getGlobalCDefsTypesEnv' prog) of
    Ok env -> env
    Bad m -> emptyTypeEnv

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

addTopCDefsToEnv :: CProgram -> CheckerType ()
addTopCDefsToEnv (CProgram tdefs) =
  mapM_ (\(CTDFnDef tret id args block) -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing         -> do
        let targs = map (\(CArg t _) -> t) args
        addToEnv id ((TFun tret targs), (0, 0), topLevelDepth)
      Just _ -> fail $ internalErrMsg
      ) tdefs

checkTypesTopDef :: TopDef -> CheckerType CTopDef
checkTypesTopDef tdef = case tdef of
  TDFnDef tret pid@(PIdent (pos, id)) args block -> do
    oldEnv <- getEnv
    incDepth
    depth <- getDepth
    cargs <- mapM (\(Arg t (PIdent (pos, id))) -> do
      addToEnv id (t, pos, depth)
      return $ CArg t id) args
    (tblock, CBlock cblock) <- checkTypesBlock block tret
    when (tblock /= tret) (fail $ typeError pos ("Function " ++ id ++
      " not always returns expected type.") Nothing tret tblock)
    decDepth
    putEnv oldEnv
    if tret == typeVoid
      then return $ CTDFnDef tret id cargs (CBlock $ cblock ++ [CSVRet])
      else return $ CTDFnDef tret id cargs (CBlock cblock)

checkTypesBlock :: Block -> Type -> CheckerType (Type, CBlock)
checkTypesBlock (Block stmts) exRetType = do
  incDepth
  (tstmts, cstmts) <- fmap unzip $ mapM ((flip checkTypesStmt) exRetType) stmts
  decDepth
  if any ((==) exRetType) tstmts
    then return (exRetType, CBlock cstmts)
    else return (typeVoid, CBlock cstmts)

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

checkTypesStmt :: Stmt -> Type -> CheckerType (Type, CStmt)
checkTypesStmt x exRetType = case x of
  SEmpty -> return (typeVoid, CSEmpty)
  SBlock block -> do
    odlEnv <- getEnv
    (t, cblock) <- checkTypesBlock block exRetType
    putEnv odlEnv
    return (t, CSBlock cblock)
  SDecl t items -> do
    depth <- getDepth
    citems <- mapM (\item -> case item of
      INoInit pid@(PIdent (pos, id))   -> do
        checkIfRedeclared pid
        addToEnv id (t, pos, depth)
        return $ CINoInit id
      IInit pid@(PIdent (pos, id)) exp -> do
        checkIfRedeclared pid
        (texp, cexp) <- checkTypesExpr exp
        when (texp /= t) $ fail $ typeError pos ("Initialization of variable " ++ show id) (Just exp) t texp
        addToEnv id (t, pos, depth)
        return $ CIInit id cexp
          ) items
    return (typeVoid, CSDecl t citems)
  SAss pid@(PIdent (pos, id)) expr -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing      -> fail $ undecError pos id
      Just (t, _, _)  -> do
        (texpr, cexpr) <- checkTypesExpr expr
        when (texpr /= t) (fail $ typeError pos "Bad expression type after assignment sign." (Just expr) t texpr)
        return (typeVoid, CSAss id cexpr)
  SDIncr pid@(PIdent (pos, id)) (TDIOp (_, opr)) -> do
    mt <- lookupTypeEnv id
    let opr' = if opr == "++" then "+" else "-"
    case mt of
      Nothing      -> fail $ undecError pos id
      Just (t, _, _)  ->
        if t /= typeInt
          then fail $ typeError pos ("Variable " ++ show id) Nothing typeInt  t
          else return (typeVoid, (CSAss id ((CBinOp ((CEVar id), t) opr' ((CELit (LInt 1)), typeInt)), typeInt)))
  SRet (TRet (pos, _)) expr -> do
    (texpr, cexpr) <- checkTypesExpr expr
    when (texpr /= exRetType) (fail $ typeError pos "Bad return type." (Just expr) exRetType texpr)
    return (texpr, CSRet cexpr)
  SVRet (TRet (pos, _)) -> do
    when (typeVoid  /= exRetType) (fail $ typeError pos "Bad return type." Nothing exRetType typeVoid)
    return (typeVoid, CSVRet)
  SCond (TIf (pos, _)) expr stmt -> do
    (texpr, cexpr) <- checkTypesExpr expr
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in if condition." (Just expr) typeBool texpr)
    (tstmt, cstmt) <- checkTypesStmt stmt exRetType
    if isCTExprTrue cexpr
      then return (tstmt, cstmt)
      else return (typeVoid, CSCondElse cexpr cstmt CSEmpty)
  SCondElse (TIf (pos, _)) expr stmt1 stmt2 -> do
    (texpr, cexpr) <- checkTypesExpr expr
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in if condition." (Just expr) typeBool texpr)
    (tretif, cstmt1) <- checkTypesStmt stmt1 exRetType
    (tretel, cstmt2)<- checkTypesStmt stmt2 exRetType
    case (isCTExprTrue cexpr, isCTExprFalse cexpr) of
      (True, _) -> return (tretif, cstmt1)
      (_, True) -> return (tretel, cstmt2)
      (_, _)    ->
        if (tretif == typeVoid || tretel == typeVoid)
          then return (typeVoid, CSCondElse cexpr cstmt1 cstmt2)
          else return (exRetType, CSCondElse cexpr cstmt1 cstmt2)
  SWhile (TWhile (pos, _)) expr stmt -> do
    (texpr, cexpr) <- checkTypesExpr expr
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in while condition." (Just expr) typeBool texpr)
    (trestmt, cstmt) <- checkTypesStmt stmt exRetType
    if isCTExprTrue cexpr
      then return (exRetType, CSRepeat cstmt)
      else return (typeVoid, CSWhile cexpr cstmt)
  SExp expr -> do
    (_, cexr) <- checkTypesExpr expr
    return (typeVoid, CSExp cexr)

checkTypesExpr :: Expr -> CheckerType (Type, CTExpr)
checkTypesExpr exp = case exp of
  EVar pid@(PIdent (pos, id)) -> do
    mt <- lookupTypeEnv id
    maybe (fail $ undecError pos id) (\(t, _, _)  -> return (t, (CEVar id, t))) mt
  ELit lit -> case lit of
    LInt _    -> return (typeInt, (CELit lit, typeInt))
    LTrue     -> return (typeBool, (CELit lit, typeBool))
    LFalse    -> return (typeBool, (CELit lit, typeBool))
    LString _ -> return (typeString, (CELit lit, typeString))
  EApp pid@(PIdent (pos, id)) exps -> do
    mt <- lookupTypeEnv id
    (texps, cexs) <- fmap unzip $ mapM (checkTypesExpr) exps
    case mt of
      Nothing      -> fail $ undecError pos id
      Just (t, decPos, _)  -> case t of
        TFun treturn targs -> do
          when (length targs /= length texps) (fail $ "Type Error\n" ++ show pos ++ "\nFunction " ++ show id ++
            " declared at " ++ show decPos ++ " used with bad number of arguments" ++ "\nNumber expected: " ++
            show (length targs) ++ "\nNumber found: " ++ show (length texps) ++ "\n")
          mapM_ (\(ta, te, exp, i) -> do
            when (ta /= te) $ fail $ typeError pos ("Function " ++ show id ++ " declared at " ++ show decPos ++
              " argument no. " ++ show i) (Just exp) ta te
            return ()) (zip4 targs texps exps [1..])
          return (treturn, (CEApp id cexs, treturn))
        _ -> fail $ show pos ++ "\n" ++ id ++ " declared at " ++ show decPos ++ "is not a function\n" ++
               "\nType found: " ++ printTree t
  EUnOp opr exp -> do
    (texp, cexp) <- checkTypesExpr exp
    case opr of
      ONeg (TMinus (pos, opr)) ->
        if texp == typeInt
          then return (typeInt, (CBinOp (CELit (LInt 0), typeInt) "-" cexp, typeInt))
          else fail $ typeError pos ("\nUnary operator " ++ show opr ++ " applied to" ++
            "non-integer expression.") (Just exp) typeInt  texp
      ONot (TExclM (pos, opr))  ->
        if texp == typeBool
          then return (typeBool, (CBinOp (CELit LTrue, typeBool) "^" cexp, typeBool))
          else fail $ typeError pos ("\nUnary operator " ++ show opr ++ " applied to" ++
            "non-boolean expression.") (Just exp) typeBool texp
        --TODO wydziel kod tych metod
  EMul expr1 (TMulOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeInt typeInt
  EAdd expr1 addop expr2 -> case addop of
    OPlus (TPlus info) -> do
      (texpr1, _) <- checkTypesExpr expr1
      if texpr1 == typeInt
        then checkTypesBinOp info expr1 expr2 typeInt typeInt
        else do
          (t, cexpr) <- checkTypesBinOp info expr1 expr2 typeString typeString
          (texpr1, cexpr1) <- checkTypesExpr expr1
          (texpr2, cexpr2) <- checkTypesExpr expr2
          return (t, (CEApp concatStringName [cexpr1, cexpr2], t))
    OMinus (TMinus info) -> checkTypesBinOp info expr1 expr2 typeInt typeInt
  ERel expr1 (TRelOp info@(pos, opr)) expr2 ->
    if opr == eqOp || opr == neqOp
      then do
        (texpr1, cexpr1) <- checkTypesExpr expr1
        (texpr2, cexpr2) <- checkTypesExpr expr2
        when (texpr1 /= texpr2) $ fail $ typeError pos ("\nBinary operator " ++ show opr ++
              " applied to expression.") (Just expr2) texpr1 texpr2
        return (typeBool, (CBinOp cexpr1 opr cexpr2, typeBool))
      else checkTypesBinOp info expr1 expr2 typeInt typeBool
  EAnd expr1 (TLogAndOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeBool typeBool
  EOr expr1 (TLogOrOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeBool typeBool


checkTypesBinOp :: ((Int, Int), String) -> Expr -> Expr -> Type -> Type -> CheckerType (Type, CTExpr)
checkTypesBinOp (pos, opr) expl expr eType rtype = do
  (texpl, cexpl) <- checkTypesExpr expl
  when (texpl /= eType) $ fail $ typeError pos ("\nBinary operator " ++ show opr ++
     " applied to expression.\n") (Just expl) eType texpl
  (texpr, cexpr) <- checkTypesExpr expr
  when (texpr /= eType) $ fail $ typeError pos ("\nBinary operator " ++ show opr ++
       " applied to an expression.\n") (Just expr) eType texpr
  return (rtype, (CBinOp cexpl opr cexpr, rtype))
