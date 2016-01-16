{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Frontend.TypeChecker (checkTypes, getGlobalCDefsTypesEnv) where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.BNFC.PrintLatte
import Latte.Frontend.Optimisations
import Latte.Internal.BuiltIn
import Latte.Internal.Type
import Latte.Internal.ASTInternal

import Control.Applicative (Applicative)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State
import qualified Data.Map as Map
import Data.List

internalErrMsg :: String
internalErrMsg = "Internal error during type checking phase.\n"

data CheckerState = CheckerState  {
  environment     :: TypeEnv,
  depth           :: Int,
  expectedRetType :: Type
  }

topLevelDepth = 0
defaultCheckerState = CheckerState {
  environment = emptyTypeEnv,
  depth = topLevelDepth,
  expectedRetType = typeVoid
  }

newtype CheckerType a = CheckerType (StateT CheckerState Err a)
  deriving (Functor, Applicative, Monad, MonadState CheckerState)

runCheckerType :: CheckerType a -> Err a
runCheckerType (CheckerType x) = evalStateT x defaultCheckerState

putEnv :: TypeEnv -> CheckerType ()
putEnv env = modify $ (\s -> s { environment = env })

getEnv :: CheckerType TypeEnv
getEnv = gets environment

addToEnv :: String -> TypeInfo -> CheckerType ()
addToEnv ident t = modify $ (\s -> s { environment = Map.insert ident t $ environment s })

{-
removeFromEnv :: String ->  CheckerType ()
removeFromEnv ident = modify $ (\s -> s { environment = Map.delete ident $ environment s })
-}

lookupTypeEnv :: String -> CheckerType (Maybe TypeInfo)
lookupTypeEnv id = do
  env <- getEnv
  return $ Map.lookup id env

getDepth :: CheckerType Int
getDepth = gets depth

changeDepth' :: (Int -> Int) -> CheckerType ()
changeDepth' f = modify $ (\s -> s { depth = f $ depth s })

incDepth :: CheckerType ()
incDepth = changeDepth' (1+)

decDepth :: CheckerType ()
decDepth = changeDepth' (1-)

typeError :: Position -> String -> Maybe Expr -> Maybe Type -> Type -> String
typeError pos errMsg mexpr mtexpected tfound  =
  "Type Error\n" ++
  show pos ++ "\n" ++
  errMsg ++ "\n" ++
  exprStr ++
  texpectedStr ++
  "Type found: " ++ printTree tfound ++ "\n"
 where exprStr = maybe "" (\exp -> "Expression: " ++ printTree exp ++ "\n") mexpr
       texpectedStr = maybe "" (\t -> "Type expected: " ++ printTree t ++ "\n") mtexpected

undecError :: Position -> String -> String
undecError pos ident =
   show pos ++ "\n" ++ ident ++ " has not been declared.\n"

redecError :: Position -> String -> Position -> String
redecError pos ident decPos =
   show pos ++ "\n" ++ ident ++ " has been already declared in this block at: " ++ show decPos ++ ".\n"

checkTypes' :: Program -> CheckerType CProgram
checkTypes' prog@(Program topdefs) = do
  putEnv $ addBuiltInsToTypeEnv emptyTypeEnv
  addTopDefsToEnv prog
  ctopdefs <- mapM checkTypesTopDef topdefs
  return $ CProgram ctopdefs

checkTypes :: Program -> Err CProgram
checkTypes prog = runCheckerType $ checkTypes' prog

getGlobalCDefsTypesEnv' :: CProgram -> CheckerType TypeEnv
getGlobalCDefsTypesEnv' prog = do
  putEnv $ addBuiltInsToTypeEnv emptyTypeEnv
  addTopCDefsToEnv prog
  getEnv

getGlobalCDefsTypesEnv :: CProgram -> TypeEnv
getGlobalCDefsTypesEnv prog =
  case (runCheckerType $ getGlobalCDefsTypesEnv' prog) of
    Ok env -> env
    Bad _ -> emptyTypeEnv

addTopDefsToEnv :: Program -> CheckerType ()
addTopDefsToEnv (Program tdefs) =
  mapM_ (\(TDFnDef tret (PIdent (pos, ident)) args _) -> do
    mt <- lookupTypeEnv ident
    case mt of
      Nothing -> do
        let targs = map (\(Arg t _) -> t) args
        addToEnv ident ((TFun tret targs), pos, topLevelDepth)
      Just (_, decPos, _)  -> fail $ redecError pos ident decPos
      ) tdefs

addTopCDefsToEnv :: CProgram -> CheckerType ()
addTopCDefsToEnv (CProgram tdefs) =
  mapM_ (\(CTDFnDef tret ident args _) -> do
    mt <- lookupTypeEnv ident
    case mt of
      Nothing         -> do
        let targs = map (\(CArg t _) -> t) args
        addToEnv ident ((TFun tret targs), (0, 0), topLevelDepth)
      Just _ -> fail $ internalErrMsg
      ) tdefs

checkDeclVarType :: Position -> Type -> CheckerType ()
checkDeclVarType pos t = do
  when (elem t permittedVarTypes) (fail $ typeError pos "Use of permitted type in declaration." Nothing Nothing t)
  return ()

checkTypesTopDef :: TopDef -> CheckerType CTopDef
checkTypesTopDef tdef = case tdef of
  TDFnDef tret (PIdent (pos, ident)) args block -> do
    oldEnv <- getEnv
    incDepth
    depth <- getDepth
    cargs <- mapM (\(Arg t (PIdent (pos, ident))) -> do
      checkDeclVarType pos t
      addToEnv ident (t, pos, depth)
      return $ CArg t ident) args
    modify (\s -> s { expectedRetType = tret })
    (tblock, CBlock cblock) <- checkTypesBlock block
    when (tblock /= tret) (fail $ typeError pos ("Function " ++ ident ++
      " not always returns expected type.") Nothing (Just tret) tblock)
    decDepth
    putEnv oldEnv
    if tret == typeVoid
      then return $ CTDFnDef tret ident cargs (CBlock $ cblock ++ [CSVRet])
      else return $ CTDFnDef tret ident cargs (CBlock cblock)

checkTypesBlock :: Block -> CheckerType (Type, CBlock)
checkTypesBlock (Block stmts) = do
  incDepth
  (tstmts, cstmts) <- fmap unzip $ mapM checkTypesStmt stmts
  decDepth
  exRetType <- gets expectedRetType
  if any ((==) exRetType) tstmts
    then return (exRetType, CBlock cstmts)
    else return (typeVoid, CBlock cstmts)

checkIfRedeclared :: PIdent -> CheckerType ()
checkIfRedeclared (PIdent (pos, ident)) = do
  depth <- getDepth
  mt <- lookupTypeEnv ident
  maybe (return ()) (\(_, decPos, depthDec) -> when (depth == depthDec) $ fail $ redecError pos ident decPos) mt
  return ()

checkTypesStmt :: Stmt -> CheckerType (Type, CStmt)
checkTypesStmt x = case x of
  SEmpty -> return (typeVoid, CSEmpty)
  SBlock block -> do
    odlEnv <- getEnv
    (t, cblock) <- checkTypesBlock block
    putEnv odlEnv
    return (t, CSBlock cblock)
  SDecl t items -> do
    depth <- getDepth
    let declPos = (\item -> case item of
                     INoInit (PIdent (pos, _)) -> pos
                     IInit (PIdent (pos, _)) _ -> pos) (head items)
    checkDeclVarType declPos t
    citems <- mapM (\item -> case item of
      INoInit pid@(PIdent (pos, ident))   -> do
        checkIfRedeclared pid
        addToEnv ident (t, pos, depth)
        return $ CINoInit ident
      IInit pid@(PIdent (pos, ident)) exp -> do
        checkIfRedeclared pid
        (texp, cexp) <- checkTypesExpr exp
        when (texp /= t) $ fail $ typeError pos ("Initialization of variable " ++ ident ++ ".") (Just exp) (Just t) texp
        addToEnv ident (t, pos, depth)
        return $ CIInit ident cexp
          ) items
    return (typeVoid, CSDecl t citems)
  SAss (PIdent (pos, ident)) expr -> do
    mt <- lookupTypeEnv ident
    case mt of
      Nothing         -> fail $ undecError pos ident
      Just (t, _, _)  -> do
        (texpr, cexpr) <- checkTypesExpr expr
        when (texpr /= t) (fail $ typeError pos "Bad expression type after assignment sign." (Just expr) (Just t) texpr)
        return (typeVoid, CSAss ident cexpr)
  SDIncr (PIdent (pos, ident)) (TDIOp (_, opr)) -> do
    mt <- lookupTypeEnv ident
    let opr' = if opr == "++" then "+" else "-"
    case mt of
      Nothing         -> fail $ undecError pos ident
      Just (t, _, _)  ->
        if t /= typeInt
          then fail $ typeError pos ("Variable " ++ ident ++ ".") Nothing (Just typeInt)  t
          else return (typeVoid, (CSAss ident ((CBinOp ((CEVar ident), t) opr' ((CELit (CLInt 1)), typeInt)), typeInt)))
  SRet (TRet (pos, _)) expr -> do
    (texpr, cexpr) <- checkTypesExpr expr
    when (texpr == typeVoid) (fail $ typeError pos "Use of permitted type in return statement." Nothing Nothing texpr)
    exRetType <- gets expectedRetType
    when (texpr /= exRetType) (fail $ typeError pos "Bad return type." (Just expr) (Just exRetType) texpr)
    return (texpr, CSRet cexpr)
  SVRet (TRet (pos, _)) -> do
    exRetType <- gets expectedRetType
    when (typeVoid  /= exRetType) (fail $ typeError pos "Bad return type." Nothing (Just exRetType) typeVoid)
    return (typeVoid, CSVRet)
  SCond (TIf (pos, _)) expr stmt -> do
    (texpr, cexpr) <- checkTypesExpr expr
    odlEnv <- getEnv
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in if condition." (Just expr) (Just typeBool) texpr)
    (tstmt, cstmt) <- checkTypesStmt stmt
    putEnv odlEnv
    if isCTExprTrue cexpr
      then return (tstmt, cstmt)
      else return (typeVoid, CSCondElse cexpr cstmt CSEmpty)
  SCondElse (TIf (pos, _)) expr stmt1 stmt2 -> do
    (texpr, cexpr) <- checkTypesExpr expr
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in if condition." (Just expr) (Just typeBool) texpr)
    odlEnv <- getEnv
    (tretif, cstmt1) <- checkTypesStmt stmt1
    putEnv odlEnv
    (tretel, cstmt2)<- checkTypesStmt stmt2
    putEnv odlEnv
    exRetType <- gets expectedRetType
    case (isCTExprTrue cexpr, isCTExprFalse cexpr) of
      (True, _) -> return (tretif, cstmt1)
      (_, True) -> return (tretel, cstmt2)
      (_, _)    ->
        if (tretif == typeVoid || tretel == typeVoid)
          then return (typeVoid, CSCondElse cexpr cstmt1 cstmt2)
          else return (exRetType, CSCondElse cexpr cstmt1 cstmt2)
  SWhile (TWhile (pos, _)) expr stmt -> do
    (texpr, cexpr) <- checkTypesExpr expr
    when (texpr /= typeBool) (fail $ typeError pos "Bad type in while condition." (Just expr) (Just typeBool) texpr)
    odlEnv <- getEnv
    (_, cstmt) <- checkTypesStmt stmt
    putEnv odlEnv
    exRetType <- gets expectedRetType
    if isCTExprTrue cexpr
      then return (exRetType, CSRepeat cstmt)
      else return (typeVoid, CSWhile cexpr cstmt)
  SExp expr -> do
    (_, cexr) <- checkTypesExpr expr
    return (typeVoid, CSExp cexr)

checkTypesExpr :: Expr -> CheckerType (Type, CTExpr)
checkTypesExpr expr = do
  (t, cexpr) <- checkTypesExpr' expr
  cexpr' <- CheckerType $ lift $ constantFolding cexpr
  return (t, cexpr')

checkTypesExpr' :: Expr -> CheckerType (Type, CTExpr)
checkTypesExpr' x = case x of
  EVar (PIdent (pos, ident)) -> do
    mt <- lookupTypeEnv ident
    maybe (fail $ undecError pos ident) (\(t, _, _)  -> return (t, (CEVar ident, t))) mt
  ELit lit -> case lit of
    LInt val    -> return (typeInt, (CELit (CLInt val), typeInt))
    LTrue       -> return (typeBool, (CELit (CLBool True), typeBool))
    LFalse      -> return (typeBool, (CELit (CLBool False), typeBool))
    LString str -> return (typeString, (CELit (CLString str), typeString))
  EApp (PIdent (pos, ident)) exps -> do
    mt <- lookupTypeEnv ident
    (texps, cexs) <- fmap unzip $ mapM (checkTypesExpr) exps
    case mt of
      Nothing      -> fail $ undecError pos ident
      Just (t, decPos, _)  -> case t of
        TFun treturn targs -> do
          when (length targs /= length texps) (fail $ "Type Error\n" ++ show pos ++ "\nFunction " ++ ident ++
            " declared at " ++ show decPos ++ " used with bad number of arguments." ++ "\nNumber expected: " ++
            show (length targs) ++ "\nNumber found: " ++ show (length texps) ++ "\n")
          mapM_ (\(ta, te, exp, i) -> do
            when (ta /= te) $ fail $ typeError pos ("Function " ++ ident ++ " declared at " ++ show decPos ++
              " argument no. " ++ show i ++ ".") (Just exp) (Just ta) te
            return ()) (zip4 targs texps exps [1..])
          return (treturn, (CEApp ident cexs, treturn))
        _ -> fail $ typeError pos (ident ++ " declared at " ++ show decPos ++ " is not a function.") Nothing Nothing t
  EUnOp opr exp -> do
    (texp, cexp) <- checkTypesExpr exp
    case opr of
      ONeg (TMinus (pos, opr)) ->
        if texp == typeInt
          then return (typeInt, (CBinOp (CELit (CLInt 0), typeInt) "-" cexp, typeInt))
          else fail $ typeError pos ("Unary operator " ++ show opr ++ " applied to " ++
            "non-integer expression.") (Just exp) (Just typeInt)  texp
      ONot (TExclM (pos, opr))  ->
        if texp == typeBool
          then return (typeBool, (CBinOp (CELit (CLBool True), typeBool) "^" cexp, typeBool))
          else fail $ typeError pos ("Unary operator " ++ show opr ++ " applied to " ++
            "non-boolean expression.") (Just exp) (Just typeBool) texp
  EMul expr1 (TMulOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeInt typeInt
  EAdd expr1 addop expr2 -> case addop of
    OPlus (TPlus info) -> do
      (texpr1, _) <- checkTypesExpr expr1
      if texpr1 == typeInt
        then checkTypesBinOp info expr1 expr2 typeInt typeInt
        else do
          (t, _) <- checkTypesBinOp info expr1 expr2 typeString typeString
          (_, cexpr1) <- checkTypesExpr expr1
          (_, cexpr2) <- checkTypesExpr expr2
          cexprFolded <- CheckerType $ lift $ constantFolding $ (CBinOp cexpr1 "+" cexpr2, t)
          case cexprFolded of
            (CBinOp cexpr1 _ cexpr2, _) -> return (t, (CEApp (functionName concatStringFI) [cexpr1, cexpr2], t))
            (CELit _, _) -> return (t, cexprFolded)
            _ -> fail internalErrMsg
    OMinus (TMinus info) -> checkTypesBinOp info expr1 expr2 typeInt typeInt
  ERel expr1 (TRelOp info@(pos, opr)) expr2 ->
    if opr == eqOp || opr == neqOp
      then do
        (texpr1, cexpr1) <- checkTypesExpr expr1
        (texpr2, cexpr2) <- checkTypesExpr expr2
        when (texpr1 /= texpr2) $ fail $ typeError pos ("Binary operator " ++ show opr ++
              " applied to expression.") (Just expr2) (Just texpr1) texpr2
        if texpr1 == typeString
          then
            if opr == eqOp
              then return (typeBool, (CEApp (functionName equalsStringFI) [cexpr1, cexpr2], typeBool))
              else return (typeBool, (CBinOp (CELit (CLBool True), typeBool) "^"
                (CEApp (functionName equalsStringFI) [cexpr1, cexpr2], typeBool), typeBool))
          else return (typeBool, (CBinOp cexpr1 opr cexpr2, typeBool))
      else checkTypesBinOp info expr1 expr2 typeInt typeBool
  EAnd expr1 (TLogAndOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeBool typeBool
  EOr expr1 (TLogOrOp info) expr2 -> checkTypesBinOp info expr1 expr2 typeBool typeBool


checkTypesBinOp :: (Position, String) -> Expr -> Expr -> Type -> Type -> CheckerType (Type, CTExpr)
checkTypesBinOp (pos, opr) expl expr eType rtype = do
  (texpl, cexpl) <- checkTypesExpr expl
  when (texpl /= eType) $ fail $ typeError pos ("Binary operator " ++ show opr ++
     " applied to expression.") (Just expl) (Just eType) texpl
  (texpr, cexpr) <- checkTypesExpr expr
  when (texpr /= eType) $ fail $ typeError pos ("Binary operator " ++ show opr ++
       " applied to an expression.") (Just expr) (Just eType) texpr
  return (rtype, (CBinOp cexpl opr cexpr, rtype))
