{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Frontend.TypeChecker (checkTypes) where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.BNFC.PrintLatte
import Latte.Frontend.Optimisations
import Latte.Internal.BuiltIn
import Latte.Internal.Type
import Latte.Internal.ASTInternal

import Control.Applicative (Applicative)
import Control.Conditional (ifM, unlessM)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State
import Data.Functor
import qualified Data.Map as Map
import Data.List

internalErrMsg :: String
internalErrMsg = "Internal error during type checking phase.\n"

data CheckerState = CheckerState  {
  typeEnv          :: TypeEnv,
  funEnv           :: FunEnv,
  classEnv         :: ClassEnv,
  depth            :: Int,
  expectedRetType  :: Type
  }

topLevelDepth = 0
defaultCheckerState = CheckerState {
  typeEnv          = emptyTypeEnv,
  classEnv         = emptyClassEnv,
  funEnv           = emptyFunEnv,
  depth            = topLevelDepth,
  expectedRetType  = typeVoid
  }

newtype CheckerType a = CheckerType (StateT CheckerState Err a)
  deriving (Functor, Applicative, Monad, MonadState CheckerState)

runCheckerType :: CheckerType a -> Err a
runCheckerType (CheckerType x) = evalStateT x defaultCheckerState

putEnv :: TypeEnv -> CheckerType ()
putEnv env = modify $ (\s -> s { typeEnv = env })

getEnv :: CheckerType TypeEnv
getEnv = gets typeEnv

addToEnv :: String -> TypeInfo -> CheckerType ()
addToEnv ident t = modify $ (\s -> s { typeEnv = Map.insert ident t $ typeEnv s })

addToFunEnv :: String -> FunInfo -> CheckerType ()
addToFunEnv ident fi = modify $ (\s -> s { funEnv = Map.insert ident fi $ funEnv s })

lookupFunEnv :: PIdent -> CheckerType FunInfo
lookupFunEnv (PIdent (pos, ident)) = maybe (fail $ undecError pos ident) return =<< Map.lookup ident <$> gets funEnv

{-
removeFromEnv :: String ->  CheckerType ()
removeFromEnv ident = modify $ (\s -> s { environment = Map.delete ident $ environment s })
-}

lookupTypeEnv :: String -> CheckerType (Maybe TypeInfo)
lookupTypeEnv ident = do
  env <- getEnv
  return $ Map.lookup ident env

lookupVarTypeEnv :: PIdent -> CheckerType TypeInfo
lookupVarTypeEnv (PIdent (pos, ident)) = do
  env <- getEnv
  let mti = Map.lookup ident env
  maybe (fail $ undecError pos ident) return mti

getDepth :: CheckerType Int
getDepth = gets depth

changeDepth' :: (Int -> Int) -> CheckerType ()
changeDepth' f = modify $ (\s -> s { depth = f $ depth s })

incDepth :: CheckerType ()
incDepth = changeDepth' (1+)

decDepth :: CheckerType ()
decDepth = changeDepth' (1-)

getClassInfo :: PIdent -> CheckerType ClassInfo
getClassInfo (PIdent (pos, ident)) = do
  let t = classType ident
  mci <- gets $ (Map.lookup ident) . classEnv
  maybe (fail $ unknownTypeError pos t) return mci

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

unknownTypeError :: Position -> Type -> String
unknownTypeError pos t = typeError pos "Unknown type." Nothing Nothing t

checkTypes' :: Program -> CheckerType CProgramInfo
checkTypes' prog@(Program topdefs) = do
  modify $ \s -> s { funEnv = addBuiltInsToFunEnv $ funEnv s, classEnv = addBuiltInsToClassEnv $ classEnv s }
  addTopDefsToEnv prog
  ctopdefs <- concat <$> mapM checkTypesTopDef topdefs
  cenv <- gets classEnv
  fenv <- gets funEnv
  return $ (CProgram $ builtInsClassesDefs ++ ctopdefs, cenv, fenv)

checkTypes :: Program -> Err CProgramInfo
checkTypes prog = runCheckerType $ checkTypes' prog

addThisArgToFun :: PIdent -> FunDef -> CheckerType FunDef
addThisArgToFun classPId (FunDef tret mpid@(PIdent (mpos, _)) args block) = do
  let args' = (Arg (TType (TClass (CPType classPId))) (PIdent (mpos, "this"))) : args
  return $ FunDef tret mpid args' block

makeFunNameFromMet :: String -> String -> String
makeFunNameFromMet classId methodId =  "_cm_" ++ classId ++ "_" ++ methodId

methodToFun :: PIdent -> FunDef -> CheckerType FunDef
methodToFun classPId@(PIdent (_, classId)) fdef = do
  FunDef tret (PIdent (mpos, mid)) args block <- addThisArgToFun classPId fdef
  return $ FunDef tret (PIdent (mpos, makeFunNameFromMet classId mid)) args block

addTopDefToEnv :: TopDef -> CheckerType ()
addTopDefToEnv tdef = case tdef of
  TDFnDef (FunDef tret (PIdent (pos, id)) args _) -> do
    mt <- lookupTypeEnv id
    case mt of
      Nothing -> do
        let targs = map (\(Arg t _) -> removePosFromType t) args
        addToFunEnv id (removePosFromType tret, targs, pos)
      Just (_, decPos, _) -> fail $ redecError pos id decPos
  TDCDef (CDef pid body) -> addTopDefToEnv $ TDCDef (CDefE pid objectClassPId body)
  TDCDef (CDefE pid@(PIdent (pos, classId)) (PIdent (_, superClassId)) (CBody fdecls)) -> do
    mc <- gets $ (Map.lookup classId) . classEnv
    case mc of
      Nothing -> do
        let fields = Map.fromList $ map (\((ident, t), i) -> (ident, (removePosFromType t, i, classId))) $ zip (concatMap (
                       \fdecl -> case fdecl of
                         CVar t idents -> map (\(PIdent (_, ident)) -> (ident, t)) idents
                         _ -> []) fdecls) [1..]
            classInfo = ClassInfo fields (Just superClassId) [] pos
        mapM_ (\fdecl -> case fdecl of
                 CVar _ _ -> return ()
                 CMet fdef -> addTopDefToEnv =<< TDFnDef <$> methodToFun pid fdef
                  ) fdecls
        modify (\s -> s { classEnv = Map.insert classId classInfo $ classEnv s })
      Just cinfo -> fail $ redecError pos classId $ classDeclPos cinfo

addTopDefsToEnv :: Program -> CheckerType ()
addTopDefsToEnv (Program tdefs) = do
  mapM_ addTopDefToEnv tdefs
  computeSubclasses
  computeAllDerivedFieldsInClasses

removePosFromType :: Type -> Type
removePosFromType t = case t of
  TType (TClass (CPType (PIdent (_, id)))) -> classType id
  _ -> t

checkDeclVarType :: Position -> Type -> CheckerType Type
checkDeclVarType pos t = do
  when (elem t permittedVarTypes) (fail $ typeError pos "Use of permitted type in declaration." Nothing Nothing t)
  case t of
    TType (TBuiltIn _) -> return t
    TType (TClass (CPType pid@(PIdent (_, ident)))) -> do
      getClassInfo pid
      return $ classType ident
    _ -> fail internalErrMsg

checkTypesFunDef :: FunDef -> CheckerType CFunDef
checkTypesFunDef (FunDef tret (PIdent (pos, ident)) args block) = do
  oldEnv <- getEnv
  incDepth
  depth <- getDepth
  cargs <- mapM (\(Arg t (PIdent (pos, ident))) -> do
    t' <- checkDeclVarType pos t
    addToEnv ident (t', pos, depth)
    return $ CArg t' ident) args
  let tret' = removePosFromType tret
  modify (\s -> s { expectedRetType = tret' })
  (tblock, CBlock cblock) <- checkTypesBlock block
  unlessM (compareTypes tret' tblock) (fail $ typeError pos ("Function " ++ ident ++
    " not always returns expected type.") Nothing (Just tret') tblock)
  decDepth
  putEnv oldEnv
  if tret' == typeVoid
    then return $ CFunDef tret' ident cargs (CBlock $ cblock ++ [CSVRet])
    else return $ CFunDef tret' ident cargs (CBlock cblock)

checkTypesTopDef :: TopDef -> CheckerType [CTopDef]
checkTypesTopDef tdef = case tdef of
  TDFnDef fundef -> (:[]) <$> CTDFnDef <$> checkTypesFunDef fundef
  TDCDef (CDef pid body) -> checkTypesTopDef $ TDCDef (CDefE pid objectClassPId body)
  TDCDef (CDefE cpid@(PIdent (_, cid)) (PIdent (_, superClassId)) (CBody fdecls)) -> do
    oldEnv <- getEnv
    incDepth
    depth <- getDepth
    decls' <- concat <$> mapM (\fdecl -> case fdecl of
        CVar t pidents -> do
          let declPos = (\(PIdent (pos, _)) -> pos) (head pidents)
          t' <- checkDeclVarType declPos t
          idents <- mapM (\pid@(PIdent (pos, ident)) -> do
            checkIfRedeclared pid
            addToEnv ident (t', pos, depth)
            return ident) pidents
          return $ [CCVar t' idents]
        _ -> return []
        ) fdecls
    decDepth
    putEnv oldEnv
    methods <- concat <$> mapM (\x -> case x of
        CMet fdef -> fmap ((:[]) . CTDFnDef) $ checkTypesFunDef =<< methodToFun cpid fdef
        _ -> return []
        ) fdecls
    return $ [CTDCDef $ CCDef cid (Just superClassId) (CCBody decls')] ++ methods

addSubclass :: String -> String -> CheckerType ()
addSubclass classId subClassId = do
  mci <- gets $ (Map.lookup classId) . classEnv
  cinf <- maybe (fail internalErrMsg) return mci
  let subClasses' = subClassId : (classSubClasses cinf)
      cinf' = cinf { classSubClasses = subClasses' }
  modify $ \s -> s { classEnv = Map.update (const $ Just cinf') classId $ classEnv s }

computeSubclasses :: CheckerType ()
computeSubclasses = do
  cenv <- gets $ classEnv
  mapM_ (\(cid, cinf) ->
    maybe (return ()) (\x -> addSubclass x cid) $ classSuperClass cinf) $ Map.toList cenv

computeAllDerivedFieldsInClasses' :: ClassFieldEnv -> String -> CheckerType ()
computeAllDerivedFieldsInClasses' dfields classId = do
  mci <- gets $ (Map.lookup classId) . classEnv
  cinf <- maybe (fail internalErrMsg) return mci
  let dfields' =  Map.union dfields $ classFields cinf
      cinf' = cinf { classFields = dfields' }
  modify $ \s -> s { classEnv = Map.update (const $ Just cinf') classId $ classEnv s }
  mapM_ (computeAllDerivedFieldsInClasses' dfields') $ classSubClasses cinf'

computeAllDerivedFieldsInClasses :: CheckerType ()
computeAllDerivedFieldsInClasses = computeAllDerivedFieldsInClasses' emptyClassFieldEnv objectClassId

checkTypesBlock :: Block -> CheckerType (Type, CBlock)
checkTypesBlock (Block stmts) = do
  incDepth
  (tstmts, cstmts) <- unzip <$> mapM checkTypesStmt stmts
  decDepth
  exRetType <- gets expectedRetType
  ifM (fmap (any id) $ mapM (compareTypes exRetType) tstmts) (return (exRetType, CBlock cstmts)) (return (typeVoid, CBlock cstmts))

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
  SDecl decl -> do
    decl' <- checkTypesDecl decl
    return (typeVoid, CSDecl decl')
  SAss ref expr -> do
    (t, cref) <- checkTypesRef ref
    (texpr, cexpr) <- checkTypesExpr expr
    --TODO write get pos
    unlessM (compareTypes t texpr) (fail $ typeError (0, 0) "Bad expression type after assignment sign." (Just expr) (Just t) texpr)
    return (typeVoid, CSAss (cref, t) (setType t cexpr))
  SDIncr pid@(PIdent (pos, ident)) (TDIOp (_, opr)) -> do
    (t, _, _) <- lookupVarTypeEnv pid
    let opr' = if opr == "++" then "+" else "-"
    when (t /= typeInt) $ fail $ typeError pos ("Variable " ++ ident ++ ".") Nothing (Just typeInt)  t
    return (typeVoid, (CSAss ((CRVar ident), t) ((CBinOp ((CERef (CRVar ident)), t) opr' ((CELit (CLInt 1)), typeInt)), typeInt)))
  SRet (TRet (pos, _)) expr -> do
    (texpr, cexpr) <- checkTypesExpr expr
    exRetType <- gets expectedRetType
    unlessM (compareTypes exRetType texpr) (fail $ typeError pos "Bad return type." (Just expr) (Just exRetType) texpr)
    return (texpr, CSRet $ setType exRetType cexpr)
  SVRet (TRet (pos, _)) -> do
    exRetType <- gets expectedRetType
    unlessM (compareTypes exRetType typeVoid) (fail $ typeError pos "Bad return type." Nothing (Just exRetType) typeVoid)
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

checkTypesDecl :: Decl -> CheckerType CDecl
checkTypesDecl (Decl t items) = do
  depth <- getDepth
  let declPos = (\item -> case item of
                   INoInit (PIdent (pos, _)) -> pos
                   IInit (PIdent (pos, _)) _ -> pos) (head items)
  t' <- checkDeclVarType declPos t
  citems <- mapM (\item -> case item of
    INoInit pid@(PIdent (pos, id))   -> do
      checkIfRedeclared pid
      addToEnv id (t', pos, depth)
      return $ CINoInit id
    IInit pid@(PIdent (pos, id)) exp -> do
      checkIfRedeclared pid
      (texp, cexp) <- checkTypesExpr exp
      unlessM (compareTypes t' texp) $ fail $ typeError pos ("Initialization of variable " ++ show id ++ ".") (Just exp) (Just t') texp
      addToEnv id (t', pos, depth)
      return $ CIInit id $ setType t' cexp
        ) items
  return $ CDecl t' citems

checkTypesExpr :: Expr -> CheckerType (Type, CTExpr)
checkTypesExpr expr = do
  (t, cexpr) <- checkTypesExpr' expr
  cexpr' <- CheckerType $ lift $ constantFolding cexpr
  return (t, cexpr')

setType :: Type -> CTExpr -> CTExpr
setType t (x, _) = (x, t)

checkTypesExpr' :: Expr -> CheckerType (Type, CTExpr)
checkTypesExpr' x = case x of
  ERef ref -> do
    (t, cref) <- checkTypesRef ref
    return (t, (CERef cref, t))
  ELit lit -> case lit of
    LInt val    -> return (typeInt, (CELit (CLInt val), typeInt))
    LTrue       -> return (typeBool, (CELit (CLBool True), typeBool))
    LFalse      -> return (typeBool, (CELit (CLBool False), typeBool))
    LString str -> return (typeString, (CELit (CLString str), typeString))
  EApp (FApp pid@(PIdent (pos, ident)) exps) -> do
    (treturn, targs, decPos) <- lookupFunEnv pid
    (texps, cexs) <- unzip <$> mapM (checkTypesExpr) exps
    when (length targs /= length texps) (fail $ "Type Error\n" ++ show pos ++ "\nFunction " ++ ident ++
      " declared at " ++ show decPos ++ " used with bad number of arguments." ++ "\nNumber expected: " ++
      show (length targs) ++ "\nNumber found: " ++ show (length texps) ++ "\n")
    mapM_ (\(ta, te, exp, i) -> do
      unlessM (compareTypes ta te) $ fail $ typeError pos ("Function " ++ ident ++ " declared at " ++ show decPos ++
        " argument no. " ++ show i ++ ".") (Just exp) (Just ta) te
      return ()) (zip4 targs texps exps [1..])
    return (treturn, (CEApp ident cexs, treturn))
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
  ENew (NClass pid@(PIdent (_, ident))) -> do
    getClassInfo pid
    let t' = classType ident
    return (t', (CENew (CNClass ident), t'))
  ENull expr -> case expr of
    ERef (RVar pid@(PIdent (_, ident))) -> do
      getClassInfo pid
      let t' = classType ident
      return (t', (CENull t', t'))
    _ -> fail $ "Expression is not a type." --TODO add position
  EMet varId (FApp (PIdent (mpos, mid)) mexps) -> do
    classId <- getVarClassId varId
    let fname = makeFunNameFromMet classId mid
        thisExp = ERef $ RVar varId
        mexps' = thisExp : mexps
    --TODO maybe better msg if method does not exist
    checkTypesExpr $ EApp $ FApp (PIdent (mpos, fname)) mexps'

getVarClassId :: PIdent -> CheckerType String
getVarClassId varPId@(PIdent (varPos, _)) = do
  (t, _, _) <- lookupVarTypeEnv varPId
  case t of
    TType (TClass (CType (Ident classId))) -> return classId
    _ -> fail $ typeError varPos "Dot operator applied to forbbiden type." Nothing Nothing t

--TODO remove unnecessary search
getClassField :: String -> PIdent -> CheckerType ClassFieldInfo
getClassField classId fieldPId@(PIdent (fieldPos, fieldId)) = do
  mci <- gets $ (Map.lookup classId) . classEnv
  ClassInfo fields superClass _ declClassPos <- maybe (fail internalErrMsg) return mci
  let mf = Map.lookup fieldId fields
  case mf of
    Nothing  -> do
      case superClass of
        Nothing -> fail $ show fieldPos ++ "\nClass " ++ classId ++ " declared at " ++ show declClassPos ++
                " does not have filed named " ++ fieldId ++ ".\n"
        Just superClassId -> getClassField superClassId fieldPId
    Just cfi -> return $ cfi

checkTypesRef :: Ref -> CheckerType (Type, CRef)
checkTypesRef x = case x of
  RDot varPid@(PIdent (_, varIdent)) fieldPId -> do
    classId <- getVarClassId varPid
    (ftype, i, fclass) <- getClassField classId fieldPId
    return (ftype, CRDot varIdent i fclass)
  RVar pid@(PIdent (_, ident)) -> do
    (t, _, _) <- lookupVarTypeEnv pid
    return (t, CRVar ident)

checkTypesBinOp :: (Position, String) -> Expr -> Expr -> Type -> Type -> CheckerType (Type, CTExpr)
checkTypesBinOp (pos, opr) expl expr eType rtype = do
  (texpl, cexpl) <- checkTypesExpr expl
  unlessM (compareTypes eType texpl) $ fail $ typeError pos ("Binary operator " ++ show opr ++
     " applied to expression.") (Just expl) (Just eType) texpl
  (texpr, cexpr) <- checkTypesExpr expr
  unlessM (compareTypes eType texpr) $ fail $ typeError pos ("Binary operator " ++ show opr ++
       " applied to an expression.") (Just expr) (Just eType) texpr
  return (rtype, (CBinOp (setType eType cexpl) opr (setType eType cexpr), rtype))

isSuperclass :: String -> String -> CheckerType Bool
isSuperclass x y = do
  mci <- gets $ (Map.lookup y) . classEnv
  cinf <- maybe (fail internalErrMsg) return mci
  maybe (return False) (\z -> if z == x then return True else isSuperclass x z) $ classSuperClass cinf

isSubclass :: String -> String -> CheckerType Bool
isSubclass x y = isSuperclass y x

compareTypes :: Type -> Type -> CheckerType Bool
compareTypes x y = compareTypes' (removePosFromType x) (removePosFromType y)

compareTypes' :: Type -> Type -> CheckerType Bool
compareTypes' (TType (TClass (CType (Ident  x)))) (TType (TClass (CType (Ident y)))) = fmap (x == y ||) $ isSubclass y x
compareTypes' x y = return $ x == y
