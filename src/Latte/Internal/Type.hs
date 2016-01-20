{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Internal.Type where

import Latte.BNFC.AbsLatte

import qualified Data.Map as Map

typeInt :: Type
typeInt = TType (TBuiltIn BIInt)
typeBool :: Type
typeBool = TType (TBuiltIn BIBool)
typeString :: Type
typeString = TType (TBuiltIn BIStr)
typeVoid :: Type
typeVoid = TType (TBuiltIn BIVoid)

classType :: String -> Type
classType x = TType $ TClass $ CType $ Ident x

type Position = (Int, Int)

{-data TypeInfo = TypeInfo { tiType :: Type, tiDecPosition :: Position, tiDecDepth :: Int }-}
type TypeInfo = (Type, Position, Int)
type TypeEnv = Map.Map String TypeInfo
emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty

type FunInfo = (Type, [Type], Position)
type FunEnv = Map.Map String FunInfo
emptyFunEnv :: FunEnv
emptyFunEnv = Map.empty

type ClassFieldInfo = (Type, Integer, String)
type ClassFieldEnv = Map.Map String ClassFieldInfo
emptyClassFieldEnv = Map.empty

data ClassInfo = ClassInfo {
  classFields     :: ClassFieldEnv,
  classSuperClass :: Maybe String,
  classSubClasses :: [String],
  classDeclPos    :: Position
  } deriving (Eq, Ord, Show, Read)
defaultClassInfo = ClassInfo Map.empty Nothing [] (0, 0)
type ClassEnv = Map.Map String ClassInfo
emptyClassEnv :: ClassEnv
emptyClassEnv = Map.empty

negOp = "-"
notOp = "!"

eqOp = "=="
neqOp = "!="

logAndOp = "&&"
logOrOp = "||"
logOps = [logAndOp, logOrOp]

permittedVarTypes = [typeVoid]
