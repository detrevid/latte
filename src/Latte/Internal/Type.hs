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

type Position = (Int, Int)

type TypeInfo = (Type, Position, Int)

type TypeEnv = Map.Map String TypeInfo
emptyTypeEnv :: TypeEnv
emptyTypeEnv = Map.empty

negOp = "-"
notOp = "!"

eqOp = "=="
neqOp = "!="

logAndOp = "&&"
logOrOp = "||"
logOps = [logAndOp, logOrOp]

permittedVarTypes = [typeVoid]
