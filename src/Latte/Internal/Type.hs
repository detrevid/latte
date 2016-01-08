{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Latte.Internal.Type where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad
import Control.Applicative (Applicative)
import Control.Monad.Trans.State
import Debug.Trace


typeInt :: Type
typeInt = TType (TBuiltIn BIInt)
typeBool :: Type
typeBool = TType (TBuiltIn BIBool)
typeString :: Type
typeString = TType (TBuiltIn BIStr)
typeVoid :: Type
typeVoid = TType (TBuiltIn BIVoid)

type Position = (Int, Int)
type VarId = String

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
