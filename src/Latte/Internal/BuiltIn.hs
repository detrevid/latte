module Latte.Internal.BuiltIn (addBuiltInsToTypeEnv) where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.Internal.Type

import qualified Data.Map as Map
import Debug.Trace
import Control.Monad.State
import qualified Control.Monad.Trans.State as StateT


printIntName = "printInt"
printIntInfo = (TFun typeVoid [typeInt], (-1,-1))

printStringName = "printString"
printStringInfo = (TFun typeVoid [typeString], (-1,-1))

errorName = "error"
errorInfo = (TFun typeVoid [], (-1,-1))

readIntName = "readInt"
readIntInfo = (TFun typeInt [], (-1,-1))

readStringName = "readString"
readStringInfo = (TFun typeString [], (-1,-1))

builtInsTypes = Map.fromList [(printIntName, printIntInfo),
                              (printStringName, printStringInfo),
                              (errorName, errorInfo),
                              (readIntName, readIntInfo),
                              (readStringName, readStringInfo)]

addBuiltInsToTypeEnv :: TypeEnv -> TypeEnv
addBuiltInsToTypeEnv = flip Map.union builtInsTypes
