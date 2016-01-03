module Latte.Internal.BuiltIn (addBuiltInsToTypeEnv) where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.Internal.Type

import qualified Data.Map as Map
import Debug.Trace
import Control.Monad.State
import qualified Control.Monad.Trans.State as StateT

binpos = (-1,-1)
bidepth = 0

printIntName = "printInt"
printIntInfo = (TFun typeVoid [typeInt], binpos, bidepth)

printStringName = "printString"
printStringInfo = (TFun typeVoid [typeString], binpos, bidepth)

errorName = "error"
errorInfo = (TFun typeVoid [], binpos, bidepth)

readIntName = "readInt"
readIntInfo = (TFun typeInt [], binpos, bidepth)

readStringName = "readString"
readStringInfo = (TFun typeString [], binpos, bidepth)

builtInsTypes = Map.fromList [(printIntName, printIntInfo),
                              (printStringName, printStringInfo),
                              (errorName, errorInfo),
                              (readIntName, readIntInfo),
                              (readStringName, readStringInfo)]

addBuiltInsToTypeEnv :: TypeEnv -> TypeEnv
addBuiltInsToTypeEnv = flip Map.union builtInsTypes
