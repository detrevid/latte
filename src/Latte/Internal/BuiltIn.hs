module Latte.Internal.BuiltIn (addBuiltInsToTypeEnv, builtInsDescs) where

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
printIntRetType = typeVoid
printIntArgs = [(typeInt, "x")]
printIntInfo = (TFun printIntRetType [typeInt], binpos, bidepth)
printIntDesc = (printIntRetType, printIntName, printIntArgs)

printStringName = "printString"
printStringRetType = typeVoid
printStringArgs = [(typeString, "s")]
printStringInfo = (TFun printStringRetType [typeString], binpos, bidepth)
printStringDesc = (printStringRetType, printStringName, printStringArgs)

errorName = "error"
errorRetType = typeVoid
errorArgs = []
errorInfo = (TFun errorRetType [], binpos, bidepth)
errorDesc = (errorRetType, errorName, errorArgs)

readIntName = "readInt"
readIntRetType = typeInt
readIntArgs = []
readIntInfo = (TFun readIntRetType [], binpos, bidepth)
readIntDesc = (readIntRetType, readIntName, readIntArgs)

readStringName = "readString"
readStringRetType = typeString
readStringArgs = []
readStringInfo = (TFun readStringRetType [], binpos, bidepth)
readStringDesc = (readStringRetType, readStringName, readStringArgs)

builtInsTypes = Map.fromList [(printIntName, printIntInfo),
                              (printStringName, printStringInfo),
                              (errorName, errorInfo),
                              (readIntName, readIntInfo),
                              (readStringName, readStringInfo)]

builtInsDescs = [printIntDesc, printStringDesc, errorDesc, readIntDesc, readStringDesc]

addBuiltInsToTypeEnv :: TypeEnv -> TypeEnv
addBuiltInsToTypeEnv = flip Map.union builtInsTypes
