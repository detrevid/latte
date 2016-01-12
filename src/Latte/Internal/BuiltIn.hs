module Latte.Internal.BuiltIn where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.Internal.Type

import qualified Data.Map as Map
import Debug.Trace
import Control.Monad.State
import qualified Control.Monad.Trans.State as StateT

binpos = (-1,-1)
bidepth = 0

data FunctionInfo = FunctionInfo {
  functionName       :: String,
  functionReturnType :: Type,
  functionArguments  :: [(Type, String)]
  } deriving (Show)

getArgsTypesFI :: FunctionInfo -> [Type]
getArgsTypesFI fi = map fst (functionArguments fi)

getFunTypeFI :: FunctionInfo -> Type
getFunTypeFI fi = TFun (functionReturnType fi) (getArgsTypesFI fi)

getFunDescFI :: FunctionInfo -> (Type, String, [(Type, String)])
getFunDescFI fi = (functionReturnType fi, functionName fi, functionArguments fi)

getBinFunTypeInfo :: FunctionInfo -> (String, TypeInfo)
getBinFunTypeInfo fi = (functionName fi, ((getFunTypeFI fi), binpos, bidepth))

printIntFI = FunctionInfo {
  functionName       = "printInt",
  functionReturnType = typeVoid,
  functionArguments  = [(typeInt, "x")]
}

printStringFI = FunctionInfo {
  functionName       = "printString",
  functionReturnType = typeVoid,
  functionArguments  = [(typeString, "s")]
}

errorFI = FunctionInfo {
  functionName       = "error",
  functionReturnType = typeVoid,
  functionArguments  = []
}

readIntFI = FunctionInfo {
  functionName       = "readInt",
  functionReturnType = typeInt,
  functionArguments  = []
}

readStringFI = FunctionInfo {
  functionName       = "readString",
  functionReturnType = typeString,
  functionArguments  = []
}

concatStringFI = FunctionInfo {
  functionName       = "concatString",
  functionReturnType = typeString,
  functionArguments  = [(typeString, "str1"), (typeString, "str2")]
}

equalsStringFI = FunctionInfo {
  functionName       = "equalsString",
  functionReturnType = typeBool,
  functionArguments  = [(typeString, "str1"), (typeString, "str2")]
}

builtIns = [printIntFI,  printStringFI, errorFI, readIntFI, readStringFI, concatStringFI, equalsStringFI]

builtInsTypeInfos = map getBinFunTypeInfo builtIns

builtInsDescs = map getFunDescFI builtIns

addBuiltInsToTypeEnv :: TypeEnv -> TypeEnv
addBuiltInsToTypeEnv = flip Map.union $ Map.fromList builtInsTypeInfos
