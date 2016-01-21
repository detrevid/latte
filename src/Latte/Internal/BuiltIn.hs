module Latte.Internal.BuiltIn where

import Latte.BNFC.AbsLatte
import Latte.Internal.Type
import Latte.Internal.ASTInternal

import qualified Data.Map as Map

binpos = (-1,-1)
bidepth = 0

getFunTypeFI :: FunctionInfo -> Type
getFunTypeFI fi = TFun (functionReturnType fi) (functionArgsTypes fi)

getFunDescFI :: FunctionInfo -> (Type, String, [(Type, String)])
getFunDescFI fi = (functionReturnType fi, functionName fi, functionArgs fi)

getBinFunTypeInfo :: FunctionInfo -> (String, TypeInfo)
getBinFunTypeInfo fi = (functionName fi, (getFunTypeFI fi, binpos, bidepth))

getBinFunInfo :: FunctionInfo -> (String, FunctionInfo)
getBinFunInfo fi = (functionName fi, fi)

printIntFI = FunctionInfo {
  functionName       = "printInt",
  functionReturnType = typeVoid,
  functionArgs  = [(typeInt, "x")],
  functionArgsTypes  = [typeInt],
  functionDeclPos = binpos
}

printStringFI = FunctionInfo {
  functionName       = "printString",
  functionReturnType = typeVoid,
  functionArgs  = [(typeString, "s")],
  functionArgsTypes = [typeString],
  functionDeclPos = binpos
}

errorFI = FunctionInfo {
  functionName       = "error",
  functionReturnType = typeVoid,
  functionArgs  = [],
  functionArgsTypes = [],
  functionDeclPos = binpos
}

readIntFI = FunctionInfo {
  functionName       = "readInt",
  functionReturnType = typeInt,
  functionArgs  = [],
  functionArgsTypes = [],
  functionDeclPos = binpos
}

readStringFI = FunctionInfo {
  functionName       = "readString",
  functionReturnType = typeString,
  functionArgs  = [],
  functionArgsTypes = [],
  functionDeclPos = binpos
}

concatStringFI = FunctionInfo {
  functionName       = "concatString",
  functionReturnType = typeString,
  functionArgs  = [(typeString, "str1"), (typeString, "str2")],
  functionArgsTypes = [typeString, typeString],
  functionDeclPos = binpos
}

equalsStringFI = FunctionInfo {
  functionName       = "equalsString",
  functionReturnType = typeBool,
  functionArgs  = [(typeString, "str1"), (typeString, "str2")],
  functionArgsTypes = [typeString, typeString],
  functionDeclPos = binpos
}

--TODO think about it  - should malloc return type be a string?
mallocFI = FunctionInfo {
  functionName       = "malloc",
  functionReturnType = typeString,
  functionArgs  = [(typeInt, "x")],
  functionArgsTypes = [typeInt],
  functionDeclPos = binpos
}

builtIns = [printIntFI,  printStringFI, errorFI, readIntFI, readStringFI, concatStringFI, equalsStringFI, mallocFI]

builtInsTypeInfos = map getBinFunTypeInfo builtIns

builtInsDescs = map getFunDescFI builtIns

bultInsFunInfos = map getBinFunInfo builtIns

addBuiltInsToFunEnv :: FunEnv -> FunEnv
addBuiltInsToFunEnv = flip Map.union $ Map.fromList bultInsFunInfos

objectClassId = "NothingObject"
objectClassPId = PIdent(binpos, objectClassId)
objectClass = CTDCDef $ CCDef objectClassId objectSuperClass emptyBody
objectSuperClass = Nothing
objectClassInfo = defaultClassInfo {
  classDeclPos = binpos
}

builtInsClassesDefs = [objectClass]
builtInsClassEnv = [(objectClassId, objectClassInfo)]

addBuiltInsToClassEnv :: ClassEnv -> ClassEnv
addBuiltInsToClassEnv = flip Map.union $ Map.fromList builtInsClassEnv

thisId = "this"