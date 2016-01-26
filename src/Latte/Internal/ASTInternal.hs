module Latte.Internal.ASTInternal where

import Latte.BNFC.AbsLatte
import Latte.Internal.Type

type CIdent = String
type COperator = String

data CBlock = CBlock [CStmt]
  deriving (Eq, Ord, Show, Read)

data CItem = CINoInit CIdent | CIInit CIdent CTExpr
  deriving (Eq, Ord, Show, Read)

type CProgramInfo = (CProgram, ClassEnv, FunEnv)

data CProgram = CProgram [CTopDef]
  deriving (Eq, Ord, Show, Read)

data CFunDef = CFunDef Type CIdent [CArg] CBlock
  deriving (Eq, Ord, Show, Read)

data CTopDef = CTDFnDef CFunDef | CTDCDef CClassDef
  deriving (Eq, Ord, Show, Read)

data CClassDef = CCDef String (Maybe String) CClassBody
  deriving (Eq, Ord, Show, Read)

data CClassBody = CCBody [CFieldDecl]
  deriving (Eq, Ord, Show, Read)

emptyBody = CCBody []

data CFieldDecl = CCVar Type [CIdent]
  deriving (Eq, Ord, Show, Read)

data CArg = CArg Type CIdent
  deriving (Eq, Ord, Show, Read)

data CStmt
    = CSEmpty
    | CSBlock CBlock
    | CSDecl CDecl
    | CSAss CTExpr CTExpr
    | CSRet Type CTExpr
    | CSVRet
    | CSCondElse CTExpr CStmt CStmt
    | CSWhile CTExpr CStmt
    | CSRepeat CStmt
    | CSExp CTExpr
  deriving (Eq, Ord, Show, Read)

data CDecl = CDecl Type [CItem]
  deriving (Eq, Ord, Show, Read)

data CLiteral = CLInt Integer | CLBool Bool | CLString String
  deriving (Eq, Ord, Show, Read)

data CNew = CNClass CIdent
  deriving (Eq, Ord, Show, Read)

type CTExpr = (CExpr, Type)

data CExpr
    = CELit CLiteral
    | CEApp CIdent [CTExpr]
    | CEMet CTExpr Int [CTExpr]
    | CEDot CTExpr Integer CIdent
    | CEVar CIdent
    | CBinOp CTExpr COperator CTExpr
    | CENew CNew
    | CENull Type
  deriving (Eq, Ord, Show, Read)
