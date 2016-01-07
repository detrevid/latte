module Latte.Internal.ASTInternal where

import Latte.BNFC.AbsLatte

type CIdent = String
type COperator = String

data CBlock = CBlock [CStmt]
  deriving (Eq, Ord, Show, Read)

data CItem = CINoInit CIdent | CIInit CIdent CTExpr
  deriving (Eq, Ord, Show, Read)

data CProgram = CProgram [CTopDef]
  deriving (Eq, Ord, Show, Read)

data CTopDef = CTDFnDef Type CIdent [CArg] CBlock
  deriving (Eq, Ord, Show, Read)

data CArg = CArg Type CIdent
  deriving (Eq, Ord, Show, Read)

data CStmt
    = CSEmpty
    | CSBlock CBlock
    | CSDecl Type [CItem]
    | CSAss CIdent CTExpr
    | CSRet CTExpr
    | CSVRet
    | CSCondElse CTExpr CStmt CStmt
    | CSWhile CTExpr CStmt
    | CSExp CTExpr
  deriving (Eq, Ord, Show, Read)

type CTExpr = (CExpr, Type)

data CExpr
    = CEVar CIdent
    | CELit Literal
    | CEApp CIdent [CTExpr]
    | CBinOp CTExpr COperator CTExpr
  deriving (Eq, Ord, Show, Read)