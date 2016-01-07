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
    | CSRepeat CStmt
    | CSExp CTExpr
  deriving (Eq, Ord, Show, Read)

type CTExpr = (CExpr, Type)

data CLiteral = CLInt Integer | CLBool Bool | CLString String
  deriving (Eq, Ord, Show, Read)

data CExpr
    = CEVar CIdent
    | CELit CLiteral
    | CEApp CIdent [CTExpr]
    | CBinOp CTExpr COperator CTExpr
  deriving (Eq, Ord, Show, Read)

isCTExprTrue :: CTExpr -> Bool
isCTExprTrue (CELit (CLBool val), _) = val
isCTExprTrue _ = False

isCTExprFalse :: CTExpr -> Bool
isCTExprFalse (CELit (CLBool val), _) = not val
isCTExprFalse _ = False