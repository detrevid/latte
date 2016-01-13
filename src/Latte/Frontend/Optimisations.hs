module Latte.Frontend.Optimisations (constantFolding, isCTExprTrue, isCTExprFalse) where

import Latte.BNFC.ErrM
import Latte.BNFC.PrintLatte
import Latte.Internal.BuiltIn
import Latte.Internal.Type
import Latte.Internal.ASTInternal

import Control.Applicative (Applicative)
import Control.Monad
import qualified Data.Map as Map
import Data.List

internalErrMsg :: String
internalErrMsg = "Internal error during optimisation phase.\n"

relOprs = ["<", "<=",  ">", ">=", "==", "!="]
relOprsFs :: Map.Map String (Integer -> Integer -> Bool)
relOprsFs = Map.fromList [
  ("<", (<)),
  ("<=", (<=)),
  (">", (>)),
  (">=", (>=)),
  ("==", (==)),
  ("!=", (/=))
  ]

artOprs = ["+", "-",  "*", "/", "%"]
artOprsFs :: Map.Map String (Integer -> Integer -> Integer)
artOprsFs = Map.fromList [
  ("+", (+)),
  ("-", (-)),
  ("*", (*)),
  ("/", quot),
  ("%", rem)
  ]

boolOprs = ["||", "&&", "==", "!=", "^"]
boolOprsFs :: Map.Map String (Bool -> Bool -> Bool)
boolOprsFs = Map.fromList [
  ("||", (||)),
  ("&&", (&&)),
  ("==", (==)),
  ("!=", (/=)),
  ("^", (\x y -> x /= y))
  ]

strOprs = ["+"]
strOprsFs :: Map.Map String (String -> String -> String)
strOprsFs = Map.fromList [
  ("+", (++))
  ]

strEqOprs = ["==", "!="]
strEqOprsFs = Map.fromList [
  ("==", (==)),
  ("!=", (/=))
  ]

constantFolding :: CTExpr -> Err CTExpr
constantFolding ctexpr@(CBinOp exp1 op exp2, t) = do
  exp1' <- constantFolding exp1
  exp2' <- constantFolding exp2
  case (exp1', exp2') of
    ((CELit lit1, tl1), (CELit lit2, tl2)) ->
      case (lit1, lit2) of
        (CLInt val1, CLInt val2) ->
          case (elem op relOprs, elem op artOprs) of
            (True, _) -> let opF = (relOprsFs Map.! op) in return (CELit $ CLBool $ opF val1 val2, typeBool)
            (_, True) -> let opF = (artOprsFs Map.! op) in return (CELit $ CLInt $ opF val1 val2, typeInt)
            _         -> fail $ internalErrMsg
        (CLBool val1, CLBool val2) ->
          if (elem op boolOprs)
            then let opF = (boolOprsFs Map.! op) in return (CELit $ CLBool $ opF val1 val2, typeBool)
            else fail $ internalErrMsg
        (CLString str1, CLString str2) ->
          case (elem op strOprs, elem op strEqOprs) of
            (True, _) -> let opF = (strOprsFs Map.! op) in return (CELit $ CLString $ opF str1 str2, typeString)
            (_, True) -> let opF = (strEqOprsFs Map.! op) in return (CELit $ CLBool $ opF str1 str2, typeBool)
            _         -> fail $ internalErrMsg
        _ -> fail $ internalErrMsg
    _ -> return ctexpr
constantFolding x = return x

isCTExprTrue :: CTExpr -> Bool
isCTExprTrue (CELit (CLBool val), _) = val
isCTExprTrue _ = False

isCTExprFalse :: CTExpr -> Bool
isCTExprFalse (CELit (CLBool val), _) = not val
isCTExprFalse _ = False
