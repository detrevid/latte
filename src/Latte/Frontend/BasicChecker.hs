module Latte.Frontend.BasicChecker (doBasicChecks) where

import Latte.BNFC.ErrM
import Latte.BNFC.AbsLatte
import Latte.Internal.Type

import Control.Monad (when)
import Data.List

doBasicChecks :: Program -> Err ()
doBasicChecks prog = checkForMain prog

checkForMain :: Program -> Err ()
checkForMain (Program tdefs) =
  case (find (\tdef -> case tdef of
    TDFnDef (FunDef _ (PIdent (_, name)) _ _) -> name == "main"
    _                                         -> False) tdefs) of
      Just (TDFnDef (FunDef rtype (PIdent (pos, _)) args _)) -> do
        when (rtype /= typeInt) $ fail $ show pos ++ ":\nMain function of type different then int."
        when (args /= []) $ fail $ show pos ++ ":\nMain function has non-empty list of arguments."
        return ()
      _ -> fail "No main() function found."
