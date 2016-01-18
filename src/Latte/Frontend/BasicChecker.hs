module Latte.Frontend.BasicChecker (doBasicChecks) where

import Latte.BNFC.ErrM
import Latte.BNFC.AbsLatte
import Latte.Internal.Type
import Latte.Internal.ASTInternal

import Data.List

doBasicChecks :: Program -> Err ()
doBasicChecks prog = checkForMain prog

checkForMain :: Program -> Err ()
checkForMain (Program tdefs) =
  case (find (\tdef -> case tdef of
    TDFnDef _ (PIdent (_, name)) _ _ -> name == "main"
    _                                -> False) tdefs) of
      Just (TDFnDef rtype (PIdent (pos, _)) args _) ->
        if rtype /= typeInt
          then fail $ show pos ++ ":\nMain function of type different then int."
          else if args /= []
            then fail $ show pos ++ ":\nMain function has non-empty list of arguments."
            else return ()
      _ -> fail "No main() function found."
