module Latte.Frontend.BasicChecker where

import Latte.BNFC.ErrM
import Latte.BNFC.AbsLatte
import Latte.Frontend.TypeChecker
import Latte.Internal.Type

import Data.List

doChecks :: Program -> Err ()
doChecks prog = do
  checkTypes prog
  checkForMain prog

checkForMain :: Program -> Err ()
checkForMain (Program tdefs) =
  case (find (\tdef -> case tdef of
    TDFnDef _ (PIdent (_, name)) _ _ -> name == "main") tdefs) of
      Nothing -> fail "No main function found"
      Just (TDFnDef rtype (PIdent (pos, _)) args _) ->
        if rtype /= typeInt
          then fail $ show pos ++ ":\nMain function of type different then int"
          else if args /= []
            then fail $ show pos ++ ":\nMain function has non-empty list of arguments"
            else return ()
