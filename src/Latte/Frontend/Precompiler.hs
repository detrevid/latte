module Latte.Frontend.Precompiler (precompile) where

import Latte.BNFC.ErrM
import Latte.BNFC.AbsLatte
import Latte.Frontend.BasicChecker
import Latte.Frontend.TypeChecker
import Latte.Internal.ASTInternal

precompile :: Program -> Err CProgramInfo
precompile prog = do
  doBasicChecks prog
  checkTypes prog