module Latte.Main where

import Latte.BNFC.LexLatte
import Latte.BNFC.ParLatte
import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.BNFC.PrintLatte
import Latte.Frontend.BasicChecker

import Control.Monad.State
import Data.Map as Map
import System.IO

main :: IO ()
main = do
  interact compile
  putStrLn ""

compile :: String -> String
compile s =
  let p = pProgram (myLexer s)
  in case p of
    Ok prog -> case (doChecks prog) of
      Ok () -> show prog
      Bad m -> m
    Bad m -> m
