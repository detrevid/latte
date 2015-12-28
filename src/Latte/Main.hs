module Latte.Main where

import Latte.BNFC.LexLatte
import Latte.BNFC.ParLatte
import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.BNFC.PrintLatte

import Control.Monad.State
import Data.Map as Map

main :: IO ()
main = do
  interact interpret
  putStrLn ""

interpret :: String -> String
interpret s =
  let p = pProgram (myLexer s)
  in case p of
    Ok prog -> show prog
    Bad m -> show m
