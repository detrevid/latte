module Latte.Main where

import Latte.BNFC.LexLatte
import Latte.BNFC.ParLatte
import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.BNFC.PrintLatte
import Latte.Frontend.BasicChecker
import Latte.Backend.Compiler
import Latte.Backend.Compiler
import Latte.MainH

import Control.Monad.State
import Data.Map as Map
import System.IO
import System.IO.Unsafe
import Control.Monad
import System.Environment
import System.FilePath
import System.IO
import System.Process (callCommand)

import qualified LLVM.General.AST as AST

compileProg' :: String -> Program -> Err AST.Module
compileProg' name prog = do
  doChecks prog
  compileProgram name prog

compileProg :: String -> Program -> IO String
compileProg name prog = case (compileProg' name prog) of
  Ok mod -> compileModuleToLLVM $ mod
  Bad m -> fail m

compileLLFile :: String -> IO ()
compileLLFile filePath = do
  progName <- getProgName
  let libDirectory = (takeDirectory progName) ++ "/lib/"
      fileBC = addExtension (dropExtension filePath) ".bc"
  callCommand $ "llvm-as -o " ++ fileBC ++ " " ++ filePath
  callCommand $ "llvm-link -o " ++ fileBC ++ " " ++  fileBC ++ " " ++ libDirectory ++ "runtime.bc"
  putStrLn $ "Generated: " ++ fileBC ++ " "

main :: IO ()
main = mainH compileProg "ll" compileLLFile
