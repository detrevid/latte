module Latte.Main where

import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM
import Latte.Frontend.Precompiler
import Latte.Backend.Compiler
import Latte.MainH

import System.Environment
import System.FilePath
import System.Process (callCommand)

import qualified LLVM.General.AST as AST

compileProg' :: String -> Program -> Err AST.Module
compileProg' name prog = do
  cprog <- precompile prog
  compileCProgram name cprog

compileProg :: String -> Program -> IO (Err String)
compileProg name prog =
  case (fmap compileModuleToLLVM (compileProg' name prog)) of
    Ok io -> io
    Bad m -> return $ Bad m

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
