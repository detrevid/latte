module Latte.MainH where

import Latte.BNFC.LexLatte
import Latte.BNFC.ParLatte
import Latte.BNFC.AbsLatte
import Latte.BNFC.ErrM

import Control.Monad
import System.Environment
import System.FilePath
import System.IO

fileExtension = ".lat"

parseFilePath :: String -> IO String
parseFilePath filePath = do
  when (takeExtension filePath /= fileExtension) (fail "BAD FILE EXTENSION")
  return $ dropExtension filePath

compileToAssemblerFile :: (String -> Program -> IO (Err String)) -> String -> String -> IO String
compileToAssemblerFile compiler filePath extension = do
  path <- parseFilePath filePath
  let name = takeFileName path
  let outputFile = addExtension path extension
  withFile filePath ReadMode (\handle -> do
    contents <- hGetContents handle
    compiledProgram <- compile compiler name contents
    case compiledProgram of
      Ok compiled -> do
        hPutStrLn stderr ("OK\n")
        writeFile outputFile compiled
      Bad m -> do
        hPutStrLn stderr ("ERROR\n" ++ m)
        fail ""
    )
  return outputFile

compile :: (String -> Program -> IO (Err String)) -> String -> String -> IO (Err String)
compile compiler fileName code  =
  let p = pProgram (myLexer code)
  in case p of
    Ok prog -> compiler fileName prog
    Bad m -> return $ Bad m

mainH :: (String -> Program -> IO (Err String)) -> String -> (String -> IO ()) -> IO ()
mainH compiler extension generateObjectCode = do
  args <- getArgs
  when (length args /= 1) (fail "BAD NUMBER OF ARGUMENTS")
  let filePath = args !! 0
  path <- parseFilePath filePath
  assemblerPath <- compileToAssemblerFile compiler filePath extension
  putStrLn $ "Generated: " ++ assemblerPath
  generateObjectCode assemblerPath