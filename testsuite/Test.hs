import System.Exit( ExitCode( ExitSuccess ) )
import System.Process (system)

main = do
  ExitSuccess <- system "./testsuite/runtests.sh"
  return ()
