
import Postfix.PostfixParser
import Postfix.PostfixSyntax
import System.Environment

-- definition of main function

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      content <- readFile file
      parseTest content
