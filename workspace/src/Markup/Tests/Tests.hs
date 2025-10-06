import Markup.Language.Frontend
import Markup.Printer.Backend
import Markup.Printer.Html

import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile, ByteString)
import Data.String (fromString)
import Test.Tasty (defaultMain,TestTree,testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath (takeBaseName, replaceExtension)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
  testFiles <- findByExtension [".md"] "./src/Markup/Tests/inputs"
  return $ testGroup "Markdown compilation tests"
                     [
                       goldenVsString (takeBaseName file)
                                      goldenFile
                                      (pipeline <$> readFile file)
                     | file <- testFiles
                     , let goldenFile = replaceExtension file ".html"
                     ]

pipeline :: ByteString -> ByteString
pipeline content
  = case frontEnd $ show content of
      Left err -> fromString err
      Right ast -> fromString $ render $ backEnd (title_ "") ast

