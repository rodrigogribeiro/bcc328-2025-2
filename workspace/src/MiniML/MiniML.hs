import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

import MiniML.Frontend.Parser.ExpParser (parser)
import MiniML.Frontend.Pretty.PrettyPrint
import MiniML.Frontend.TypeInference.Inference
import MiniML.Frontend.TypeInference.SolverMonad
import MiniML.Frontend.Syntax.Type

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "MiniML> "
      case minput of
        Nothing -> outputStrLn "Goodbye!"
        Just ":q" -> outputStrLn "Goodbye!"
        Just input -> do
          liftIO $ process input
          loop

process :: String -> IO ()
process input = do
  case parser input of
    Left err -> putStrLn $ "Parser Error:\n" ++ err
    Right ast -> do
      case infer ast of
        Left err -> putStrLn $ "Type Error: " ++ err
        Right (c, env, ty) -> do
          let s = subst env
              infs = infered env
          putStrLn "--- Generated Constraint ---"
          putStrLn $ pretty c
          putStrLn "--- Logs ---"
          putStrLn $ unlines $ reverse (logs env)
          putStrLn "--- Inference Result ---"
          putStrLn $ "Substition: " ++ pretty s
          putStrLn $ "Main Expression Type: " ++ pretty (apply s ty)
          if not (null infs)
            then do
              putStrLn "\nGlobal Bindings:"
              mapM_ (\(n, sch) -> putStrLn $ "  " ++ n ++ " : " ++ pretty sch) infs
            else return ()
