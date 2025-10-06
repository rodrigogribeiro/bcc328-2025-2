import Control.Monad.Trans
import qualified Exp.Backend.RiscV.ExpCodegen as V
import qualified Exp.Backend.WASM.ExpCodegen as W
import qualified Exp.Frontend.Parser.Happy.ExpParser as H
import qualified Exp.Frontend.Parser.Megaparsec.ExpParser as M
import Exp.Frontend.Syntax.ExpSyntax
import Exp.Interp.ExpInterp
import Options.Applicative
import System.Console.Haskeline
import System.FilePath
import System.Process

-- command line argument parser

data Option
  = Compiler Frontend Backend FilePath
  | Repl

data Frontend
  = Recursive
  | LALR

data Backend
  = WASM
  | RiscV

compilerP :: Parser Option
compilerP = Compiler <$> frontEndP <*> backEndP <*> fileOptionP

frontEndP :: Parser Frontend
frontEndP
  = recursiveP <|> lalrP
    where
      recursiveP = flag' Recursive
                      (  long "recursive"
                      <> help "Uses a recursive descendent parser.")
      lalrP = flag' LALR
                 (  long "lalr"
                 <> help "Uses a LALR parser.")

backEndP :: Parser Backend
backEndP
  = wasmP <|> riscvP
  where
    wasmP = flag' WASM
               (  long "wasm"
               <> help "Uses the wasm backend.")
    riscvP = flag' RiscV
                (  long "riscv"
                <> help "Uses the riscv backend.")

fileOptionP :: Parser FilePath
fileOptionP = strOption (  long "file"
                        <> short 'f'
                        <> metavar "FILENAME"
                        <> help "Option file" )

replP :: Parser Option
replP = flag' Repl
        (  long "repl"
        <> help "Starts the interpreter repl")

input :: Parser Option
input = compilerP <|> replP

opts :: ParserInfo Option
opts = info input
       (fullDesc
       <> header "Arithmetic Expression interpreter and compiler")

-- repl definition

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "exp> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just "quit" -> outputStrLn "Goodbye."
      Just inp -> do
        res <- liftIO $ frontend Recursive inp
        case res of
          Left err -> outputStrLn err >> loop
          Right e  -> outputStrLn (show (eval e)) >> loop

-- compiler frontend

frontend :: Frontend -> String -> IO (Either String Exp)
frontend Recursive content
  = pure (M.expParser content)
frontend LALR content
  = H.expParser content

-- compiler backend

backend :: FilePath -> Backend -> Exp -> IO ()
backend file WASM ast
  = do
      let watCode = W.compileExpression ast
          watFile = file -<.> "wat"
      writeFile watFile watCode
backend file RiscV ast
  = do
      let riscVCode = V.compileExpression ast
          riscVFile = file -<.> "s"
      writeFile riscVFile riscVCode
      runLinker riscVFile

runLinker :: FilePath -> IO ()
runLinker asm
  = do
      let asmobj = asm -<.> "o"
          exec = dropExtension asm
          optsasm = ["-o", asmobj, asm]
          optslink = ["-static", "-o", exec, asmobj, runtime]
      _ <- callCommand $ unwords ("riscv64-linux-gnu-as" : optsasm)
      _ <- callCommand $ unwords ("riscv64-linux-gnu-gcc" : optslink)
      pure ()

runtime :: String
runtime = "./src/Exp/Runtime/RiscV/runtime.o"

-- main expression function.

startPipeline :: Option -> IO ()
startPipeline (Compiler fend bend fname)
  = do
      content <- readFile fname
      res <- frontend fend content
      case res of
        Left err -> putStrLn err
        Right e  -> backend fname bend e
startPipeline Repl
  = repl

-- definition of main function

main :: IO ()
main = execParser opts >>= startPipeline
