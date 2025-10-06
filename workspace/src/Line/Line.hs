import qualified Line.Backend.RiscV.LineCodegen as V
import qualified Line.Backend.WASM.LineCodegen as W
import qualified Line.Frontend.Parser.LineParser as H
import Line.Frontend.Syntax.LineSyntax
import Line.Interp.LineInterp
import Options.Applicative
import System.FilePath
import System.Process

-- command line argument parser

data Option
  = Option Exec FilePath

data Exec
  = Back Backend | Interp

data Backend
  = WASM | RiscV

compilerP :: Parser Option
compilerP = Option <$> execP <*> fileOptionP

fileOptionP :: Parser FilePath
fileOptionP = strOption (  long "file"
                        <> short 'f'
                        <> metavar "FILENAME"
                        <> help "Option file" )

execP :: Parser Exec
execP = flag' (Back WASM)
                (  long "wasm"
                <> help "Use the WASM backend")
           <|>
           flag' (Back RiscV)
                 ( long "riscv"
                 <> help "Use the riscv backend")
           <|>
           flag' Interp
                 (  long "interp"
                 <> help "Use the interpreter")

input :: Parser Option
input = compilerP

opts :: ParserInfo Option
opts = info input
       (fullDesc
       <> header "Arithmetic Lineression interpreter and compiler")

-- compiler frontend

frontend :: FilePath -> IO (Either String Line)
frontend path
  = do
      content <- readFile path
      H.lineParser content

-- compiler backend

backend :: FilePath -> Backend -> Line -> IO ()
backend file WASM ast
  = do
      let result = W.compileLine ast
          watFile = file -<.> "wat"
      case result of 
        Left err -> putStrLn err 
        Right watCode -> writeFile watFile watCode
backend file RiscV ast
  = do
      let riscVCode = V.compileLine ast
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
runtime = "./src/Line/Runtime/RiscV/runtime.o"


-- main expression function.

startPipeline :: Option -> IO ()
startPipeline (Option opt fname)
  = do
      res <- frontend fname
      case res of
        Left err -> putStrLn err
        Right ast  ->
          case opt of
            Interp -> interp ast
            Back bend -> backend fname bend ast

-- definition of main function

main :: IO ()
main = execParser opts >>= startPipeline
