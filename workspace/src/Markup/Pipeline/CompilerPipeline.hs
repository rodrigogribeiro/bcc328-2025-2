module Markup.Pipeline.CompilerPipeline ( createHandles
                                        , startPipeline
                                        , optionsParser
                                        , hClose
                                        ) where

import Control.Monad
import Control.Monad.Reader
import Data.List (partition)
import Markup.Printer.Html (Head, render, title_, stylesheet_)
import Markup.Language.Env
import Markup.Language.Frontend
import Markup.Printer.Backend
import Markup.Pipeline.OptionsParser
import System.Directory ( createDirectory
                        , removeDirectoryRecursive
                        , listDirectory
                        , doesDirectoryExist
                        , doesFileExist
                        , copyFile
                        )
import System.FilePath ( takeExtension
                       , takeBaseName
                       , (<.>)
                       , (</>)
                       , takeFileName
                       )
import System.Exit (exitFailure, exitSuccess)
import System.IO

---------------------------------------------------
-- Creating handles                              --
---------------------------------------------------

createHandles :: Input -> Output -> IO (String, Handle, Handle)
createHandles inp out
  = do
      (title, inpHandle) <- createInputHandle inp
      outHandle <- createOutputHandle out
      return (title, inpHandle, outHandle)

createInputHandle :: Input -> IO (String, Handle)
createInputHandle Stdin = return ("", stdin)
createInputHandle (FileInput path)
  = (,) path <$> openFile path ReadMode

confirmOverwrite :: FilePath -> IO Bool
confirmOverwrite path
  = do
      putStrLn $ "\n" ++ path ++ "\nexists. Confirm overwrite?"
      answer <- getLine
      case answer of
        "y" -> return True
        "n" -> return False
        _   -> do
                 putStrLn "Invalid answer! Please type y or n."
                 confirmOverwrite path

createOutputHandle :: Output -> IO Handle
createOutputHandle Stdout = return stdout
createOutputHandle (FileOutput path)
  = do
      exists <- doesFileExist path
      canWrite <- if exists then confirmOverwrite path
                  else return True
      if canWrite then openFile path WriteMode
        else exitSuccess

---------------------------------------------------
-- compilation pipeline                          --
---------------------------------------------------

startPipeline :: IO ()
startPipeline
  = do
      options <- optionsParser
      case options of
        Single inp out ->
          runCompilerM defaultEnv (filePipeline False inp out)
        Directory inpDir outDir env ->
          runCompilerM env (directoryPipeline inpDir outDir)

-- definition of the compilation pipeline monad

type CompilerM a = (ReaderT Env IO) a

runCompilerM :: Env -> CompilerM a -> IO a
runCompilerM env m = runReaderT m env

filePipeline :: Bool -> Input -> Output -> CompilerM ()
filePipeline dirMode inpFile outFile
  = do
      env <- ask
      progressMessage dirMode inpFile
      (title,inpHandle,outHandle) <- liftIO $ createHandles inpFile outFile
      content <- liftIO $ hGetContents inpHandle
      let header = title_ title <> stylesheet_ (stylePath env)
      res <- pipeline header content
      writeAndCloseHandles res inpHandle outHandle

writeAndCloseHandles :: String -> Handle -> Handle -> CompilerM ()
writeAndCloseHandles res inpHandle outHandle
  = liftIO $ do
      hPutStrLn outHandle res
      hClose inpHandle
      hClose outHandle

progressMessage :: Bool -> Input -> CompilerM ()
progressMessage True (FileInput file)
  = liftIO $ putStrLn $ "Processing file " ++ file
progressMessage _ _ = return ()

pipeline :: Head -> String -> CompilerM String
pipeline title content
  = case frontEnd content of
      Left err -> liftIO $ putStrLn err >> exitFailure
      Right ast -> return $ render $ backEnd title ast

---------------------------------------------------
-- Directory processing functions                --
---------------------------------------------------

data DirContents
  = DirContents {
       filesToCompile :: [(FilePath, FilePath)]
     , filesToCopy :: [FilePath]
    }

directoryContents :: FilePath -> CompilerM DirContents
directoryContents inputDir
  = do
       files <- map (inputDir </>) <$> listDirectoryM inputDir
       let
         select = (== ".md") . takeExtension
         (mdFiles, otherFiles) = partition select files
         htmlFiles = map (\ f -> takeBaseName f <.> "html") mdFiles
       return $ DirContents (zip mdFiles htmlFiles) otherFiles

directoryPipeline :: FilePath -> FilePath -> CompilerM ()
directoryPipeline inputDir outputDir
  = do
      d <- directoryContents inputDir
      let files = filesToCompile d
          otherFiles = filesToCopy d
      let entries = map (\ (i,o) -> (FileInput i, FileOutput o)) files
      shouldContinue <- createOutputDirectory outputDir
      liftIO $ unless shouldContinue (hPutStrLn stderr "Cancelled." *> exitFailure)
      mapM_ (uncurry (filePipeline True)) entries
      let copy file = liftIO $ copyFile file (outputDir </> takeFileName file)
      mapM_ copy otherFiles
      liftIO $ putStrLn "Done."


createOutputDirectory :: FilePath -> CompilerM Bool
createOutputDirectory outDir
  = do
      dirExists <- doesDirectoryExistM outDir
      shouldCreate <-
        if dirExists then do
          override <- confirmOverwriteM outDir
          whenM override (removeDirectoryRecursive outDir)
          return override
        else return True
      whenM shouldCreate (createDirectory outDir)
      return shouldCreate

-- lifting IO operations to compilerM

doesDirectoryExistM :: FilePath -> CompilerM Bool
doesDirectoryExistM p = liftIO (doesDirectoryExist p)

confirmOverwriteM :: FilePath -> CompilerM Bool
confirmOverwriteM p = liftIO (confirmOverwrite p)

whenM :: Bool -> IO () -> CompilerM ()
whenM True  m = liftIO m
whenM _ _ = return ()

listDirectoryM :: FilePath -> CompilerM [FilePath]
listDirectoryM p = liftIO $ listDirectory p
