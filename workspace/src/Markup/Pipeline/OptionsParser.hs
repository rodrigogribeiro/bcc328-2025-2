module Markup.Pipeline.OptionsParser ( Options (..)
                                     , Input (..)
                                     , Output (..)
                                     , optionsParser
                                     ) where

import Data.Maybe (fromMaybe)
import Markup.Language.Env
import Options.Applicative

----------------------------------------------------
-- definition of the command line argument parser --
----------------------------------------------------

-- basic types for options 

data Input
  = Stdin              -- input from console
  | FileInput FilePath -- file name
  deriving Show 

data Output
  = Stdout                -- output to console 
  | FileOutput FilePath   -- file name
  deriving Show
 
data Options
  = Single Input Output
  | Directory FilePath FilePath Env 
  deriving Show

-- functions for defining the parser

optionsParser :: IO Options
optionsParser = execParser opts

opts :: ParserInfo Options
opts = info (pOptions <**> helper)
            (fullDesc                            <>
             header "BCC328 - Markdown compiler" <>
             progDesc "Simplified Markdown compiler")


pOptions :: Parser Options
pOptions
  = subparser (singleParser <> directoryParser)
    where
      singleParser
        = command "file"
                  (info (helper <*> pSingle)
                        (progDesc "Convert a single markdown file to HTML"))
      directoryParser
        = command "directory"
                  (info (helper <*> pDirectory)
                        (progDesc "Convert all directory md files to HTML"))

-- parsing files

pSingle :: Parser Options
pSingle =
  Single <$> pSingleInput <*> pSingleOutput

pSingleInput :: Parser Input
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser Output
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

pInputFile :: Parser Input
pInputFile = FileInput <$> parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

pOutputFile :: Parser Output
pOutputFile = FileOutput <$> parser
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )

-- parsing directories

pDirectory :: Parser Options
pDirectory =
  Directory <$> pInputDir <*> pOutputDir <*> pEnv

pInputDir :: Parser FilePath
pInputDir =
  strOption
    ( long "input"
      <> short 'i'
      <> metavar "DIRECTORY"
      <> help "Input directory"
    )

pOutputDir :: Parser FilePath
pOutputDir =
  strOption
    ( long "output"
      <> short 'o'
      <> metavar "DIRECTORY"
      <> help "Output directory"
    )

pEnv :: Parser Env
pEnv = fromMaybe defaultEnv <$> optional p
  where
    p =  Env <$> strOption
                 (  long "style"
                 <> short 'S'
                 <> metavar "FILE"
                 <> help "Stylesheet filename"
                 )
