module Markup.Language.Env ( Env (..)
                           , defaultEnv
                           ) where

-- definition of a simple environment

data Env
  = Env {
      stylePath :: FilePath -- path of css file
    } deriving Show

defaultEnv :: Env
defaultEnv = Env "" -- no css file
