module Markup.Language.Syntax ( Document
                              , Structure (..)
                              ) where

import Numeric.Natural


type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String] 
  | CodeBlock [String]
  deriving Show
