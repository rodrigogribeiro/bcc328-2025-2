module Markup.Language.Frontend (frontEnd) where

import Markup.Language.Syntax
import Markup.Language.Parser
import Markup.Language.Semantics

-- Markup compiler frontend

frontEnd :: String -> Either String Document
frontEnd
  = either Left (Right . semantics) . parseDocument
