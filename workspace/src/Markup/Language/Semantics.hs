module Markup.Language.Semantics (semantics) where

-- Simple semantic analysis step for markup
-- Essentially, we glue together list elements
-- to reflect the correct meaning of a file

import Markup.Language.Syntax


semantics :: Document -> Document
semantics = foldr step []
  where
    step h@(Heading _ _) ac = h : ac
    step p@(Paragraph _) ac = p : ac
    step (CodeBlock s) ((CodeBlock s1) : ds)
      = (CodeBlock (s ++ s1)) : ds
    step c@(CodeBlock _) ac = c : ac
    step (UnorderedList s) ((UnorderedList s1) : ds)
      = (UnorderedList (s ++ s1)) : ds
    step c@(UnorderedList _) ac = c : ac
