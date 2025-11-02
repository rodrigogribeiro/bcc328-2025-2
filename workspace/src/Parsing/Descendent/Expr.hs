module Parsing.Descendent.Expr (gen) where

import Parsing.Descendent.Combinators

-- generalizing the construction of parser
-- for left recursive grammars

type Op s a = (s, a -> a -> a)

gen :: Eq s => [Op s a] -> Parser s a -> Parser s a
gen ops p = chainl p (choice (map f ops))
  where
    f (s,c) = const c <$> symbol s
