module Utils.Pretty ( Pretty(..)
                    , pretty
                    , module Text.PrettyPrint.HughesPJ
                    ) where

import Text.PrettyPrint.HughesPJ

-- definition of a type class for pretty-print

class Pretty a where
  ppr :: a -> Doc

pretty :: Pretty a => a -> String
pretty = render . ppr
