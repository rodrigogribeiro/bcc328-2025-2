module Markup.Printer.Backend (backEnd) where

import qualified Markup.Language.Syntax as AST
import qualified Markup.Printer.Html as HTML

-- definition of the backend

backEnd :: HTML.Head -> AST.Document -> HTML.Html
backEnd title = HTML.html_ title . translate

translate :: AST.Document -> HTML.Structure
translate = foldMap translateStructure

translateStructure :: AST.Structure -> HTML.Structure
translateStructure (AST.Heading n txt)
  = HTML.h_ n $ HTML.txt_ txt
translateStructure (AST.Paragraph p)
  = HTML.p_ $ HTML.txt_ p
translateStructure (AST.UnorderedList ds)
  = HTML.ul_ $ map (HTML.p_ . HTML.txt_) ds
translateStructure (AST.CodeBlock ds)
  = HTML.code_ (unlines ds)

