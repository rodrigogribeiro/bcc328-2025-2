module Markup.Tests.HtmlTests (myhtml) where


import Markup.Printer.Html

myhtml :: Html
myhtml =
  html_
    (title_ "My title")
    ((h_ 1 (txt_ "Heading"))    <>
     (p_ $ txt_ "Paragraph #1") <>
     (p_ $ txt_ "Paragraph #2"))
