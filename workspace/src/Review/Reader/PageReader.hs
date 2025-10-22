import Review.Reader.SimpleReader
import Review.Reader.SimpleHtml 
import Prelude hiding (div)


main :: IO ()
main 
  = do 
    putStrLn "what is your email address?"
    email <- getLine
    print (runReader view email)

view :: Reader Email Html
view 
  = do 
      r <- page 
      pure $ div [ r ]

page :: Reader Email Html
page 
  = do
      r <- content 
      pure $ div [ topNav , r ]

topNav :: Html
topNav 
  = div [ 
          h1 [ "OurSite.com" ]
        ]

content :: Reader Email Html
content 
  = do 
      email <- ask
      r <- right 
      pure $ div [ h1 [ "Custom Content for " ++ email ]
                 , left
                 , r
                 ]

left :: Html
left 
  = div [ p [ "this is the left side" ] ]

right :: Reader Email Html
right 
  = do
      a <- article
      pure $ div [ a ]

article :: Reader Email Html
article  
  = do 
      w <- widget 
      pure $ div [ p [ "this is an article" ]
                 , w
                 ]

widget :: Reader Email Html
widget 
  = do
      email <- ask
      pure $ div [ 
                   p [ "Hey " ++ email ++ 
                       ", we've got a great offer for you!" 
                     ]
                 ]
