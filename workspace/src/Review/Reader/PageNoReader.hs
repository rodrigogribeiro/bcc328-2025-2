import Review.Reader.SimpleHtml 
import Prelude hiding (div)

main :: IO ()
main 
  = do 
    putStrLn "what is your email address?"
    email <- getLine
    print (view email)

view :: Email -> Html
view email 
  = div [ page email ]

page :: Email -> Html
page email 
  = div [ topNav
        , content email
        ]

topNav :: Html
topNav 
  = div [ 
          h1 [ "OurSite.com" ]
        ]

content :: Email -> Html
content email 
  = div [ h1 [ "Custom Content for " ++ email ]
        , left
        , right email
        ]

left :: Html
left 
  = div [ p [ "this is the left side" ] ]

right :: Email -> Html
right email 
  = div [ article email ]

article :: Email -> Html
article email 
  = div [ p [ "this is an article" ]
        , widget email
        ]

widget :: Email -> Html
widget email 
  = div [ 
      p [ "Hey " ++ email ++ ", we've got a great offer for you!" ]
    ]
