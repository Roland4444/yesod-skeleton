--https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/basics-of-yesod/yesod-1
--https://hackage.haskell.org/package/yesod-0.4.1/docs/Yesod-Yesod.html
--https://www.yesodweb.com/book/widgets
--https://www.programmer-books.com/wp-content/uploads/2019/04/Haskell-High-Performance-Programming.pdf
--https://github.com/yesodweb/yesod/wiki/Powered-by-Yesod
--https://www.yesodweb.com/book



-- LINK:: https://apprize.best/programming/haskell/5.html
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder) -- Изменено!
import Data.Char (toLower)
import Data.List (sort)
import Yesod
import Network.HTTP.Types (status200, status404, hContentType)
import Network.Wai (responseBuilder)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Blaze.ByteString.Builder (toByteString) -- Добавлено!
import Text.Blaze.Html.Renderer.Utf8'

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/raw DirectR GET
/isr IsraelR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getDirectR :: Handler TypedContent
getDirectR = sendWaiResponse $ 
    responseBuilder status200 [("Content-Type", "text/plain")] "Hello RAW!"

getIsraelR :: Handler TypedContent
getIsraelR = do
    let person2 = Person "Michael" 26
    let htmlBuilder = renderHtmlBuilder [shamlet|
<p>Hello, my name is #{name person2} and I am #{show $ age person2}.
<p>
    Let's do some funny stuff with my name: #
<b>#{sort $ map toLower (name person2)}
<p>Oh, and in 5 years I'll be #{show ((+) 5 (age person2))} years old.
|]
    sendWaiResponse $ responseBuilder status200 
        [("Content-Type", "text/html")] htmlBuilder -- Изменено на text/html!

data Person = Person { name :: String, age :: Int }

main :: IO ()
main = do
    let person = Person "Michael" 26
    
    putStrLn $ renderHtml [shamlet|
<p>Hello, my name is #{name person} and I am #{show $ age person}.
<p>Let's do some funny stuff with my name: #
<b>#{sort $ map toLower (name person)}
<p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.
|]

    putStrLn "*********************************************************"
    putStrLn "***  STARTUP SERVER AT PORT 3000                    ***"
    putStrLn "*********************************************************"
    
    warp 3000 HelloWorld




------CHECK

-- {-# LANGUAGE QuasiQuotes #-}
-- import Text.Hamlet (shamletFile)
-- import Text.Blaze.Html.Renderer.String (renderHtml)
-- import Data.Char (toLower)
-- import Data.List (sort)

-- data Person = Person
--     { name :: String
--     , age  :: Int
--     }

-- main :: IO ()
-- main = do
--     let person = Person "Michael" 26
--     html <- shamletFile "template.hamlet" person
--     putStrLn $ renderHtml html


------


-- <p>Hello, my name is #{name person} and I am #{show $ age person}.
-- <p>
--     Let's do some funny stuff with my name: #
--     <b>#{sort $ map toLower (name person)}
-- <p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.