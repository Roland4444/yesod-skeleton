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

import              Text.Hamlet                         (shamlet)

import              Text.Blaze.Html.Renderer.String     (renderHtml)

import              Data.Char                           (toLower)

import              Data.List                           (sort)    
import              Yesod
import              Network.HTTP.Types                  (status200, hContentType)
import              Network.HTTP.Types                  (status404, hContentType)

import              Network.Wai                         (responseBuilder)
import              Network.HTTP.Types                  (status200)

import              Data.ByteString.Lazy.Char8          (pack)
import qualified    Data.ByteString.Char8 as BS

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/raw DirectR GET
-- /static StaticR Static getStatic
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getDirectR :: Handler TypedContent
getDirectR = sendWaiResponse $ responseBuilder status200 [("Content-Type", "text/plain")] "Hello RAW!"

data Person = Person{ name ::String, age ::Int}

main ::IO ()

main =putStrLn $ renderHtml [shamlet|

<p>Hello, my name is #{name person} and I am #{show $ age person}.

<p>

Let'sdo some funny stuff with my name: #

<b>#{sort $ map toLower (name person)}

<p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.

|] where person =Person "Michael" 26
    -- putStrLn "*********************************************************"
    -- putStrLn "*********************************************************"
    -- putStrLn "***  ***************************************    ****  ***"
    -- putStrLn "***  *******                           *****  ** ***  ***"
    -- putStrLn "***  *******STARTUP SERVER AT PORT 3000*****  *** **  ***"
    -- putStrLn "***  *******                           *****  **** *  ***"
    -- putStrLn "***  ***************************************  *****   ***"
    -- putStrLn "***       **********************************  ******  ***"
    -- putStrLn "*********************************************************"


    -- putStrLn $ renderHtml [shamlet|

    --         <p>Hello, my name is #{name person} and I am #{show $ age person}.
    --         <p>Let'sdo some funny stuff with my name: #
    --         <b>#{sort $ map toLower (name person)}
    --         <p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.
    -- |]    where person =Person "Michael" 26

    -- warp 3000 HelloWorld




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