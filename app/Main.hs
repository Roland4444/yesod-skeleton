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
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)    
import Yesod
import Network.HTTP.Types (status200, hContentType)
import Network.Wai (responseBuilder)
import Text.Printf (printf)
import System.Directory (doesFileExist)

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

data Person = Person { name :: String, age :: Int }

defaultPort :: Int
defaultPort = 3000

-- Шаблоны как функции (просто и работает)
person1Template :: Person -> Html
person1Template person = [shamlet|
    <p>Hello, my name is #{name person} and I am #{show $ age person}.
    <p>
    Let's do some funny stuff with my name: #
    <b>#{sort $ map toLower (name person)}
    <p>Oh, and in 5 years I'll be #{show (age person + 5)} years old.
|]

person2Template :: Person -> Html
person2Template person = [shamlet|
    <p>Hello, my name is #{name person} and I am #{show $ age person}.
    <p>Let's do some funny stuff with my name: #
    <b>#{sort $ map toLower (name person)}
    <p>Oh, and in 5 years I'll be #{show (age person + 5)} years old.
|]

-- Упрощенная версия для вывода в консоль
renderTemplateToConsole :: (Person -> Html) -> String -> Person -> IO ()
renderTemplateToConsole template description person = do
    putStrLn $ "\n=== " ++ description ++ " ==="
    putStrLn $ renderHtml $ template person

main :: IO ()
main = do
    let person = Person "Michael" 26
    
    -- Первый шаблон
    renderTemplateToConsole person1Template "First Template" person
    
    putStrLn "*********************************************************"
    putStrLn "*********************************************************"
    putStrLn "***  ***************************************    ****  ***"
    putStrLn "***  *******                           *****  ** ***  ***"
    putStrLn $ printf "***  *******STARTUP SERVER AT PORT %-5d*****  *** **  ***" defaultPort
    putStrLn "***  *******                           *****  **** *  ***"
    putStrLn "***  ***************************************  *****   ***"
    putStrLn "***       **********************************  ******  ***"
    putStrLn "*********************************************************"

    -- Второй шаблон
    renderTemplateToConsole person2Template "Second Template" person

    warp defaultPort HelloWorld






-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE QuasiQuotes           #-}
-- {-# LANGUAGE TemplateHaskell       #-}
-- {-# LANGUAGE TypeFamilies          #-}
-- module Main where

-- import              Text.Hamlet                         (shamlet)

-- import              Text.Blaze.Html.Renderer.String     (renderHtml)

-- import              Data.Char                           (toLower)

-- import              Data.List                           (sort)    
-- import              Yesod
-- import              Network.HTTP.Types                  (status200, hContentType)
-- import              Network.HTTP.Types                  (status404, hContentType)

-- import              Network.Wai                         (responseBuilder)
-- import              Network.HTTP.Types                  (status200)

-- import              Data.ByteString.Lazy.Char8          (pack)
-- import qualified    Data.ByteString.Char8 as BS
-- import              Text.Printf                         (printf)  -- Добавьте этот импорт!


-- data HelloWorld = HelloWorld

-- mkYesod "HelloWorld" [parseRoutes|
-- / HomeR GET
-- /raw DirectR GET
-- -- /static StaticR Static getStatic
-- |]

-- instance Yesod HelloWorld

-- getHomeR :: Handler Html
-- getHomeR = defaultLayout [whamlet|Hello World!|]

-- getDirectR :: Handler TypedContent
-- getDirectR = sendWaiResponse $ responseBuilder status200 [("Content-Type", "text/plain")] "Hello RAW!"

-- data Person = Person{ name ::String, age ::Int}


-- defaultPort :: Int
-- defaultPort = 3000

-- main ::IO ()

-- main = do
    
--     let person = Person "Michael" 26   

    
--     putStrLn $ renderHtml [shamlet|
--         <p>Hello, my name is #{name person} and I am #{show $ age person}.
--         <p>
--         Let'sdo some funny stuff with my name: #
--         <b>#{sort $ map toLower (name person)}
--         <p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.
--     |]

--     putStrLn "*********************************************************"
--     putStrLn "*********************************************************"
--     putStrLn "***  ***************************************    ****  ***"
--     putStrLn "***  *******                           *****  ** ***  ***"
--     putStrLn $ printf "***  *******STARTUP SERVER AT PORT %-5d*****  *** **  ***" defaultPort
--     putStrLn "***  *******                           *****  **** *  ***"
--     putStrLn "***  ***************************************  *****   ***"
--     putStrLn "***       **********************************  ******  ***"
--     putStrLn "*********************************************************"

--     putStrLn $ renderHtml [shamlet|
--         <p>Hello, my name is #{name person} and I am #{show $ age person}.
--         <p>Let'sdo some funny stuff with my name: #
--         <b>#{sort $ map toLower (name person)}
--         <p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.
--     |]   

--     warp defaultPort HelloWorld













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