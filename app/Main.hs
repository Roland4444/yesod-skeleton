--https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/basics-of-yesod/yesod-1
--https://hackage.haskell.org/package/yesod-0.4.1/docs/Yesod-Yesod.html
--https://www.yesodweb.com/book/widgets
--https://www.programmer-books.com/wp-content/uploads/2019/04/Haskell-High-Performance-Programming.pdf
--https://github.com/yesodweb/yesod/wiki/Powered-by-Yesod
--https://www.yesodweb.com/book
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where
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

main :: IO ()
main = warp 3000 HelloWorld
