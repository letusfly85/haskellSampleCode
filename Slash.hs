{-# 
    LANGUAGE TypeFamilies,
    QuasiQuotes,
    TemplateHaskell,
    MultiParamTypeClasses,
    OverloadedStrings
 #-}
 
import Yesod
import Network.HTTP.Types (encodePath)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Arrow ((***))
import Data.Monoid (mappend)

data Slash = Slash

mkYesod "Slash" [parseRoutes|
/ RootR GET
/foo FooR GET
|]

instance Yesod Slash where
    joinPath _ ar pieces' qs' =
        fromText ar `mappend` encodePath pieces qs
        where
            qs = map (TE.encodeUtf8 *** go) qs'
            go "" = Nothing
            go x  = Just $ TE.encodeUtf8 x
            pieces = pieces' ++ [""]

    cleanPath _ [] = Right []
    cleanPath _ s
        | dropWhile (not . T.null) s == [""] = Right $ init s
        | otherwise = Left $ filter (not . T.null) s

getRootR = defaultLayout [whamlet|$newline always
<p><a href=@{RootR}>RootR
<p><a href=@{FooR}>FooR
|]

getFooR = getRootR

main = warpDebug 3000 Slash
