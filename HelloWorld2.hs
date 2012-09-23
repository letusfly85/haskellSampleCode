{-# 
    LANGUAGE TypeFamilies,
    QuasiQuotes,
    TemplateHaskell,
    MultiParamTypeClasses,
    OverloadedStrings
 #-}
 
import Yesod

data Links = Links

mkYesod "Links" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
|]

instance Yesod Links

getHomeR  = defaultLayout [whamlet|$newline always
<a href=@{Page1R}>Got to page1!
|]

getPage1R = defaultLayout [whamlet|$newline always
<a href=@{Page2R}>Got to page2!
|]

getPage2R = defaultLayout [whamlet|$newline always
<a href=@{HomeR}>Go home!
|]

main = warpDebug 3000 Links
