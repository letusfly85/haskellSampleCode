{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, FilePath, concatMap)
import Filesystem.Path.CurrentOS (FilePath, decodeString)
import Text.XML (Node)
import Text.XML.Cursor
import qualified Text.HTML.DOM as H
import qualified Text.XML.Cursor.Generic as G
import qualified Data.Text as T

main :: IO ()
main = do
    input <- H.readFile (decodeString "target.xml")
    let cursor = fromDocument input
    print $ getTargetNodeInfo cursor

getTargetNodeInfo c = do
    c $// element "html"
      &// element "body" >=> attributeIs "name" "xxx"
      &// element "textarea"
      &// element "p"
      >=> hasAttribute "style"
      >=> attribute "style"
