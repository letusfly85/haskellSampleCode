{-
- author : shun
- created: 2012/02/21
- updated: 2012/08/26
-
- detail : translate a format of table_name to CamelCase
-
-}

import Data.Char (toUpper,toLower)

toCamelCase :: String -> String
toCamelCase str = toCamelCase' $ map toLower str
toCamelCase' (x:y:lst) = toUpper x : helper (unscCheck y) lst
    where helper _ [] = []
          helper mc [x] =
             case (mc,x) of
                (mc, '_')        -> error "TODO"
                (Just c , x)     -> c:[x]
                (Nothing, x)     -> [toUpper x]

          helper mc [x,y]  =
             case (mc,x,y) of
                (mc, x, '_')     -> error "TODO"
                (Just c, '_', y) -> c:[toUpper y]
                (Just c, x,   y) -> c:[x,y]
                (Nothing,'_', y) -> [toUpper y]
                (Nothing, x,  y) -> [toUpper x,y]

          helper mc (x:y:lst) =
            case mc of
                Just c  -> c : helper (unscCheck x) (y:lst)
                Nothing -> toUpper x : helper (unscCheck y) lst

unscCheck :: Char -> Maybe Char
unscCheck c = case c of
      '_'   -> Nothing
      _     -> Just c

isNgString :: String -> Bool
isNgString list = if last list == '_' then False else True
