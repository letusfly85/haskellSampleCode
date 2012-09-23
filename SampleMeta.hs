{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

func list = mapM_ putStrLn $ map show list

myFunc list = $([|$(varE $ mkName "func") list|])
