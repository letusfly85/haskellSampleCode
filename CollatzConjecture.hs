{-
 - author: wada shunsuke
 - create: 28.06.2012
 - update: 28.06.2012
 -}

import Data.Bits ((.&.))

collatz :: Int -> IO ()
collatz n = do
	if ((.&.) n 1) == 0 then
	   do collatz (3 * n + 1)
	else
	   do collatz (n `div` 2)
