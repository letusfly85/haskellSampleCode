import Control.Monad.Writer


logger :: Int -> Writer [String] Int
logger x = writer (x, ["Got number: " ++ show x])

func = do a <- logger 3
          tell ["Tell the number."]
          return a

func2 n = do return $ logger n

sample :: Int -> Writer [String] [Int]
sample x = writer (foldr (\x ys -> x:ys) [] [0..x],["get number: " ++ show x])

func3 n = do return $ sample n

func4 [] acc     = do return acc
func4 (x:xs) acc = do tell [show x]
                      func4 xs (x:acc)

test4 list = runWriter $ func4 list []
