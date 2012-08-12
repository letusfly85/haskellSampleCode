import Control.Monad.State

sampleState :: State Int Int
sampleState = do
    m <- get
    let n = 1 + m
    return n

runSample :: Int -> (Int, Int)
runSample n = runState sampleState n

sampleState2 :: State [String] [Int]
sampleState2 = do
    sList <- get
    let iList = foldr (\x ys -> x:ys) [] [0..10]
    put (map show iList ++ sList)
    return iList

runSample2 :: [String] -> ([Int],[String])
runSample2 [] = runState sampleState2 []
runSample2 list = runState sampleState2 list
