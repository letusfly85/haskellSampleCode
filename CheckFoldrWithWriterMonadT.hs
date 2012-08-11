import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (liftM, forM_, when, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, execWriterT, runWriterT)

pile :: Int -> IO [String]
pile n =  return $
    foldr (\x ys -> show x:ys) [] [0..n]

simpleEntries :: Int -> WriterT [(Int,String)] IO ()
simpleEntries n = do
    tell [(n, "The number is " ++ show n)]
    return ()

pileEntries :: [Int] -> WriterT [(Int,String)] IO ()
pileEntries [] = return ()
pileEntries (x:xs) = do
    tell [(x, "Pile Entries: " ++ show x)]
    pileEntries xs

runner n  = runWriterT  $ simpleEntries n
runnerP n = execWriterT $ pileEntries [0..n] 
