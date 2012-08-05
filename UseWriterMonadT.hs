import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (liftM, forM_, when, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, execWriterT, runWriterT)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    tell [(path, length contents)]
    forM_ contents $ \name -> do
        let newName = path </> name
        isDir   <- liftIO . doesDirectoryExist $ newName
        when isDir $ countEntries newName

testCountEntries  path = execWriterT (countEntries path)
testCountEntries2 path = runWriterT  (countEntries path)

list = liftM (\x -> foldr (\(p,i) ys -> (p ++ "  " ++  show i):ys) [] x) $ testCountEntries "."

myRun = join $ liftM (\x -> mapM_ putStrLn x) list
