import System.IO
-- import Data.Conduit
-- import Data.Conduit.Binary
-- import Control.Monad.ST
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class (lift)

-- main = runResourceT $ sourceFile "input.txt" $$ sinkFile "output.txt"

func = runResourceT $ do
    (releaseO, output) <- allocate (openFile "output.txt" WriteMode) hClose
    (releaseI, input ) <- allocate (openFile "input.txt"  ReadMode ) hClose
    lift $ hGetContents input >>= hPutStrLn output
    release releaseI
    release releaseO
