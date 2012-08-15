import Control.Monad.State

data RGBState = Red | Blue | Green
    deriving (Show, Ord, Eq)

nextRGBState :: RGBState -> RGBState
nextRGBState rgb = case rgb of
    Red   -> Blue
    Blue  -> Green
    Green -> Red

rgbState :: State RGBState ()
rgbState = do
    rgb <- get
    put (nextRGBState rgb)
    return ()

runRGBState :: RGBState -> (RGBState,())
runRGBState rgb = let (a,s) = runState rgbState rgb
                  in  (s,a)
