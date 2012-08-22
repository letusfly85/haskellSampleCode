import Control.Monad.Reader

data EnvInfo = EnvInfo { getEnvInfo :: (Int, Int) }
    deriving (Show,Eq)

envInfo :: EnvInfo
envInfo = EnvInfo (1985, 3)

sampleReader :: Reader EnvInfo String
sampleReader = do
    envInfo <- ask
    let (f,s) = getEnvInfo envInfo
        res   = ("your birthdate is " ++ (show f) ++ "/0" ++ (show s))
    return res

runSample :: EnvInfo -> String
runSample = runReader sampleReader
