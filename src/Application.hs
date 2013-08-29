module Application where

import Import
import qualified System.Random.MWC as MWC

import Handler.Home
import Handler.AES

mkYesodDispatch "App" resourcesApp

main :: IO ()
main = MWC.withSystemRandom $ \gen -> do
    warpEnv App
        { appGen = gen
        }