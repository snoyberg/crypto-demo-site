module Foundation where

import Yesod
import qualified System.Random.MWC as MWC
import Yesod.Form.Jquery (YesodJquery)

data App = App
    { appGen :: !MWC.GenIO
    }

mkYesodData "App" [parseRoutes|
/ HomeR GET
/aes AesR GET
/aes/random-key RandomAesKeyR GET
/aes/random-iv RandomAesIvR GET
/aes/encrypt AesEncryptR GET
/aes/decrypt AesDecryptR GET
|]

instance Yesod App
instance YesodJquery App
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage