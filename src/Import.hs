module Import
    ( module Import
    , module X
    ) where

import ClassyPrelude.Yesod as X
import Foundation as X
import Language.Haskell.TH.Syntax (Q, Exp)
import Yesod.Default.Util (widgetFileNoReload)
import qualified System.Random.MWC as MWC
import Yesod.Form.Jquery (urlJqueryJs)

widgetFile :: String -> Q Exp
widgetFile = widgetFileNoReload def

getRandomBytes :: Int -> Handler ByteString
getRandomBytes bytes = do
    gen <- appGen <$> getYesod
    liftIO $ pack <$> replicateM bytes (MWC.uniform gen)

requireJquery :: Widget
requireJquery = getYesod >>= addScriptEither . urlJqueryJs