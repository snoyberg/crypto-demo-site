module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Crypto demo homepage"
    [whamlet|
        <p>Welcome to the crypto demo site homepage.
        
        <p>Learn more about:
            
        <ul>
            <li>
                <a href=@{AesR}>AES
    |]