module Handler.AES where

import Import hiding (Key)
import Data.Aeson.Encode (fromValue)
import Data.Aeson (withText)
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import "cipher-aes" Crypto.Cipher.AES
import Control.Failure
import Data.Attempt (attempt)

aesKeySize :: Int
aesKeySize = 32

aesIvSize :: Int
aesIvSize = 16

keyToText :: Key -> Text
keyToText = decodeUtf8 . B16.encode . keyOfCtx

keyFromText :: Failure InvalidAesKey m => Text -> m Key
keyFromText t =
    case B16.decode $ encodeUtf8 t of
        (bs, "")
            | length bs == aesKeySize -> return $ initKey bs
            | otherwise -> failure $ InvalidAesKeySize (length bs)
        _ -> failure InvalidAesKeyBase16Encoding

ivToText :: IV -> Text
ivToText (IV bs) = decodeUtf8 $ B16.encode bs

ivFromText :: Failure InvalidAesIv m => Text -> m IV
ivFromText t =
    case B16.decode $ encodeUtf8 t of
        (bs, "")
            | length bs == aesIvSize -> return $ IV bs
            | otherwise -> failure $ InvalidAesIvSize (length bs)
        _ -> failure InvalidAesIvBase16Encoding

data InvalidAesKey = InvalidAesKeySize Int
                   | InvalidAesKeyBase16Encoding
    deriving (Show, Typeable)
instance Exception InvalidAesKey

data InvalidAesIv = InvalidAesIvSize Int
                  | InvalidAesIvBase16Encoding
    deriving (Show, Typeable)
instance Exception InvalidAesIv

instance ToJSON Key where
    toJSON = toJSON . keyToText
instance FromJSON Key where
    parseJSON = withText "AES Key" $ attempt (fail . show) return . keyFromText

instance ToJSON IV where
    toJSON = toJSON . ivToText
instance FromJSON IV where
    parseJSON = withText "AES IV" $ attempt (fail . show) return . ivFromText

randomAesKey :: Handler Key
randomAesKey = initKey <$> getRandomBytes aesKeySize

randomAesIv :: Handler IV
randomAesIv = IV <$> getRandomBytes aesIvSize

getAesR :: Handler Html
getAesR = defaultLayout $ do
    setTitle "AES"
    requireJquery
    [whamlet|
        <p>AES is a symmetric key cryptography mechanism.
        <p>
            <label for=key>Key (base16 encoded)
            <input type=text #key>
            <input #random-key type=submit value="Get a random key">
        <p>
            <label for=iv>Initialization vector (base16 encoded)
            <input type=text #iv>
            <input #random-iv type=submit value="Get random IV">
        <p>Plain text
        <textarea #plaintext>
        <p>Cipher text (base64 encoded)
        <textarea #ciphertext>
        <input #encrypt type=submit value=Encrypt>
        <input #decrypt type=submit value=Decrypt>
    |]
    toWidget [julius|
        $(function(){
            $("#random-key").click(function(){
                $.getJSON("@{RandomAesKeyR}", function(data) {
                    $("#key").val(data.key);
                });
            });
            $("#random-iv").click(function(){
                $.getJSON("@{RandomAesIvR}", function(data) {
                    $("#iv").val(data.iv);
                });
            });
            $("#encrypt").click(function(){
                var key = $("#key").val(),
                    iv = $("#iv").val(),
                    plain = $("#plaintext").val();
                $.getJSON("@{AesEncryptR}", {key:key, iv:iv, plain:plain}, function(result){
                    $("#ciphertext").val(result.cipher);
                });
            });
            $("#decrypt").click(function(){
                var key = $("#key").val(),
                    iv = $("#iv").val(),
                    cipher = $("#ciphertext").val();
                $.getJSON("@{AesDecryptR}", {key:key, iv:iv, cipher:cipher}, function(result){
                    $("#plaintext").val(result.plain);
                });
            });
        });
    |]

getRandomAesKeyR :: Handler TypedContent
getRandomAesKeyR = do
    key <- randomAesKey
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Random AES key"
            [whamlet|<p>Your key is: #{toLazyText $ fromValue $ toJSON key}|]
        provideJson $ object ["key" .= key]

getRandomAesIvR :: Handler TypedContent
getRandomAesIvR = do
    iv <- randomAesIv
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Random AES IV"
            [whamlet|<p>Your IV is: #{toLazyText $ fromValue $ toJSON iv}|]
        provideJson $ object ["iv" .= iv]

keyField :: Field Handler Key
keyField = checkMMap (return . attempt (Left . asText . show) Right . keyFromText) keyToText textField

ivField :: Field Handler IV
ivField = checkMMap (return . attempt (Left . asText . show) Right . ivFromText) ivToText textField

getAesEncryptR :: Handler TypedContent
getAesEncryptR = do
    (key, iv, plain) <- runInputGet $ (,,)
        <$> ireq keyField "key"
        <*> ireq ivField "iv"
        <*> ireq textField "plain"
    let cipher = decodeUtf8 $ B64.encode $ encryptCTR key iv $ encodeUtf8 plain
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Encrypted content"
            [whamlet|
                #{cipher}
            |]
        provideJson $ object ["cipher" .= cipher]

b64Field :: Field Handler ByteString
b64Field = checkMMap (return . either (Left . asText . pack) Right . B64.decode . encodeUtf8) (decodeUtf8 . B64.encode) textField

getAesDecryptR :: Handler TypedContent
getAesDecryptR = do
    (key, iv, cipher) <- runInputGet $ (,,)
        <$> ireq keyField "key"
        <*> ireq ivField "iv"
        <*> ireq b64Field "cipher"
    let plain = decodeUtf8 $ decryptCTR key iv cipher
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Decrypted content"
            [whamlet|
                #{plain}
            |]
        provideJson $ object ["plain" .= plain]
