module Network.Mail.SMTP.Auth (

    authLogin

  ) where

import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString.Base16 as B16  (encode)
import qualified Data.ByteString.Base64 as B64  (encode)

import Data.ByteString  (ByteString)
import Data.List
import Data.Bits
import Data.Monoid
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8

import Network.Mail.SMTP.SMTP
import Network.Mail.SMTP.Types

-- | Do LOGIN authentication.
authLogin :: UserName -> Password -> SMTP ()
authLogin username password = do
  -- There's no Command constructor for AUTH. TODO implement one? It has a
  -- volatile form I think; varies for each AUTH type I believe.
  reply <- bytes (B8.pack "AUTH LOGIN")
  -- TBD do we need to check that it gives the right text?
  -- I thought the RFCs say that only the codes matter...
  expectCode 334
  bytes $ b64Encode username
  expectCode 334
  bytes $ b64Encode password
  -- TODO need a mechanism to specify the error in case the code is bad.
  -- Or, maybe a mechanism to expect multiple codes and handle things
  -- differently based upon the code.
  expectCode 235 

toAscii :: String -> ByteString
toAscii = B.pack . map (toEnum.fromEnum)

b64Encode :: String -> ByteString
b64Encode = B64.encode . toAscii

hmacMD5 :: ByteString -> ByteString -> ByteString
hmacMD5 text key = hash (okey <> hash (ikey <> text))
    where key' = if B.length key > 64
                 then hash key <> B.replicate 48 0
                 else key <> B.replicate (64-B.length key) 0
          ipad = B.replicate 64 0x36
          opad = B.replicate 64 0x5c
          ikey = B.pack $ B.zipWith xor key' ipad
          okey = B.pack $ B.zipWith xor key' opad

encodePlain :: UserName -> Password -> ByteString
encodePlain user pass = b64Encode $ intercalate "\0" [user, user, pass]

encodeLogin :: UserName -> Password -> (ByteString, ByteString)
encodeLogin user pass = (b64Encode user, b64Encode pass)

cramMD5 :: String -> UserName -> Password -> ByteString
cramMD5 challenge user pass =
    B64.encode $ B8.unwords [user', B16.encode (hmacMD5 challenge' pass')]
  where
    challenge' = toAscii challenge
    user'      = toAscii user
    pass'      = toAscii pass

{- Code from before the fork which is now dead, but I'll leave it around as
 - a reference for when we implement CRAM_MD5
auth :: AuthType -> String -> UserName -> Password -> ByteString
auth PLAIN    _ u p = encodePlain u p
auth LOGIN    _ u p = let (u', p') = encodeLogin u p in B8.unwords [u', p']
auth CRAM_MD5 c u p = cramMD5 c u p
-}
