module Network.Mail.SMTP.Send (

    send

  ) where

import Control.Monad.IO.Class

import Network.Mail.SMTP.Types
import Network.Mail.SMTP.SMTP
import Network.Mail.Mime

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding (encodeUtf8)

send :: Mail -> SMTP ()
send mail = do
    content <- liftIO $ fmap BL.toStrict (renderMail' mail)
    sendRendered from to content
  where
    content = renderMail' mail
    from = enc $ mailFrom mail
    to = map enc $ mailTo mail
    enc = encodeUtf8 . addressEmail

-- | Send "rendered" mail. First argument is a coding of the sender address,
--   second is a coding of each recipient address, and third is the message
--   body.
--   It is assumed that the message body does not contain a single dot
--   followed by a crlf, as this would terminate the message early. Such a
--   pattern must be escaped by putting another dot.
sendRendered :: B.ByteString -> [B.ByteString] -> B.ByteString -> SMTP ()
sendRendered from to content = do
    command (MAIL from)
    expectCode 250
    -- TODO TBD expect a 250 for each recipient? Isn't it OK to have some
    -- of them fail?
    mapM_ (\r -> command (RCPT r) >> expectCode 250) to
    command DATA
    expectCode 354
    bytes content
    -- We have to manually put in the .
    bytes (pack ".")
    expectCode 250
