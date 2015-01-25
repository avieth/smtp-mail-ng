{-# LANGUAGE OverloadedStrings #-}

{- |
Description: terms for sending mail.
-}

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
import Data.ByteString.Lazy.Search (replace)

-- | Attempt to send an email.
--   This involves sending MAIL, RCPT, and DATA commands.
send :: Mail -> SMTP ()
send mail = do
    content <- liftIO $ renderMail' mail
    sendRendered from to content
  where
    from = enc $ mailFrom mail
    to = map enc $ mailTo mail
    enc = encodeUtf8 . addressEmail

-- | Attempt to send "rendered" mail. First argument is a coding of the
--   sender address, second is a coding of each recipient address, and third
--   is the message body.
--   This function will escape any "\r\n.\r\n" pattern, which would otherwise
--   result in a premature ending to the message.
sendRendered :: B.ByteString -> [B.ByteString] -> BL.ByteString -> SMTP ()
sendRendered from to content = do
    command (MAIL from)
    expectCode 250
    -- TODO TBD expect a 250 for each recipient? Isn't it OK to have some
    -- of them fail?
    mapM_ (\r -> command (RCPT r) >> expectCode 250) to
    command DATA
    expectCode 354
    bytes (BL.toStrict escapedContent)
    -- We have to manually put in the .
    bytes (pack ".")
    expectCode 250

  where

    escapedContent :: BL.ByteString
    escapedContent = replace pattern substitution content

    pattern :: B.ByteString
    pattern = "\r\n.\r\n"

    substitution :: BL.ByteString
    substitution = "\r\n..\r\n"
