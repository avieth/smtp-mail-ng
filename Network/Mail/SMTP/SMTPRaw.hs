{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SMTP.SMTPRaw (

    SMTPRaw(..)
  , smtpConnect
  , smtpSendCommand
  , smtpSendCommandAndWait
  , smtpGetReplyLines
  , smtpDisconnect

  ) where

import qualified Data.ByteString as B
import           Data.ByteString.Char8 (pack, unpack)
import           Network
import           Network.Socket
import           Data.Attoparsec.ByteString.Char8
import           System.IO

import           Network.Mail.SMTP.ReplyLine
import           Network.Mail.SMTP.Types

-- | An SMTPRaw has arbitrary push/pull/close methods, and ALWAYS a Handle,
--   but that Handle is not assumed to be the direct means by which we push
--   pull or close. This is for STARTTLS support.
data SMTPRaw = SMTPRaw {
    smtpPush :: B.ByteString -> IO ()
  , smtpPull :: IO B.ByteString
  , smtpClose :: IO ()
  , smtpHandle :: Handle
  }

-- | Try to open an SMTPRaw, taking the server greeting as well.
--   No exception handling is performed.
smtpConnect :: String -> Int -> IO (SMTPRaw, Maybe Greeting)
smtpConnect host port = do
  handle <- connectTo host (PortNumber $ fromIntegral port)
  greet <- parseWith (B.hGetSome handle 2048) greeting ""
  let push = B.hPut handle
  let pull = B.hGetSome handle 2048
  let close = hClose handle
  return $ (SMTPRaw push pull close handle, maybeResult greet)

-- | Send an SMTP command and wait for the reply.
--   You get Nothing in case the reply does not parse.
--   No exception handling is performed.
smtpSendCommandAndWait :: SMTPRaw -> Command -> IO (Maybe [ReplyLine])
smtpSendCommandAndWait smtpraw cmd = do
  smtpSendCommand smtpraw cmd
  smtpGetReplyLines smtpraw

-- | Send an SMTP command.
--   No exception handling is performed.
smtpSendCommand :: SMTPRaw -> Command -> IO ()
smtpSendCommand smtpraw cmd = do
  smtpSendRaw smtpraw (toByteString cmd)
  smtpSendRaw smtpraw (pack "\r\n")

-- | Send a raw byte string. Use with care. No exception handling is performed.
smtpSendRaw :: SMTPRaw -> B.ByteString -> IO ()
smtpSendRaw = smtpPush

-- | Try to read ReplyLines from the SMTPRaw.
--   No exception handling is performed.
smtpGetReplyLines :: SMTPRaw -> IO (Maybe [ReplyLine])
smtpGetReplyLines smtpraw = do
  replies <- parseWith (smtpPull smtpraw) replyLines ""
  return $ maybeResult replies

-- | Close an SMTPRaw handle
--   Be sure not to use the SMTPHandle after this.
smtpDisconnect :: SMTPRaw -> IO ()
smtpDisconnect = smtpClose
