{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SMTP.SMTPRaw (

    SMTPRaw
  , smtpConnect
  , smtpSendCommand
  , smtpSendCommandAndWait
  , smtpSend
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

-- | A raw handle for (what is assumed to be) a handle to a socket (possibly
--   closed) interfacing an SMTP server.
data SMTPRaw = SMTPRaw Handle

-- | Try to open an SMTPRaw, taking the server greeting as well.
--   No exception handling is performed.
smtpConnect :: String -> Int -> IO (SMTPRaw, Maybe Greeting)
smtpConnect host port = do
  handle <- connectTo host (PortNumber $ fromIntegral port)
  greet <- parseWith (B.hGetSome handle 2048) greeting ""
  return $ (SMTPRaw handle, maybeResult greet)

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
smtpSendRaw (SMTPRaw handle) bs = B.hPut handle bs

-- | Try to read ReplyLines from the SMTPRaw.
--   No exception handling is performed.
smtpGetReplyLines :: SMTPRaw -> IO (Maybe [ReplyLine])
smtpGetReplyLines (SMTPRaw handle) = do
  replies <- parseWith (B.hGetSome handle 2048) replyLines ""
  return $ maybeResult replies

-- | Close an SMTPRaw handle
--   Be sure not to use the SMTPHandle after this.
smtpDisconnect :: SMTPRaw -> IO ()
smtpDisconnect (SMTPRaw h) = hClose h
