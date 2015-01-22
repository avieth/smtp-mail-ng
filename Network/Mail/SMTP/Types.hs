module Network.Mail.SMTP.Types (

    Command(..)

  , toByteString
    
  , ReplyCode
  , Response(..)

    -- * Auth types (re-exports)
  , UserName
  , Password
  , AuthType(..)

    -- * "Network.Mail.Mime" types (re-exports)
  , Address(..)

  ) where

import Network.Mail.SMTP.Auth

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Network.Mail.Mime

data Command
    = HELO ByteString
    | EHLO ByteString
    | MAIL ByteString
    | RCPT ByteString
    | DATA ByteString
    | EXPN ByteString
    | VRFY ByteString
    | HELP ByteString
    | AUTH AuthType UserName Password
    | NOOP
    | RSET
    | QUIT
    | STARTTLS
    deriving (Show, Eq)

toByteString :: Command -> B.ByteString
toByteString command = case command of
    HELO bs -> B.intercalate space [(B.pack "HELO"), bs]
    EHLO bs -> B.intercalate space [(B.pack "EHLO"), bs]
    MAIL bs -> B.intercalate space [(B.pack "MAIL"), bs]
    RCPT bs -> B.intercalate space [(B.pack "RCPT"), bs]
    DATA bs -> B.intercalate space [(B.pack "DATA"), bs]
    EXPN bs -> B.intercalate space [(B.pack "EXPN"), bs]
    VRFY bs -> B.intercalate space [(B.pack "VRFY"), bs]
    HELP bs -> B.intercalate space [(B.pack "HELP"), bs]
    NOOP -> B.pack "NOOP"
    RSET -> B.pack "RSET"
    QUIT -> B.pack "QUIT"
    STARTTLS -> B.pack "STARTTLS"
    AUTH authType username password ->
      B.intercalate space [B.pack "AUTH", B.pack (show authType), B.pack username, B.pack password]
  where space = B.pack " "

type ReplyCode = Int

-- This poor datatype... It doesn't look like it's used anywhere
data Response
    = Ok
    | SystemStatus
    | HelpMessage
    | ServiceReady
    | ServiceClosing
    | UserNotLocal
    | CannotVerify
    | StartMailInput
    | ServiceNotAvailable
    | MailboxUnavailable
    | ErrorInProcessing
    | InsufficientSystemStorage
    | SyntaxError
    | ParameterError
    | CommandNotImplemented
    | BadSequence
    | ParameterNotImplemented
    | MailboxUnavailableError
    | UserNotLocalError
    | ExceededStorage
    | MailboxNotAllowed
    | TransactionFailed
    deriving (Show, Eq)
