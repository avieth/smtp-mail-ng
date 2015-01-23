module Network.Mail.SMTP.Types (

    AuthType(..)

  , UserName
  , Password

  , Command(..)

  , toByteString
    
  , ReplyCode
  , Response(..)

    -- * "Network.Mail.Mime" types (re-exports)
  , Address(..)

  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Network.Mail.Mime

type UserName = String
type Password = String

data AuthType
  = LOGIN
  deriving (Show)

data Command
    = HELO ByteString
    | EHLO ByteString
    | MAIL ByteString
    | RCPT ByteString
    | DATA
    | EXPN ByteString
    | VRFY ByteString
    | HELP ByteString
    | NOOP
    | RSET
    | QUIT
    | STARTTLS
    deriving (Show, Eq)

toByteString :: Command -> B.ByteString
toByteString command = case command of
    HELO bs -> B.intercalate space [(B.pack "HELO"), bs]
    EHLO bs -> B.intercalate space [(B.pack "EHLO"), bs]
    MAIL bs -> B.intercalate space [(B.pack "MAIL FROM:"), bs]
    RCPT bs -> B.intercalate space [(B.pack "RCPT TO:"), bs]
    EXPN bs -> B.intercalate space [(B.pack "EXPN"), bs]
    VRFY bs -> B.intercalate space [(B.pack "VRFY"), bs]
    HELP bs -> B.intercalate space [(B.pack "HELP"), bs]
    DATA -> B.pack "DATA"
    NOOP -> B.pack "NOOP"
    RSET -> B.pack "RSET"
    QUIT -> B.pack "QUIT"
    STARTTLS -> B.pack "STARTTLS"
  where
    space = B.pack " "

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
