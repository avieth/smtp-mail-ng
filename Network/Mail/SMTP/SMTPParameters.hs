module Network.Mail.SMTP.SMTPParameters (

    SMTPParameters(..)
  , SMTPAuthentication(..)
  , SMTPEncryption(..)
  , SMTPUsername
  , SMTPPassword

  , HostName
  , PortNumber(..)

  ) where

import Network.Socket (HostName, PortNumber)

data SMTPParameters = SMTPParameters {
    smtpHost :: HostName
  , smtpPort :: PortNumber
  , smtpAuthentication :: SMTPAuthentication
  , smtpEncryption :: SMTPEncryption
  , smtpUsername :: SMTPUsername
  , smtpPassword :: SMTPPassword
  } deriving (Show)

-- | SMTP authentication methods. I don't think this is exhaustive.
data SMTPAuthentication
  = LOGIN
  | PLAIN
  | CRAMMD5
  | DIGESTMD5
  | GSSAPI
  deriving (Show, Eq, Ord)

-- | SMTP encryption methods.
data SMTPEncryption
  = TLS
  | NONE
  deriving (Show, Eq, Ord)

type SMTPUsername = Maybe String

type SMTPPassword = Maybe String
