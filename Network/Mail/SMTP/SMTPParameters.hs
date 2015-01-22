module Network.Mail.SMTP.SMTPParameters (

    SMTPParameters(..)
  , SMTPEncryption(..)
  , SMTPUsername
  , SMTPPassword

  , HostName
  , PortNumber(..)

  , defaultSMTPParameters

  ) where

import Network.Socket (HostName, PortNumber(..))
import Network.Mail.SMTP.Auth

data SMTPParameters = SMTPParameters {
    smtpHost :: HostName
  , smtpPort :: PortNumber
  , smtpAuthentication :: AuthType
  , smtpEncryption :: SMTPEncryption
  , smtpUsername :: SMTPUsername
  , smtpPassword :: SMTPPassword
  } deriving (Show)

-- | SMTP encryption methods.
data SMTPEncryption
  = TLS
  | NONE
  deriving (Show, Eq, Ord)

type SMTPUsername = Maybe String

type SMTPPassword = Maybe String

-- | Default SMTP parameters for some hostname. Uses port 25, no encryption,
--   no username or password.
defaultSMTPParameters :: HostName -> SMTPParameters
defaultSMTPParameters hostname = SMTPParameters {
    smtpHost = hostname
  , smtpPort = 25
  , smtpAuthentication = PLAIN
  , smtpEncryption = NONE
  , smtpUsername = Nothing
  , smtpPassword = Nothing
  }
