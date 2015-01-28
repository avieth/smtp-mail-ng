SMTP-MAIL-NG
============

An SMTP client EDSL. If you want to interact with an SMTP server, this library
may be able to help you. It even supports STARTTLS!

The star is the SMTP monad, terms of which (thanks to do notation) often
resemble an SMTP session.

### Sending with an SMTP server

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.BSD (getHostName)
import Network.Mail.SMTP.SMTP
import Network.Mail.SMTP.SMTPParameters
import Network.Mail.SMTP.Types
import Network.Mail.SMTP.Auth
import Network.Mail.SMTP.Send
import Network.Mail.Mime
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack)

main = smtp smtpParameters $ do
  hostname <- liftIO getHostName
  -- Send EHLO and expect a 250
  command $ EHLO (pack hostname)
  expectCode 250
  -- Upgrade the connection to TLS
  -- This is a kind of utility term that takes care of sending STARTTLS,
  -- expecting a 220, and then upgrading the underlying connection to TLS.
  startTLS
  -- Authenticate with LOGIN scheme
  authLogin "jarndyce@gmail.com" "mySuperSecretPassword"
  -- Send the message
  send message
  -- End the session.
  -- Closing the connection is handled automatically by the function smtp
  command QUIT

-- We use datatypes from the mime-mail package to describe Mail.
message :: Mail
message = simpleMail' to from subject body
  where
    from = Address (Just "John Jarndyce") "jarndyce@gmail.com"
    to = Address (Just "Harold Skimpole") "harold@skimpole.com"
    subject = "Hey!"
    body = "It works!"

smtpParameters :: SMTPParameters
smtpParameters = (defaultSMTPParameters "smtp.googlemail.com") {
    smtpVerbose = True
  }
```

### Moving forward

We must implement support for more AUTH schemes. Right now all that we
facilitate is LOGIN, although other methods are possible via the bytes
term.

There is an orphan datatype, Response, from before the fork. It may be
good to use this instead of bare Ints.

It would be nice to give a convenient interface for simply sending some
messages, in which the user must supply only a list of Mail values, an
SMTPParameters, and a description of the authentication and encryption
parameters of the mail server.

### Thanks

This library is forked from Jason Hickner's smtp-mail, but it has diverged
significantly and bears little resemblance.
