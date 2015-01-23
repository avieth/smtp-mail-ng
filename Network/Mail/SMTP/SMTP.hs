{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Mail.SMTP.SMTP (

    -- This is an internal module; we are very transparent.
    SMTP(..)
  , command
  , expect
  , expectCode

  , SMTPContext
  , smtpContext
    -- Oops: exporting a dangerous function. Be careful!
  , getSMTPHandle
  , getSMTPServerHostName
  , getSMTPClientHostName

  , SMTPError(..)

  ) where

import Control.Exception
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Network
import Network.BSD
import Network.Mail.SMTP.Types
import Network.Mail.SMTP.ReplyLine
import Network.Mail.SMTP.SMTPRaw
import Network.Mail.SMTP.SMTPParameters

import System.IO

data SMTPError
  = UnexpectedResponse
  | ConnectionFailure
  | EncryptionError
  | UnknownError
  deriving (Show, Eq)

data SMTPContext = SMTPContext {
    smtpRaw :: SMTPRaw
  , smtpServerHostName :: HostName
  , smtpClientHostName :: HostName
  }

-- | Access the Handle datatype buried beneath the SMTP abstraction.
--   Use with care!
getSMTPHandle :: SMTPContext -> Handle
getSMTPHandle = smtpHandle . smtpRaw

getSMTPServerHostName :: SMTPContext -> HostName
getSMTPServerHostName = smtpServerHostName

getSMTPClientHostName :: SMTPContext -> HostName
getSMTPClientHostName = smtpServerHostName

-- | We define a little SMTP DSL: it can do effects, things can go wrong, and
--   it carried immutable state.
newtype SMTP a = SMTP {
    runSMTP :: ExceptT SMTPError (ReaderT SMTPContext IO) a
  } deriving (Functor, Applicative, Monad)

smtp :: SMTPParameters -> SMTP a -> IO (Either SMTPError a)
smtp smtpParameters smtpValue = do
  smtpContext <- makeSMTPContext smtpParameters
  case smtpContext of
    Left err -> return $ Left err
    Right smtpContext -> runReaderT (runExceptT (runSMTP smtpValue)) smtpContext

makeSMTPContext :: SMTPParameters -> IO (Either SMTPError SMTPContext)
makeSMTPContext smtpParameters = do
    clientHostname <- getHostName
    result <- liftIO $ try (smtpConnect serverHostname (fromIntegral port))
    return $ case result :: Either SomeException (SMTPRaw, Maybe Greeting) of
      Left err -> Left ConnectionFailure
      Right (smtpRaw, _) -> Right $ SMTPContext smtpRaw serverHostname clientHostname
  where
    serverHostname = smtpHost smtpParameters
    port = smtpPort smtpParameters

-- | Send a command wait for the reply.
command :: Command -> SMTP [ReplyLine]
command cmd = SMTP $ do
  ctxt <- lift ask
  result <- liftIO $ try ((smtpSendCommandAndWait (smtpRaw ctxt) cmd))
  case result :: Either SomeException (Maybe [ReplyLine]) of
    Left err -> throwE UnknownError
    Right x -> case x of
      Just y -> return y
      Nothing -> throwE UnknownError

expect :: [ReplyLine] -> ([ReplyLine] -> Maybe SMTPError) -> SMTP ()
expect reply ok = SMTP $ do
  case ok reply of
    Just err -> throwE err
    Nothing -> return ()

expectCode :: [ReplyLine] -> ReplyCode -> SMTP ()
expectCode reply code = expect reply hasCode
  where
    hasCode [] = Just UnexpectedResponse
    hasCode (reply : _) =
      if replyCode reply == code
      then Nothing
      else Just UnexpectedResponse

-- | Grab the SMTPContext.
smtpContext :: SMTP SMTPContext
smtpContext = SMTP $ lift ask
