{-# LANGUAGE OverloadedStrings #-}

module Network.Mail.SMTP.ReplyLine (

    ReplyLine
  , Greeting

  -- Attoparsec parsers for the datatype given above. The only way you can
  -- obtain a ReplyLine or Greeting is by parsing one from a ByteString.
  , greeting
  , replyLines

  ) where

import qualified Data.ByteString as B
import Data.Attoparsec.Char8
import Control.Applicative

import Network.Mail.SMTP.Types

-- | A reply from a server: code and message.
data ReplyLine = ReplyLine !ReplyCode !B.ByteString
  deriving (Show)

-- | A greeting from a server: domain/host name and message(s).
data Greeting = Greeting !B.ByteString ![B.ByteString]
  deriving (Show)

-- What follows is definitions for parsing ReplyLine and Greeting via
-- Attoparsec combinators.
-- Parser definitions pulled from RFC 5321 section 4.2

crlf :: Parser ()
crlf = char '\r' >> char '\n' >> pure ()

textstring :: Parser B.ByteString
textstring = takeWhile1 predicate
  where
    -- 9 is horizontal tab, and [32, 126] is all printable US ASCII.
    -- Just check your table ;)
    predicate c' = let c = fromEnum c' in c == 9 || (c >= 32 && c <= 126)

-- Spec seems to indicate that the first n reply lines must have a hyphen, but
-- the last one (and there must be at least one) should not have a hyphen
-- between reply code and textstring.
replyLines :: Parser [ReplyLine]
replyLines = (++) <$> many' replyLine' <*> (pure <$> replyLine)

replyLine :: Parser ReplyLine
replyLine = ReplyLine <$> replyCode <* space <*> option "" textstring <* crlf

replyLine' :: Parser ReplyLine
replyLine' = ReplyLine <$> replyCode <* char '-' <*> option "" textstring <* crlf

-- We deviate from the RFC on the response code, because it demands that
-- an SMTP server SHOULD only send the codes listed in the spec. We just take
-- any decimal numbere.
replyCode :: Parser ReplyCode
replyCode = decimal

-- | Parser for a Greeting.
greeting :: Parser Greeting
greeting = manyGreetings <|> oneGreeting
  where

    oneGreeting :: Parser Greeting
    oneGreeting = do
      string "220 "
      bytestring <- takeWhile1 isASCIIPrintableNonWhitespace
      messages <- option [] (char ' ' *> (pure <$> textstring))
      crlf
      return $ Greeting bytestring messages

    manyGreetings :: Parser Greeting
    manyGreetings = do
      string "220-"
      bytestring <- takeWhile1 isASCIIPrintableNonWhitespace
      greets <- option [] (char ' ' *> (pure <$> textstring))
      crlf
      moreGreets <- many (string "220-" *> textstring <* crlf)
      string "220"
      lastgreet <- option [] (pure <$> (char ' ' >> textstring))
      let messages = greets ++ moreGreets ++ lastgreet
      return $ Greeting bytestring messages

isASCIIPrintableNonWhitespace :: Char -> Bool
isASCIIPrintableNonWhitespace c' = let c = fromEnum c' in c > 32 && c <= 126
