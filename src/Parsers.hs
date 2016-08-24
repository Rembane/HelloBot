{-# LANGUAGE OverloadedStrings #-}
module Parsers (parseIrc) where

import Control.Applicative ((<*), (*>))
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Char (isAlpha, isAlphaNum, isAsciiUpper, isDigit, isSpace)
import Data.Ix (inRange)

import Types

-- | Take until \r\n has been reached. Throws newlines away.
takeUntilEOL :: AP.Parser T.Text
takeUntilEOL = AP.takeWhile ((/= '\r'))

-- All the special chars according to special in RFC 2812.
-- https://tools.ietf.org/html/rfc2812#page-8
specialChars :: Char -> Bool
specialChars c = let x = fromEnum c
                  in if ((0x5B, 0x60) `inRange` x) || ((0x7B, 0x7D) `inRange` x)
                        then True
                        else AP.inClass ";[]\\`_^{|}" c

nickParser :: AP.Parser Nick
nickParser = do
  frst <- AP.satisfy (\c -> isAlpha c || specialChars c)
  rest <- AP.takeWhile (\c -> isAlphaNum c || specialChars c || (c == '-'))
  return $ frst `T.cons` rest

shortnameParser :: AP.Parser T.Text
shortnameParser = foldr1 T.append <$> (AP.many1 $ AP.choice [AP.takeWhile1 isAlphaNum, "-"])

hostParser :: AP.Parser T.Text
hostParser = (foldr1 T.append <$> (AP.many' $ AP.choice [shortnameParser, "."]))

-- | Returns the name of the server that sent the ping.
pingParser :: AP.Parser IRC
pingParser = Ping <$> (AP.string "PING :" *> takeUntilEOL)

channelIdParser :: AP.Parser T.Text
channelIdParser = do
  exclamation <- AP.char '!'
  rest        <- AP.count 5 (AP.satisfy (\c -> isAsciiUpper c || isDigit c))
  return $ T.pack (exclamation : rest)

chanParser :: AP.Parser Chan
chanParser = do
  prefix  <- AP.choice ["#", "+", channelIdParser, "&"]
  channel <- AP.takeWhile1 (AP.notInClass " ,:\r\n\NUL\BEL")
  return $ T.append prefix channel

-- TODO Refactor the joinParser, quitParser and partParser to be one baseparser with "children", this will save us quite some backtracking.

joinParser :: AP.Parser IRC
joinParser = do
  nick <- prefixParser
  " JOIN :"
  chan <- chanParser
  return $ Join nick chan

partParser :: AP.Parser IRC
partParser = do
  nick <- prefixParser
  "PART "
  chan <- chanParser
  return $ Part nick chan

quitParser :: AP.Parser IRC
quitParser = do
  nick <- prefixParser
  "QUIT :"
  reason <- takeUntilEOL
  return $ Quit nick reason

targetParser :: AP.Parser Target
targetParser = AP.takeWhile1 (/= ' ')

-- https://tools.ietf.org/html/rfc2812#section-2.3.1
-- prefix = servername / ( nickname [ [ "!" user ] "@" host ] )
prefixParser :: AP.Parser Nick
prefixParser = AP.choice [complexPrefixParser, hostParser]
  where
    complexPrefixParser = nickParser <* "!" <* AP.takeWhile1 (/= '@') <* "@" <* hostParser

privmsgParser :: AP.Parser IRC
privmsgParser = do
  nick <- prefixParser
  " PRIVMSG "
  target <- targetParser
  " :"
  message <- takeUntilEOL
  return $ Privmsg nick target message

motdParser :: AP.Parser IRC
motdParser = hostParser *> " " *> AP.choice ["375", "372", "376"] *> " " *> (MOTD <$> takeUntilEOL)

topicParser :: AP.Parser IRC
topicParser = hostParser *> " 332 " *> nickParser *> " " *> chanParser *> " :" *> (Topic <$> takeUntilEOL)

-- TODO: Add support for 366 and multiline name lists.
whoParser :: AP.Parser IRC
whoParser = hostParser *> " 353 " *> nickParser *> AP.choice [" = ", " * ", " @ "] *> chanParser *> " :" *> (Who <$> AP.sepBy1 (AP.skipWhile (== '@') *> nickParser) " ")

noticeParser :: AP.Parser IRC
noticeParser = prefixParser *> " NOTICE " *> (Notice <$> takeUntilEOL)

ircParser :: AP.Parser IRC
ircParser = do
  AP.choice [pingParser, ":" *> AP.choice [motdParser, topicParser, whoParser, noticeParser, privmsgParser, joinParser, quitParser, partParser]]

-- | Parses one line of IRC communication.
parseIrc :: BS.ByteString -> Maybe IRC
parseIrc input = AP.maybeResult $ AP.parse ircParser s
  where
    s = case E.decodeUtf8' input of
          Left  _ -> E.decodeLatin1 input
          Right s -> s

