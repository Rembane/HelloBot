{-# LANGUAGE OverloadedStrings #-}
module IrcClient where -- (connect, IRC(..), ircParser, privmsg, privmsgParser, runIRC, write) where

import Control.Applicative ((<*), (*>))
import Control.Monad (forever)
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Char (isAlpha, isAlphaNum)
import Network (PortID(..), connectTo)
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetLine, hSetBuffering, stdout)
import Text.Printf (hPrintf, printf)

type Server   = T.Text
type Port     = Int
type Chan     = T.Text
type Nick     = T.Text
type LongName = T.Text

type Reason = T.Text
type Prefix = T.Text
type Target = T.Text -- User or channel
type Message = T.Text

data IRC = Join Nick Chan
         | Quit Nick Reason
         | Part Nick Chan
         | Ping Server
         | Privmsg Nick Target Message
         | MOTD Message
  deriving (Show)

-- TODO: Find out how to send a /who and receive the results.

-- | Take until \r\n has been reached. Throws newlines away.
takeUntilEOL :: AP.Parser T.Text
takeUntilEOL = AP.takeWhile ((/= '\r'))

nickParser :: AP.Parser Nick
nickParser = AP.takeWhile1 (/= '!')

shortnameParser :: AP.Parser T.Text
shortnameParser = foldr1 T.append <$> (AP.many1 $ AP.choice [AP.takeWhile1 isAlphaNum, "-"])

hostParser :: AP.Parser T.Text
hostParser = (foldr1 T.append <$> (AP.many' $ AP.choice [shortnameParser, "."]))

-- | Returns the name of the server that sent the ping.
pingParser :: AP.Parser IRC
pingParser = Ping <$> (AP.string "PING :" *> takeUntilEOL)

-- TODO Refactor the joinParser, quitParser and partParser to be one baseparser with "children", this will save us quite some backtracking.

chanParser :: AP.Parser Chan
chanParser = do
  "#"
  s <- takeUntilEOL
  return $ T.append "#" s

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

ircParser :: AP.Parser IRC
ircParser = do
  AP.choice [pingParser, ":" *> AP.choice [motdParser, privmsgParser, joinParser, quitParser, partParser]]

-- | Connect to the server, set nick and all other needed parameters.
-- Return the file handle.
connect :: Server -> Port -> Chan -> Nick -> LongName -> IO Handle
connect server port chan nick longname = do
  printf "Connecting to %s ... " $ server
  hFlush stdout
  h <- connectTo (T.unpack server) (PortNumber $ fromIntegral $ port)
  hSetBuffering h NoBuffering
  putStrLn "Done."
  setNick h nick longname
  joinChannel h chan
  return h

-- | Set nickname
setNick :: Handle -> Nick -> LongName -> IO ()
setNick h nick longname = do
  write h "NICK" nick
  write h "USER" $ nick `T.append` " 0 * :" `T.append` longname
  putStrLn $ "Changing nick to: " ++ (T.unpack nick)
  putStrLn $ "Setting long name to: " ++ (T.unpack longname)

-- | Join a channel
-- TODO: We can join many channels but not have a clue of which channel we are in. This must be fixed!
joinChannel :: Handle -> Chan -> IO ()
joinChannel h chan = do
  write h "JOIN" chan
  putStrLn $ "Joining channel " ++ (T.unpack chan)

pong :: Handle -> Server -> IO ()
pong h server = write h "PONG:" server

-- | Read one line from handle and parse it.
processLine :: Handle -> IO IRC
processLine h = do
  input <- BS.hGetLine h
  BS.putStrLn input
  case AP.maybeResult $ AP.parse ircParser $ E.decodeUtf8 $ input of
    Just    r -> return r
    Nothing   -> putStrLn "ERROR: Couldn't parse line. Trying again with next line." >> processLine h

-- | A run function for IRC
-- Runs forever.
runIRC :: Handle -> (IRC -> IO ()) -> IO ()
runIRC h f = forever $ do
  irc <- processLine h
  case irc of
    Ping server -> pong h server
    x           -> f x

-- | Send a message to a channel or user.
privmsg :: Handle -> Chan -> Message -> IO ()
privmsg h c s = do
  write h "PRIVMSG" (c `T.append` " :" `T.append` s)

-- | Write a command and a message to a handle.
write :: Handle -> T.Text -> Message -> IO ()
write h cmd msg = do
  hPrintf h "%s %s\r\n" cmd msg
  printf    "> %s %s\n" cmd msg

