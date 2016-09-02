{-# LANGUAGE OverloadedStrings #-}
module IrcClient where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Monoid ((<>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network (PortID(..), connectTo)
import System.IO (BufferMode(..), hSetBuffering)
import System.Log.FastLogger (LogStr, TimedFastLogger, TimeFormat, ToLogStr, toLogStr)
import Text.Printf (hPrintf, printf)

import Parsers
import Types

-- | Just a datetimeformat. The reference is in the link below:
-- http://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime_l.html
timeLogFormat :: TimeFormat
timeLogFormat = "[%Y-%m-%d %H:%M:%S]"

-- | Use this function with one argument, the rest is handled by fast-logger.
emitter :: ToLogStr msg => T.Text -> (msg -> LogStr)
emitter s = (\ft -> (toLogStr ft) <> " " <> (toLogStr s) <> "\n")

logThis :: T.Text -> Net ()
logThis s = do
  lg <- asks logger
  liftIO $ lg $ emitter s

-- | Connect to the server, set nick and all other needed parameters.
-- Return the file handle.
connect :: TimedFastLogger -> Server -> Port -> IO ClientState
connect logger server port = do
  logger $ emitter $ "Connecting to " <> server <> "..."
  h <- connectTo (T.unpack server) (PortNumber $ fromIntegral $ port)
  hSetBuffering h LineBuffering
  logger $ emitter "Done."
  return $ ClientState h logger

-- | Set nickname
setNick :: Nick -> LongName -> Net ()
setNick nick longname = do
  write "NICK" nick
  write "USER" $ nick <> " 0 * :" <> longname
  logThis $ "Changing nick to: " <> nick
  logThis $ "Setting long name to: " <> longname

-- | Join a channel
-- TODO: We can join many channels but not have a clue of which channel we are in. This must be fixed!
joinChannel :: Chan -> Net ()
joinChannel chan = do
  write "JOIN" chan
  logThis $ "Joining channel " <> chan

pong :: Server -> Net ()
pong server = write "PONG" server

-- | Read one line from handle and parse it.
processLine :: Net IRC
processLine = do
  h <- asks handle
  input <- liftIO $ BS.hGetLine h
  logThis $ stableDecoder input
  case parseIrc input of
    Just    r -> return r
    Nothing   -> logThis "ERROR: Couldn't parse line. Trying again with next line." >> processLine

-- | A run function for IRC
-- Runs forever.
runIRC :: (IRC -> Net ()) -> Net ()
runIRC f = forever $ do
  irc <- processLine
  case irc of
    Ping server -> pong server
    x           -> f x

sendNotice :: Nick -> Message -> Net ()
sendNotice n m = do
  write "NOTICE" (n <> " :" <> m)

-- | Send a message to a channel or user.
privmsg :: Chan -> Message -> Net ()
privmsg c s = do
  write "PRIVMSG" (c <> " :" <> s)

-- | Write a command and a message to a handle.
write :: T.Text -> Message -> Net ()
write cmd msg = do
  h <- asks handle
  liftIO $ do
    hPrintf h "%s %s\r\n" cmd $ B8.unpack $ E.encodeUtf8 msg
    printf    "> %s %s\n" cmd $ B8.unpack $ E.encodeUtf8 msg

