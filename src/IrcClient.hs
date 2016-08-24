{-# LANGUAGE OverloadedStrings #-}
module IrcClient where

import Control.Monad (forever)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Network (PortID(..), connectTo)
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetLine, hSetBuffering, stdout)
import Text.Printf (hPrintf, printf)

import Parsers
import Types

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
pong h server = write h "PONG" server

-- | Read one line from handle and parse it.
processLine :: Handle -> IO IRC
processLine h = do
  input <- BS.hGetLine h
  B8.putStrLn input
  case parseIrc input of
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

sendNotice :: Handle -> Nick -> Message -> IO ()
sendNotice h n m = do
  write h "NOTICE" (n `T.append` " :" `T.append` m)

-- | Send a message to a channel or user.
privmsg :: Handle -> Chan -> Message -> IO ()
privmsg h c s = do
  write h "PRIVMSG" (c `T.append` " :" `T.append` s)

-- | Write a command and a message to a handle.
write :: Handle -> T.Text -> Message -> IO ()
write h cmd msg = do
  hPrintf h "%s %s\r\n" cmd $ B8.unpack $ E.encodeUtf8 msg
  printf    "> %s %s\n" cmd $ B8.unpack $ E.encodeUtf8 msg

