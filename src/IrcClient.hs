{-# LANGUAGE OverloadedStrings #-}
module IrcClient where -- (connect, IRC(..), ircParser, privmsg, privmsgParser, runIRC, write) where

import Control.Applicative ((<*), (*>), (<|>))
import Control.Monad (forever)
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding as E
import Network (PortID(..), connectTo)
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetLine, hSetBuffering, stdout)
import Text.Printf (hPrintf, printf)

type Server   = T.Text
type Port     = Int
type Chan     = T.Text
type Nick     = T.Text
type LongName = T.Text

type Reason = T.Text
type Target = T.Text -- User or channel
type Message = T.Text

data IRC = Join Nick Chan
         | Quit Nick Reason
         | Part Nick Chan
         | Ping Server
         | Privmsg Target Message
         | MOTD Message
  deriving (Show)

-- TODO: Find out how to send a /who and receive the results.

-- | Take until \r\n has been reached. Throws newlines away.
takeUntilEOL :: AP.Parser T.Text
takeUntilEOL = AP.takeWhile (/= '\r')

nickParser :: AP.Parser Nick
nickParser = AP.string ":" *> (AP.takeWhile1 (/= '!')) <* AP.take 1

digitParser :: AP.Parser T.Text
digitParser = AP.takeWhile1 (AP.inClass "0123456789")

-- | Parses A-Z and a-z.
alfaParser :: AP.Parser T.Text
alfaParser = AP.takeWhile1 (\c -> ((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z')))

-- | Parses A-Z, a-z and 0-9.
alfaNumParser :: AP.Parser T.Text
alfaNumParser = AP.choice [digitParser, alfaParser]

shortnameParser :: AP.Parser T.Text
shortnameParser = foldr1 T.append <$> (AP.many1 $ AP.choice [alfaNumParser, AP.string "-"])

hostParser :: AP.Parser T.Text
hostParser = (foldr1 T.append <$> (AP.many' $ AP.choice [shortnameParser, AP.string "."]))

-- | Returns the name of the server that sent the ping.
pingParser :: AP.Parser IRC
pingParser = Ping <$> (AP.string "PING :" *> takeUntilEOL)

-- TODO Refactor the joinParser, quitParser and partParser to be one baseparser with "children", this will save us quite some backtracking.

chanParser :: AP.Parser Chan
chanParser = do
  AP.string "#"
  s <- takeUntilEOL
  return $ T.append "#" s

joinParser :: AP.Parser IRC
joinParser = do
  nick <- nickParser
  hostParser
  AP.string "JOIN :"
  chan <- chanParser
  return $ Join nick chan

partParser :: AP.Parser IRC
partParser = do
  nick <- nickParser
  hostParser
  AP.string "PART "
  chan <- chanParser
  return $ Part nick chan

quitParser :: AP.Parser IRC
quitParser = do
  nick <- nickParser
  hostParser
  AP.string "QUIT :"
  reason <- takeUntilEOL
  return $ Quit nick reason

targetParser :: AP.Parser Target
targetParser = AP.takeWhile1 (/= ' ') <* AP.take 1

privmsgParser :: AP.Parser IRC
privmsgParser = do
  nickParser
  hostParser
  AP.string "PRIVMSG "
  target  <- targetParser
  AP.string ":"
  message <- takeUntilEOL
  return $ Privmsg target message

motdParser :: AP.Parser IRC
motdParser = do
  AP.string ":"
  hostParser
  foldr1 (<|>) $ map AP.string ["375", "372", "376"]
  AP.string " "
  MOTD <$> takeUntilEOL

ircParser :: AP.Parser IRC
ircParser = AP.choice [pingParser, privmsgParser, joinParser, quitParser, partParser, motdParser]

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
pong h server = write h "PONG: " server

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

