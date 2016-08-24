-- | The main program! \o/
-- The IRC part here is heavily inspired by:
-- https://wiki.haskell.org/Roll_your_own_IRC_bot
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Lazy (get, gets, evalStateT, put, runState, StateT)
import Data.List (init, isInfixOf, isPrefixOf)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import Options.Applicative (Parser, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, strOption, value)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (BufferMode(..), Handle, hClose, hFlush, hGetLine, hSetBuffering, stdout)
import System.Random (StdGen, getStdGen, randomR)
import Text.Printf (hPrintf, printf)

import IrcClient
import Types

data Options = Options {
  optServer   :: T.Text,
  optPort     :: Int,
  optChan     :: T.Text,
  optNick     :: T.Text,
  optLongName :: T.Text
}
  deriving (Show)

optParser :: Parser Options
optParser = Options
  <$> option auto
    ( long "server"
    <> short 's'
    <> help "The domain or IP address of the server"
    <> metavar "SERVER"
    <> value "irc.dtek.se"
    )
  <*> option auto
    ( long "port"
    <> short 'p'
    <> help "The port to connect to"
    <> metavar "PORT"
    <> value 6667
    )
  <*> option auto
    ( long "channel"
    <> short 'c'
    <> help "The channel to join"
    <> metavar "CHANNEL"
    <> value "#dtek"
    )
  <*> option auto
    ( long "nick"
    <> short 'n'
    <> help "The nick of the bot"
    <> metavar "n"
    <> value "HejBot"
    )
  <*> option auto
    ( long "longname"
    <> short 'l'
    <> help "The long name of the bot"
    <> metavar "LONGNAME"
    <> value "Hej, the bot from internetz"
    )

main :: IO ()
main = do
  cfg <- execParser $ info (helper <*> optParser)
                         ( fullDesc
                         <> progDesc "The welcoming bot for #dtek"
                         <> header "Welcome!"
                         )

  bracket (connect (optServer cfg) (optPort cfg) (optChan cfg) (optNick cfg) (optLongName cfg))
          hClose
          (\h -> runIRC h (eval h))

eval :: Handle -> IRC -> IO ()
eval h irc = case irc of
               Join nick chan -> sendNotice h nick "Hej och välkommen till #dtek, här hänger snälla datateknologer, stanna här en stund vetja och insup atmosfären!"
               _              -> return ()

