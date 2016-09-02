module Types where

import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text as T
import System.IO (Handle)
import System.Log.FastLogger (TimedFastLogger)

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
         | Topic Message
         | Who [Nick]
         | Notice Message
  deriving (Show)

type Net = ReaderT ClientState IO
data ClientState = ClientState
  { handle :: Handle
  , logger :: TimedFastLogger
  }

