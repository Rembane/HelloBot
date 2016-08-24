module Types where

import qualified Data.Text as T

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

