module Data (
    Config (..),
    Game(..),
    Size,
    Time,
    Event(..)
)
    where

import Control.Monad.State
import Control.Monad.Reader

data Event
  = TickEvent
  | KeyEvent Char
  deriving Show

type Size       = (Int, Int)
type Time       = Int
data Config     = Config {
    screenSize   :: Size,
    jackPotChar  :: [(Int,Char)],
   
    tickRate     :: Time,
    spinRate    :: Time,
    jackPotList   :: [Int]
  } deriving Show
-- data Config     = Config {
--     screenSize   :: Size,
--     charA        :: Char,
--     charB        :: Char,
--     charC        :: Char,
--     charD        :: Char,
--     char7        :: Char,
--     tickRate     :: Time,
--     blinkRate    :: Time
--   } deriving Show

data Game = Game {
    winStreak :: Int,
    wins      :: Int,
    loss      :: Int,
    result    :: [Int],
    left      :: Int,
    right     :: Int,
    mid       :: Int
  } deriving Show