{-# LANGUAGE FlexibleContexts #-}
-- JACKPOT GAME
module Main where

-- import qualified Data (someFunc)
import Control.Concurrent
import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI
import System.IO
import System.Random
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Data.Maybe
import Data
import Control.Monad
import Control.Monad.Trans

-- initialise the config data type
initConfig :: MonadIO m => m Config
initConfig = do 
    s <- liftIO $ getTerminalSize
    return $ Config {

        screenSize   = fromJust s,
        -- jackPotChar  = [(0,'A'),(1,'B')], can implement in future for alternate display or image
        tickRate     = 10,
        spinRate    = 3,
        jackPotList  = [1..7]
    }

io :: MonadIO m => IO a -> m a
io = liftIO

initGame :: MonadIO m => m (Config,Game)
initGame = do 
    config  <- initConfig
    let game = Game {
        winStreak = 0,
        wins      = 0,
        loss      = 0,
        result    = [],
        left      = 1,
        right     = 2,
        mid       = 3
    }
    return $ (config,game)



renderCharacter ::  Char -> Size -> IO ()
renderCharacter c (y, x) = do
  setCursorPosition y x
  putChar c
  return ()


nextNum :: [Int]->Int->Int
nextNum xs y = do
  let lastVal =  last xs
  if lastVal == y then  head xs else y + 1


checkWin  ::  [Int]->Bool
checkWin []    = False
checkWin [x] = True
checkWin xs  = all(== head xs) xs


-- renderBorder and renderBtm render the border for the jackpot

renderBorder (0,0)=io $ renderCharacter '*'  (0,0)
renderBorder (0,x)=do
  io $ renderCharacter '*'  (0,x)
  renderBorder (0,x-1)
renderBorder (y,0)=do
  io $ renderCharacter '*'  (y,0)   
renderBorder (y,x)=do
  io $ renderCharacter '*'  (y,x)
  renderBorder (y-1,x)
  renderBorder (y-1,0)

renderBtm (y,0) = io $ renderCharacter '*'  (0,0)
renderBtm (y,x)=do
  io $ renderCharacter '*'  (y,x)
  renderBtm (y,x-1)


renderGG :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderGG = forever $ do
  c <- ask
  g <- get
  let rate = tickRate c
  io$ threadDelay (rate*10^5) 
  let (y,x) = screenSize c
  let (y1,x1) = (y `div` 2,x `div` 2)
  let res = result g
  io $ clearScreen
  io $ setCursorPosition y1 x1
  if checkWin res then io $ putStr $"GRATS: " ++ show res else io $ putStr "sorry u just loss ur life saving"
  renderBorder (y,x)
  renderBtm (y,x)

renderNext :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderNext = do
  c <- ask
  g <- get
  let (y,x) = screenSize c
  let (y1,x1) = (y `div` 2,x `div` 2)
  let l =  left g
  let r =  right g
  let m =  mid g
  let l1=head $ show l
  let r1 =head $ show r
  let m1 =head $ show m
  io clearScreen
  
  io $ renderCharacter  (l1)  (y1,x1-2) 
  io $ renderCharacter  (m1 ) (y1,x1)
  io $ renderCharacter  (r1)  (y1,x1+2)
  io $ renderCharacter '*'  (y,x)  
  io $ renderCharacter '*'  (0,0)
  io $ renderCharacter '*'  (y,0)
  io $ renderCharacter '*'  (0,x)
  io $ renderCharacter '*'  (y,x)  

-- roll the 0 to 7 for the jackpot, will replace the value of 
-- game {left, right, mid } every time it rotate
-- will not update it if the game {result} of the respective value is already updated 
iterJackpot:: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
iterJackpot = do
  c <- ask
  g <- get
  let l =  left g
  let r =  right g
  let m =  mid g
  let res = result g
  let jpList = jackPotList c
  if (length $ res )< 3 then
    case length $ res of
      0 -> do
          let newL=nextNum jpList l 
          let newM=nextNum jpList m 
          let newR=nextNum jpList r
          put (
            g{
              left = newL,
              mid = newM,
              right = newR
            })
      1 -> do
          let newM=nextNum jpList m 
          let newR=nextNum jpList r
          put (
            g{
              mid = newM,
              right = newR
            })
      2 -> do
          let newR=nextNum jpList r
          put (
            g{
              right = newR
            })
  else return ()



proceedGame :: (MonadIO m, MonadReader Config m, MonadState Game m) => Event -> m Bool
proceedGame (KeyEvent k) = do
  c <- ask
  g <- get
  let l =  left g
  let r =  right g
  let m =  mid g
  let res = result g
  case length res of
    0 -> do
        let newRes= res ++ [l]
        put (
          g{
            result = newRes
          })
        return True
    1 -> do
        let newRes= res ++ [m]
        put (
          g{
            result = newRes
          })
        return True
    2 -> do
        let newRes= res ++ [r]
        put (
          g{
            result = newRes
          })
        return False
    3 -> return False  

  
proceedGame TickEvent =  return True

castTick :: Chan Event -> Time -> IO ()
castTick chan rate = forever $ do
  threadDelay (rate*10^5) 
  writeChan chan TickEvent


castKey :: Chan Event -> IO () 
castKey chan = forever $ do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  writeChan chan (KeyEvent c)
  print c

play :: Chan Event-> ReaderT Config (StateT Game IO) ()
play chan =   forever $ do
  renderNext
  event <- liftIO $ readChan chan
  again <- proceedGame event
  if again == True then iterJackpot else  do
    renderGG
    





main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    chan <- newChan
    (c,g) <- initGame
    clearScreen 
    let (y,x) = screenSize c
    let (y1,x1) = (y `div` 2,x `div` 2)
    forkIO $ (castTick chan (spinRate c))
    forkIO $ castKey chan
 
    runStateT (runReaderT (play chan ) c) g

    -- putStrLn "do u wan to play again? [y/n]"
    -- playAgain <-getChar
    -- if playAgain == 'y' then runGame else return ((),g)

