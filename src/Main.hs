{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Brick
import Brick.BChan
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Lens
import Data.Map
import qualified Graphics.Vty as V
import Linear.V2 (V2(..), _y)
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)


type Name = ()

data BlockType = Bar | Ball
    deriving (Eq, Show, Enum)

type Coord = V2 Int

type Board = Map Coord BlockType

-- data St = St
--     {   _barPlayerOne   :: Location
--     ,   _barPlayerTwo   :: Location
--     } deriving (Eq, Show)
-- makeLenses ''St

data Game = Game
    {   _scorePlayerOne :: Int
    ,   _scorePlayerTwo :: Int
    --,   _st             :: St
    ,   _board          :: Board
    } deriving (Eq, Show)
makeLenses ''Game

data UI = UI
    {   _game   :: Game
    ,   _paused :: Bool
    ,   _barPlayerOne   :: Location
    ,   _barPlayerTwo   :: Location
    }
makeLenses ''UI

data Tick = Tick

app :: App UI Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

drawUI :: UI -> [Widget Name]
drawUI ui   =
    --[   setAvailableSize (80, 24) $ vCenter $
    --    withBorderStyle unicode $
    --    borderWithLabel (str "Pong") $ (str "Puntaje jugador 1: " <+> str (show (ui ^. game ^. scorePlayerOne))) <+> 
    --        (str "Puntaje jugador 2: " <+> str (show (ui ^. game ^. scorePlayerTwo)))
    --]
    
    [ rightBar ui
    , leftBar ui
    , vLimit 24 $ drawGrid ui
    ]


rightBar :: UI -> Widget Name
rightBar ui =
    translateBy (ui ^. barPlayerTwo) $
    border $ str "|\n|\n|\n|"



leftBar :: UI -> Widget Name
leftBar ui =
    translateBy (ui ^. barPlayerOne) $
    border $ str "|\n|\n|\n|"
    

drawGrid :: UI -> Widget Name
drawGrid ui =
    hLimit 84
        $ withBorderStyle unicode
        $ borderWithLabel (str "Pong")
        $ case ui ^. paused of
            True    -> center $ str "Juego en pausa"
            --False   -> (center $ str "Puntaje jugador 1: " <+> str (show $ ui ^. game ^. scorePlayerOne))
            False   -> (padLeft Max (str (show $ ui ^. game ^. scorePlayerOne)) <+> vBorder <+> padRight Max (str (show $ ui ^. game ^. scorePlayerTwo)))



handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)-- 
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'p') []))    = continue func
    where
        func = if ui ^. paused then ui & paused .~ False else ui & paused .~ True



handleEvent ui (VtyEvent (V.EvKey V.KDown [])) = continue func2
    where
        func2 = ui & barPlayerTwo . locationRowL %~ (+ 1)

--continue $ ui ^. game ^. st & barPlayerTwo.locationRowL .~ (+ 1)

handleEvent ui (VtyEvent (V.EvKey V.KUp [])) = continue func2
    where
        func2 = ui & barPlayerTwo . locationRowL %~ (subtract 1)

--continue $ ui ^. game ^. st & barPlayerTwo.locationRowL .~ (subtract 1)

handleEvent ui (VtyEvent (V.EvKey (V.KChar 's') [])) = continue func2
    where
        func2 = ui & barPlayerOne . locationRowL %~ (+ 1)

--continue $ ui ^. game ^. st & barPlayerTwo.locationRowL .~ (+ 1)
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue func2
    where
        func2 = ui & barPlayerOne . locationRowL %~ (subtract 1)







theMap :: AttrMap
theMap = attrMap
    V.defAttr
    [   (barAttr    , V.white `on` V.white)
    ]

barAttr :: AttrName
barAttr = "barAttr"

initGame :: IO Game
initGame = do
    pure $ Game
        {   _scorePlayerOne = 2
        ,   _scorePlayerTwo = 15
        --,   _st             = St (Location (75, 9)) (Location (2, 9))
        }

playGame :: IO Game
playGame = do
    let delay = 100
    chan <- newBChan 10
    void . forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay delay
    initialGame <- initGame
    --  ui <- customMain (V.mkVty V.defaultConfig) (Just chan) app $ UI
    ui <- defaultMain app $ UI
        { _game    = initialGame
        , _paused  = False
        , _barPlayerOne = Location (2, 9)
        , _barPlayerTwo = Location (75, 9)
        }
    return $ ui ^. game
    --  g <- withBorderStyle unicode $
    --  borderWithLabel (str "Pong") $
    --  (center (str " ") <+> vBorder <+> center (str " "))

handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> putStrLn $ "Tu puntaje es " ++ show s
    Just hs -> if s <= hs then justShowScore else newHighScore
    _       -> justShowScore
  where
    justShowScore = putStrLn $ "Your final score: " ++ show s
    newHighScore = do
      putStrLn $ "Congrats! You just got the new highest score: " ++ show s
      -- setHighScore s

getHighScore :: IO (Maybe Int)
getHighScore = do
    return $ Just 45

main :: IO ()
main = do
    g <- playGame
    handleEndGame (_scorePlayerOne g)
