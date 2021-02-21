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

data Xvalue = Derecha | Izquierda
    deriving (Eq, Show)
data Yvalue = Arriba | Abajo
    deriving (Eq, Show)

data UI = UI
    {   _game   :: Game
    ,   _barPlayerOne   :: Location
    ,   _barPlayerTwo   :: Location
    ,   _ball           :: Location
    ,   _xBall          :: Xvalue
    ,   _yBall          :: Yvalue
    ,   _status         :: Int      --  0: Pausa, 1: Inicio, 2: Elegir nivel, 3: Jugando
    ,   _previousStatus :: Int      --  1 por defecto.
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
    case ui ^. status of
        0   -> 
                [paused]
        1   ->
                [mainScreen]
        3   ->
                [   rightBar ui
                ,   leftBar ui
                ,   ballDraw ui
                ,   playing
                ]
        where
            paused  = hLimit 80
                    $ withBorderStyle unicode
                    $ borderWithLabel (str "Pong")
                    $ center $ str "Game paused"
            playing = hLimit 80
                    $ withBorderStyle unicode
                    $ borderWithLabel (str "Pong")
                    $ (padLeft Max (str (show $ ui ^. game ^. scorePlayerOne)) <+> vBorder <+> padRight Max (str (show $ ui ^. game ^. scorePlayerTwo)))
            mainScreen  =   (   hLimit 80
                                $ withBorderStyle unicode
                                $ border
                                $ center pongTitle
                                <=> (hCenter $ str "a version by Butros Asis and Juan Barozzi")
                                <=> (hCenter $ str "\n(1): Play classic\n(2): Play against the machine\n(3): Play against the wall")
                            )
                            

                    
    
    
    --[ rightBar ui
    --, leftBar ui
    --, ballDraw ui
    --, vLimit 24 $ drawGrid ui
    --]

pongTitle :: Widget Name
pongTitle = str "         _               _                _                   _        \n        /\\ \\            /\\ \\             /\\ \\     _          /\\ \\      \n       /  \\ \\          /  \\ \\           /  \\ \\   /\\_\\       /  \\ \\     \n      / /\\ \\ \\        / /\\ \\ \\         / /\\ \\ \\_/ / /      / /\\ \\_\\    \n     / / /\\ \\_\\      / / /\\ \\ \\       / / /\\ \\___/ /      / / /\\/_/    \n    / / /_/ / /     / / /  \\ \\_\\     / / /  \\/____/      / / / ______  \n   / / /__\\/ /     / / /   / / /    / / /    / / /      / / / /\\_____\\ \n  / / /_____/     / / /   / / /    / / /    / / /      / / /  \\/____ / \n / / /           / / /___/ / /    / / /    / / /      / / /_____/ / /  \n/ / /           / / /____\\/ /    / / /    / / /      / / /______\\/ /   \n\\/_/            \\/_________/     \\/_/     \\/_/       \\/___________/     \n"

rightBar :: UI -> Widget Name
rightBar ui =
    translateBy (ui ^. barPlayerTwo) $
    border $ str "|\n|\n|\n|"

leftBar :: UI -> Widget Name
leftBar ui =
    translateBy (ui ^. barPlayerOne) $
    border $ str "|\n|\n|\n|"
    
ballDraw :: UI -> Widget Name
ballDraw ui =
    translateBy (ui ^. ball) $
    str "---"

--  drawGrid :: UI -> Widget Name
--  drawGrid ui =
--      hLimit 80
--          $ withBorderStyle unicode
--          $ borderWithLabel (str "Pong")
--          $ case ui ^. paused of
--              True    -> center $ str "Juego en pausa"
--              --False   -> (center $ str "Puntaje jugador 1: " <+> str (show $ ui ^. game ^. scorePlayerOne))
--              False   -> (padLeft Max (str (show $ ui ^. game ^. scorePlayerOne)) <+> vBorder <+> padRight Max (str (show $ ui ^. game ^. scorePlayerTwo)))

ballUp :: UI -> UI
ballUp ui =
    ui & ball . locationRowL %~ (subtract 1)

ballLeft :: UI -> UI
ballLeft ui =
    ui & ball . locationColumnL %~ (subtract 1)

ballDown :: UI -> UI
ballDown ui =
    ui & ball . locationRowL %~ (+ 1)

ballRight :: UI -> UI
ballRight ui =
    ui & ball . locationColumnL %~ (+ 1)

handleTick :: UI -> EventM Name (Next UI)
handleTick ui =
    if ui ^. status == 0    --  Si está pausado
    then continue ui
    else 
        if ui ^. ball . locationColumnL < ui ^. barPlayerOne . locationColumnL 
        then continue  $ reset $ pointTwo ui
        else if ui ^. ball . locationColumnL > ui ^. barPlayerTwo . locationColumnL 
        then continue $ reset $ pointOne ui
        else
            case ui ^. ball . locationRowL of
                0   -> continue $ uiFinal $ bordeSuperior $ tocaBarra ui
                --False   -> (center $ str "Puntaje jugador 1: " <+> str (show $ ui ^. game ^. scorePlayerOne))
                23  -> continue $ uiFinal $ bordeInferior $ tocaBarra ui
                _   -> continue $ uiFinal $ tocaBarra ui

                where
                    uiHorizontal ui'= if (ui' ^. xBall) == Izquierda then ballLeft ui' else ballRight ui'
                    uiFinal ui' = if (ui' ^. yBall) == Arriba then ballUp $ uiHorizontal ui' else ballDown $ uiHorizontal ui'



        


pointOne :: UI -> UI
pointOne ui = ui & game . scorePlayerOne %~ (+ 1)

pointTwo :: UI -> UI
pointTwo ui = ui & game . scorePlayerTwo %~ (+ 1)

reset :: UI -> UI
reset ui = UI
    { _game             = ui ^. game
    , _barPlayerOne     = Location (2, 9)
    , _barPlayerTwo     = Location (75, 9)
    , _ball             = Location (39, 12)
    , _xBall            = Izquierda
    , _yBall            = Arriba
    , _status           = ui ^. status
    , _previousStatus   = ui ^. previousStatus
    }



        --next <- execStateT timeStep $ ui
        --continue next
    --else continue func3
            --where func3 n = n & ball . locationRowL %~ (+ 1)

--timeStep :: MonadIO m => UI -> PongT m ()
--timeStep ui =
--    ui & ball . locationRowL %~ (+ 1)

tocaBarra :: UI -> UI
tocaBarra ui = 
    if ((ui ^. ball . locationRowL >= ui ^. barPlayerOne . locationRowL) && (ui ^. ball . locationRowL <= (ui ^. barPlayerOne . locationRowL)+5)) && ((ui ^. ball . locationColumnL >= ui ^. barPlayerOne . locationColumnL) && (ui ^. ball . locationColumnL <= (ui ^. barPlayerOne . locationColumnL)+2))
    then ui & xBall .~ Derecha
    else 
        if ((ui ^. ball . locationRowL >= ui ^. barPlayerTwo . locationRowL) && (ui ^. ball . locationRowL <= (ui ^. barPlayerTwo . locationRowL)+5)) && ((ui ^. ball . locationColumnL <= ui ^. barPlayerTwo . locationColumnL) && (ui ^. ball . locationColumnL >= (ui ^. barPlayerTwo . locationColumnL)-2))
        then ui & xBall .~ Izquierda
        else ui


bordeSuperior :: UI -> UI
bordeSuperior ui = ui & yBall .~ Abajo


bordeInferior :: UI -> UI
bordeInferior ui = ui & yBall .~ Arriba


handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)
handleEvent ui event =
    case ui ^. status of
        --  Juego pausado
        0   -> case event of
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue pause
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue pause
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        pause = ui & status .~ (ui ^. previousStatus)    -- Acá debería ir al estado anterior.
        --  Pantalla principal
        1   -> case event of
                    (VtyEvent (V.EvKey (V.KChar '1') []))   -> continue playClassic
                    (VtyEvent (V.EvKey (V.KChar '2') []))   -> continue playMachine
                    (VtyEvent (V.EvKey (V.KChar '3') []))   -> continue playWall
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        playClassic = ui & status .~ 3
                        playMachine = undefined
                        playWall    = undefined
        --  Elegir nivel
        2   -> continue ui
        --  Jugando classic
        3   -> case event of
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue pause
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue pause
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Tick
                    (AppEvent Tick)                         -> handleTick ui
                    --  Controles
                    (VtyEvent (V.EvKey V.KDown []))         -> continue func2
                    (VtyEvent (V.EvKey V.KUp []))           -> continue func3
                    (VtyEvent (V.EvKey (V.KChar 's') []))   -> continue func4
                    (VtyEvent (V.EvKey (V.KChar 'S') []))   -> continue func4
                    (VtyEvent (V.EvKey (V.KChar 'w') []))   -> continue func5
                    (VtyEvent (V.EvKey (V.KChar 'W') []))   -> continue func5
                    --  Cualquier otra tecla no hace nada
                    _                                   -> continue ui
                    where
                        pause = ui & status .~ 0
                        func2 = if (ui ^. barPlayerTwo . locationRowL) < 18 then ui & barPlayerTwo . locationRowL %~ (+ 1) else ui     
                        func3 = if (ui ^. barPlayerTwo . locationRowL) > 0 then ui & barPlayerTwo . locationRowL %~ (subtract 1) else ui
                        func4 = if (ui ^. barPlayerOne . locationRowL) < 18 then ui & barPlayerOne . locationRowL %~ (+ 1) else ui
                        func5 = if (ui ^. barPlayerOne . locationRowL) > 0 then ui & barPlayerOne . locationRowL %~ (subtract 1) else ui
        _   -> halt ui
                    

--  handleEvent ui (AppEvent Tick)  = handleTick ui

--  handleEvent ui (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue func
--  	where
--  		func = if ui ^. paused then ui & paused .~ False else ui & paused .~ True

--  handleEvent ui (VtyEvent (V.EvKey (V.KChar 'P') [])) = continue func
--  	where
--  		func = if ui ^. paused then ui & paused .~ False else ui & paused .~ True

--  handleEvent ui (VtyEvent (V.EvKey V.KDown [])) = continue func2
--  	where
--  		func2 = if (ui ^. barPlayerTwo . locationRowL) < 18 then ui & barPlayerTwo . locationRowL %~ (+ 1) else ui

--  handleEvent ui (VtyEvent (V.EvKey V.KUp [])) = continue func2
--  	where
--  		func2 = if (ui ^. barPlayerTwo . locationRowL) > 0 then ui & barPlayerTwo . locationRowL %~ (subtract 1) else ui

--  handleEvent ui (VtyEvent (V.EvKey (V.KChar 's') [])) = continue func2
--  	where
--  		func2 = if (ui ^. barPlayerOne . locationRowL) < 18 then ui & barPlayerOne . locationRowL %~ (+ 1) else ui

--  handleEvent ui (VtyEvent (V.EvKey (V.KChar 'S') [])) = continue func2
--  	where
--  		func2 = if (ui ^. barPlayerOne . locationRowL) < 18 then ui & barPlayerOne . locationRowL %~ (+ 1) else ui

--  handleEvent ui (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue func2
--  	where
--  	    func2 = if (ui ^. barPlayerOne . locationRowL) > 0 then ui & barPlayerOne . locationRowL %~ (subtract 1) else ui

--  handleEvent ui (VtyEvent (V.EvKey (V.KChar 'W') [])) = continue func2
--  	where
--  		func2 = if (ui ^. barPlayerOne . locationRowL) > 0 then ui & barPlayerOne . locationRowL %~ (subtract 1) else ui


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
        {   _scorePlayerOne = 0
        ,   _scorePlayerTwo = 0
        --,   _st             = St (Location (75, 9)) (Location (2, 9))
        }

playGame :: IO Game
playGame = do
    let delay = 200000
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay delay
    initialGame <- initGame
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    ui <- customMain initialVty buildVty (Just chan) app UI
    --ui <- defaultMain app $ UI
        { _game             = initialGame
        , _barPlayerOne     = Location (2, 9)
        , _barPlayerTwo     = Location (75, 9)
        , _ball             = Location (39, 12)
        , _xBall            = Izquierda
        , _yBall            = Arriba
        , _status           = 1
        , _previousStatus   = 3
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