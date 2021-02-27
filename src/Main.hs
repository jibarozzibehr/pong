--Game over screens
--Instructions Screen
--Add Readme
--Add comments to the code
--Delete innecessary code and comments
--Split code in modules
--Prepare Final presentation

--Set ball direction randomly on the points but the first one


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
import qualified Graphics.Vty as V
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import System.Random


type Name = ()

data BlockType = Bar | Ball
    deriving (Eq, Show, Enum)

data Game = Game
    {   _scorePlayerOne :: Int
    ,   _scorePlayerTwo :: Int
    } deriving (Eq, Show)
makeLenses ''Game

data Xvalue = Derecha | Izquierda
    deriving (Eq, Show)
data Yvalue = Arriba | Abajo
    deriving (Eq, Show)

data UI = UI
    {   _game   :: Game
    ,   _barPlayerOne       :: Location
    ,   _barPlayerTwo       :: Location
    ,   _ball               :: Location
    ,   _xBall              :: Xvalue
    ,   _yBall              :: Yvalue
    ,   _status             :: Int      --  0: Pausa, 1: Inicio, 2: Elegir nivel, 3: Jugando
    ,   _previousStatus     :: Int      --  1 por defecto. Se usa para poner en pausa el juego y volver al mismo.
    ,   _level              :: TVar Int
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
    case ui ^. status of
        0   ->  [paused]
        1   ->  [mainScreen]
        2   ->  [selectClassicLevel]
        3   ->  [   withAttr barAttr $ rightBar ui
                ,   withAttr barAttr $ leftBar ui
                ,   withAttr ballAttr $ ballDraw ui
                ,   playing
                ]
        4   ->  [selectMachineLevel]
        
        5   ->  [   withAttr barAttr $ rightBar ui
                ,   withAttr barAttr $ leftBar ui
                ,   withAttr ballAttr $ ballDraw ui
                ,   playing
                ]

        6   ->  [selectWallLevel]
        
        7   ->  [   withAttr barAttr $ leftBar ui
                ,   withAttr ballAttr $ ballDraw ui
                ,   playingWall
                ]
        
        8   ->  [gameOverClassic]
        9   ->  [gameOverMachine]
        10  ->  [gameOverWall]
        11  ->  [controls, joystickArt, instructions]
        _   ->  [emptyWidget]
        where
            paused              =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                $ center $ str "Game paused"

            playing             =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                (padLeft Max (str (show $ ui ^. game . scorePlayerOne)) <+> vBorder <+> padRight Max (str (show $ ui ^. game . scorePlayerTwo)))

            playingWall         =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                $ padBottom Max
                --  $ padTop (Pad 1)
                $ hCenter (str "Score: " <+> str (show $ ui ^. game . scorePlayerOne))

            mainScreen          =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ border
                $ padTopBottom 1
                $ hCenter pongTitle
                <=> hCenter (str "\na version by Butros Asis and Juan Barozzi")
                <=> padBottom Max (hCenter $ str "\n\n(1): Play classic\n(2): Play against the machine\n(3): Play against the wall\n\n(4): Instructions")  

            selectClassicLevel  =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ border
                $ padTopBottom 1
                $ hCenter classicTitle
                <=> hCenter (str "The Pong version you already know. 1v1.")
                <=> hCenter (str "\n\n\n\n\nChoose ball speed:")
                <=> padBottom Max (hCenter (str "\n(1): Slow\n(2): Medium\n(3): Fast!\n\n(b): Go back"))

            selectMachineLevel  =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ border
                $ padTopBottom 1
                $ hCenter machineTitle
                <=> hCenter (str "\nWill you ever win?")
                <=> hCenter (str "\n\n\n\nChoose ball speed:")
                <=> padBottom Max (hCenter (str "\n(1): Slow\n(2): Medium\n(3): Fast!\n\n(b): Go back"))

            selectWallLevel     =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ border
                $ padTopBottom 1
                $ padBottom (Pad 1) (hCenter $ str "\nPlay against the")
                <=> hCenter wallTitle
                -- <=> (hCenter $ str "\nWill you ever win?")
                <=> hCenter (str "\n\n\n\nChoose ball speed:")
                <=> padBottom Max (hCenter (str "\n(1): Slow\n(2): Medium\n(3): Fast!\n\n(b): Go back"))

            gameOverClassic     = 
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                $ padTopBottom 2
                $ hCenter gameOverTitle
                <=> center (str "Final score: " <+> str (show (ui ^. game . scorePlayerOne)))
                <=> hCenter (padBottom Max (str "\n\n(b): Main menu"))

            gameOverMachine     = 
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                $ padTopBottom 2
                $ hCenter gameOverTitle
                <=> center (str "Final score: " <+> str (show (ui ^. game . scorePlayerOne)))
                <=> hCenter (padBottom Max (str "\n\n(b): Main menu"))

            gameOverWall     =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                $ padTopBottom 2
                $ hCenter gameOverTitle
                <=> center (str "Final score: " <+> str (show (ui ^. game . scorePlayerOne)))
                <=> hCenter (padBottom Max (str "\n\n(b): Main menu"))

            instructions    =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong: Instructions")
                $ center (str " ")
                <=> hCenter (str "(b): Go back\n\n\n\n")
            
            controls        =
                translateBy (Location (5, 3))
                $ hLimit 35
                $ vLimit 20
                $ borderWithLabel (str "Controls")
                $ str "\n  (w) -> Player One bar up  \n  (s) -> Player One bar down  \n\n  (↑ ) -> Player Two bar up  \n  (↓ ) -> Player Two bar down  \n\n  (p) -> Pause game  \n  (q) -> Quit game  \n  (b) -> Go back  \n\n"

pongTitle, classicTitle, machineTitle, wallTitle, gameOverTitle, joystickArt :: Widget Name
pongTitle       = str "         _               _                _                   _        \n        /\\ \\            /\\ \\             /\\ \\     _          /\\ \\      \n       /  \\ \\          /  \\ \\           /  \\ \\   /\\_\\       /  \\ \\     \n      / /\\ \\ \\        / /\\ \\ \\         / /\\ \\ \\_/ / /      / /\\ \\_\\    \n     / / /\\ \\_\\      / / /\\ \\ \\       / / /\\ \\___/ /      / / /\\/_/    \n    / / /_/ / /     / / /  \\ \\_\\     / / /  \\/____/      / / / ______  \n   / / /__\\/ /     / / /   / / /    / / /    / / /      / / / /\\_____\\ \n  / / /_____/     / / /   / / /    / / /    / / /      / / /  \\/____ / \n / / /           / / /___/ / /    / / /    / / /      / / /_____/ / /  \n/ / /           / / /____\\/ /    / / /    / / /      / / /______\\/ /   \n\\/_/            \\/_________/     \\/_/     \\/_/       \\/___________/     \n"
classicTitle    = str "░█████╗░██╗░░░░░░█████╗░░██████╗░██████╗██╗░█████╗░\n██╔══██╗██║░░░░░██╔══██╗██╔════╝██╔════╝██║██╔══██╗\n██║░░╚═╝██║░░░░░███████║╚█████╗░╚█████╗░██║██║░░╚═╝\n██║░░██╗██║░░░░░██╔══██║░╚═══██╗░╚═══██╗██║██║░░██╗\n╚█████╔╝███████╗██║░░██║██████╔╝██████╔╝██║╚█████╔╝\n░╚════╝░╚══════╝╚═╝░░╚═╝╚═════╝░╚═════╝░╚═╝░╚════╝░"
machineTitle    = str "Play against the\n __    __   ______   ______   __  __   __   __   __   ______    \n/\\ \"-./  \\ /\\  __ \\ /\\  ___\\ /\\ \\_\\ \\ /\\ \\ /\\ \"-.\\ \\ /\\  ___\\   \n\\ \\ \\-./\\ \\\\ \\  __ \\\\ \\ \\____\\ \\  __ \\\\ \\ \\\\ \\ \\-.  \\\\ \\  __\\   \n \\ \\_\\ \\ \\_\\\\ \\_\\ \\_\\\\ \\_____\\\\ \\_\\ \\_\\\\ \\_\\\\ \\_\\\\\"\\_\\\\ \\_____\\ \n  \\/_/  \\/_/ \\/_/\\/_/ \\/_____/ \\/_/\\/_/ \\/_/ \\/_/ \\/_/ \\/_____/ "
wallTitle       = str "  _/          _/    _/_/    _/        _/     \n _/          _/  _/    _/  _/        _/      \n_/    _/    _/  _/_/_/_/  _/        _/       \n _/  _/  _/    _/    _/  _/        _/        \n  _/  _/      _/    _/  _/_/_/_/  _/_/_/_/   "
gameOverTitle   = str "   ______                        ____                 \n  / ____/___ _____ ___  ___     / __ \\_   _____  _____\n / / __/ __ `/ __ `__ \\/ _ \\   / / / / | / / _ \\/ ___/\n/ /_/ / /_/ / / / / / /  __/  / /_/ /| |/ /  __/ /    \n\\____/\\__,_/_/ /_/ /_/\\___/   \\____/ |___/\\___/_/     "
joystickArt     =
    translateBy (Location (45, 6))
    $ str "            __           \n           (  )          \n            ||           \n            ||           \n        ___|\"\"|__.._     \n       /____________\\    \n       \\____________/~~~."

rightBar :: UI -> Widget Name
rightBar ui =
    translateBy (ui ^. barPlayerTwo) $
    border $ str " \n \n \n "

leftBar :: UI -> Widget Name
leftBar ui =
    translateBy (ui ^. barPlayerOne) $
    border $ str " \n \n \n "
    
ballDraw :: UI -> Widget Name
ballDraw ui =
    translateBy (ui ^. ball) $
    str "  "

--  drawGrid :: UI -> Widget Name
--  drawGrid ui =
--      hLimit 80
--          $ withBorderStyle unicode
--          $ borderWithLabel (str "Pong")
--          $ case ui ^. paused of
--              True    -> center $ str "Juego en pausa"
--              --False   -> (center $ str "Puntaje jugador 1: " <+> str (show $ ui ^. game ^. scorePlayerOne))
--              False   -> (padLeft Max (str (show $ ui ^. game ^. scorePlayerOne)) <+> vBorder <+> padRight Max (str (show $ ui ^. game ^. scorePlayerTwo)))




handleTickMachine :: UI -> EventM Name (Next UI)
handleTickMachine ui
    |   ui ^. status == 0 = continue ui --  Si está pausado
    |   ui ^. ball . locationColumnL < ui ^. barPlayerOne . locationColumnL =
            if ui ^. game . scorePlayerTwo < 6
                then continue  $ reset $ pointTwo ui
                else continue $ pointTwo $ ui & status .~ 9
    |   ui ^. ball . locationColumnL > ui ^. barPlayerTwo . locationColumnL =
            if ui ^. game . scorePlayerOne < 6
                then continue $ reset $ pointOne ui
                else continue $ pointOne $ ui & status .~ 9
    |   otherwise = 
            case ui ^. ball . locationRowL of
                1   -> continue $ uiFinal $ bordeSuperior $ tocaBarra ui
                22  -> continue $ uiFinal $ bordeInferior $ tocaBarra ui
                _   -> continue $ uiFinal $ tocaBarra ui

                where
                    uiHorizontal ui'= if (ui' ^. xBall) == Izquierda then ballLeft ui' else ballRight ui'
                    uiFinal ui' = if (ui' ^. yBall) == Arriba then ballUp $ uiHorizontal $ machineBarUp ui' else ballDown $ uiHorizontal $ machineBarDown ui'

machineBarUp :: UI -> UI
machineBarUp ui =
    if (((ui ^. xBall) == Derecha) && ((ui ^. ball . locationColumnL) > 39)) && ((ui ^. barPlayerTwo . locationRowL) > 0) then ui & barPlayerTwo . locationRowL %~ subtract 1 else ui

machineBarDown :: UI -> UI
machineBarDown ui =
    if (((ui ^. xBall) == Derecha) && ((ui ^. ball . locationColumnL) > 39)) && ((ui ^. barPlayerTwo . locationRowL) < 18) then ui & barPlayerTwo . locationRowL %~ (+ 1) else ui

handleTickWall :: UI -> EventM Name (Next UI)
handleTickWall ui
    |   ui ^. status == 0 = continue ui --  Si está pausado
    |   ui ^. ball . locationColumnL < ui ^. barPlayerOne . locationColumnL = continue $ ui & status .~ 10                                         --GAME OVER
    |   ui ^. ball . locationColumnL == 77 = continue $ uiFinal $ pointOne ui & xBall .~ Izquierda
    |   otherwise =
            case ui ^. ball . locationRowL of
                1   -> continue $ uiFinal $ bordeSuperior $ tocaBarraWall ui
                --False   -> (center $ str "Puntaje jugador 1: " <+> str (show $ ui ^. game ^. scorePlayerOne))
                22  -> continue $ uiFinal $ bordeInferior $ tocaBarraWall ui
                _   -> continue $ uiFinal $ tocaBarraWall ui

                where
                    uiHorizontal ui'= if (ui' ^. xBall) == Izquierda then ballLeft ui' else ballRight ui'
                    uiFinal ui' = if (ui' ^. yBall) == Arriba then ballUp $ uiHorizontal ui' else ballDown $ uiHorizontal ui'

ballUp :: UI -> UI
ballUp ui =
    ui & ball . locationRowL %~ subtract 1

ballLeft :: UI -> UI
ballLeft ui =
    ui & ball . locationColumnL %~ subtract 1

ballDown :: UI -> UI
ballDown ui =
    ui & ball . locationRowL %~ (+ 1)

ballRight :: UI -> UI
ballRight ui =
    ui & ball . locationColumnL %~ (+ 1)

handleTick :: UI -> EventM Name (Next UI)
handleTick ui
    |   ui ^. status == 0 =  continue ui   --  Si está pausado
    |   ui ^. ball . locationColumnL < ui ^. barPlayerOne . locationColumnL = 
            if ui ^. game . scorePlayerTwo < 6
                then continue  $ reset $ pointTwo ui
                else continue $ pointTwo $ ui & status .~ 8
    |   ui ^. ball . locationColumnL > ui ^. barPlayerTwo . locationColumnL = 
            if ui ^. game . scorePlayerOne < 6
                then continue $ reset $ pointOne ui
                else continue $ pointOne $ ui & status .~ 8
    |   otherwise = 
            case ui ^. ball . locationRowL of
                1   -> continue $ uiFinal $ bordeSuperior $ tocaBarra ui
                --False   -> (center $ str "Puntaje jugador 1: " <+> str (show $ ui ^. game ^. scorePlayerOne))
                22  -> continue $ uiFinal $ bordeInferior $ tocaBarra ui
                _   -> continue $ uiFinal $ tocaBarra ui

                where
                    uiHorizontal ui'= if (ui' ^. xBall) == Izquierda then ballLeft ui' else ballRight ui'
                    uiFinal ui' = if (ui' ^. yBall) == Arriba then ballUp $ uiHorizontal ui' else ballDown $ uiHorizontal ui'

pointOne :: UI -> UI
pointOne ui = ui & game . scorePlayerOne %~ (+ 1)
--pointOne ui = if ui & game . scorePlayerOne < 6 then ui & game . scorePlayerOne %~ (+ 1) else ui & status 

pointTwo :: UI -> UI
pointTwo ui = ui & game . scorePlayerTwo %~ (+ 1)
--pointTwo ui = if ui ^. game . scorePlayerTwo < 6 then ui & game . scorePlayerTwo %~ (+ 1) else ui & status .~ 10

reset :: UI -> UI
reset ui = UI
    { _game             = ui ^. game
    , _barPlayerOne     = Location (1, 9)
    , _barPlayerTwo     = Location (76, 9)
    , _ball             = Location (39, 12)
    , _xBall            = Izquierda                      --newX randomNumber
    , _yBall            = Arriba
    , _status           = ui ^. status
    , _previousStatus   = ui ^. previousStatus
    , _level            = ui ^. level
    }
       -- where 
       --     randomNumber = do
       --         xRand <- randomRIO (0,1)
       --         return xRand

--randomRIO (1, 10)

        --next <- execStateT timeStep $ ui
        --continue next
    --else continue func3
            --where func3 n = n & ball . locationRowL %~ (+ 1)

--timeStep :: MonadIO m => UI -> PongT m ()
--timeStep ui =
--    ui & ball . locationRowL %~ (+ 1)

tocaBarra :: UI -> UI
tocaBarra ui
    |   ((ui ^. ball . locationRowL >= ui ^. barPlayerOne . locationRowL) && (ui ^. ball . locationRowL <= (ui ^. barPlayerOne . locationRowL)+5)) && ((ui ^. ball . locationColumnL >= ui ^. barPlayerOne . locationColumnL) && (ui ^. ball . locationColumnL <= (ui ^. barPlayerOne . locationColumnL)+3)) = ui & xBall .~ Derecha
    |   ((ui ^. ball . locationRowL >= ui ^. barPlayerTwo . locationRowL) && (ui ^. ball . locationRowL <= (ui ^. barPlayerTwo . locationRowL)+5)) && ((ui ^. ball . locationColumnL <= ui ^. barPlayerTwo . locationColumnL) && (ui ^. ball . locationColumnL >= (ui ^. barPlayerTwo . locationColumnL)-2)) = ui & xBall .~ Izquierda
    |   otherwise = ui

tocaBarraWall :: UI -> UI
tocaBarraWall ui = 
    if ((ui ^. ball . locationRowL >= ui ^. barPlayerOne . locationRowL) && (ui ^. ball . locationRowL <= (ui ^. barPlayerOne . locationRowL)+5)) && ((ui ^. ball . locationColumnL >= ui ^. barPlayerOne . locationColumnL) && (ui ^. ball . locationColumnL <= (ui ^. barPlayerOne . locationColumnL)+3))
        then ui & xBall .~ Derecha
        else ui

bordeSuperior :: UI -> UI
bordeSuperior ui = ui & yBall .~ Abajo


bordeInferior :: UI -> UI
bordeInferior ui = ui & yBall .~ Arriba

--  Guarda el estado actual como previo.
setPreviousStatus :: UI -> UI
setPreviousStatus ui =
    ui & previousStatus .~ (ui ^. status)

handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)
handleEvent ui event =
    case ui ^. status of
        --  Juego pausado
        0   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue pause
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue pause
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        pause = ui & status .~ (ui ^. previousStatus)   --  Vuelve al estado en el que estaba.
        --  Pantalla principal
        1   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar '1') []))   -> continue $ playClassic ui
                    (VtyEvent (V.EvKey (V.KChar '2') []))   -> continue $ playMachine ui
                    (VtyEvent (V.EvKey (V.KChar '3') []))   -> continue $ playWall ui
                    (VtyEvent (V.EvKey (V.KChar '4') []))   -> continue $ instructions ui
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        playClassic ui'     = ui' & status .~ 2
                        playMachine ui'     = ui' & status .~ 4
                        playWall ui'        = ui' & status .~ 6
                        instructions ui'    = ui' & status .~ 11

        --  Elegir nivel de classic
        --2   -> continue ui
        2   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar '1') []))   -> setLevel ui 200000 3
                    (VtyEvent (V.EvKey (V.KChar '2') []))   -> setLevel ui 100000 3
                    (VtyEvent (V.EvKey (V.KChar '3') []))   -> setLevel ui 80000  3
                    
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        --  Jugando Classic
        3   ->  case event of
                    --  Pausa y quitar juego
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue $ pauseGame ui
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue $ pauseGame ui
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Volver atrás
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    --  Tick
                    (AppEvent Tick)                         -> handleTick ui
                    --  Controles
                    (VtyEvent (V.EvKey V.KDown []))         -> continue playerTwoMoveDown
                    (VtyEvent (V.EvKey V.KUp []))           -> continue playerTwoMoveUp
                    (VtyEvent (V.EvKey (V.KChar 's') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'S') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'w') []))   -> continue $ playerOneMoveUp ui
                    (VtyEvent (V.EvKey (V.KChar 'W') []))   -> continue $ playerOneMoveUp ui
                    --  Cualquier otra tecla no hace nada
                    _                                   -> continue ui
                    where
                        goBack ui' = ui' & status .~ 2
                        playerTwoMoveDown = if (ui ^. barPlayerTwo . locationRowL) < 18 then ui & barPlayerTwo . locationRowL %~ (+ 1) else ui
                        playerTwoMoveUp = if (ui ^. barPlayerTwo . locationRowL) > 0 then ui & barPlayerTwo . locationRowL %~ subtract 1 else ui

        --  Elegir nivel de Against the Machine
        4   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar '1') []))   -> setLevel ui 200000 5
                    (VtyEvent (V.EvKey (V.KChar '2') []))   -> setLevel ui 100000 5
                    (VtyEvent (V.EvKey (V.KChar '3') []))   -> setLevel ui 80000 5

                    --  Volver atrás
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui

                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        --  Jugando Against the Machine
        5   ->  case event of
                    --  Pausa y quitar juego
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue $ pauseGame ui
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue $ pauseGame ui
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Volver atrás
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    --  Tick
                    (AppEvent Tick)                         -> handleTickMachine ui 
                    --  Controles
                    (VtyEvent (V.EvKey (V.KChar 's') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'S') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'w') []))   -> continue $ playerOneMoveUp ui
                    (VtyEvent (V.EvKey (V.KChar 'W') []))   -> continue $ playerOneMoveUp ui
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 4

         --  Elegir nivel de Against the Wall
        6   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar '1') []))   -> setLevel ui 200000 7
                    (VtyEvent (V.EvKey (V.KChar '2') []))   -> setLevel ui 100000 7
                    (VtyEvent (V.EvKey (V.KChar '3') []))   -> setLevel ui 80000 7

                    --  Volver atrás
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui

                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 1

        --  Jugando Against the Wall
        7   ->  case event of
                    --  Pausa y quitar juego
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue $ pauseGame ui
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue $ pauseGame ui
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Volver atrás
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    --  Tick
                    (AppEvent Tick)                         -> handleTickWall ui 
                    --  Controles
                    (VtyEvent (V.EvKey (V.KChar 's') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'S') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'w') []))   -> continue $ playerOneMoveUp ui
                    (VtyEvent (V.EvKey (V.KChar 'W') []))   -> continue $ playerOneMoveUp ui
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 6
        10  ->  case event of
                    --  Quitar juego
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Volver atrás
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    _   ->  continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        11  ->  case event of
                    --  Quitar juego
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Volver atrás
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    _   ->  continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        _   ->  halt ui

resetScores :: UI -> UI
resetScores ui = UI
    { _game             = Game { _scorePlayerOne = 0, _scorePlayerTwo = 0 }
    , _barPlayerOne     = Location (1, 9)
    , _barPlayerTwo     = Location (76, 9)
    , _ball             = Location (39, 12)
    , _xBall            = Izquierda                      --newX randomNumber
    , _yBall            = Arriba
    , _status           = ui ^. status
    , _previousStatus   = ui ^. previousStatus
    , _level            = ui ^. level
    }

playerOneMoveDown :: UI -> UI
playerOneMoveDown ui =
    if (ui ^. barPlayerOne . locationRowL) < 18
        then ui & barPlayerOne . locationRowL %~ (+ 1)
        else ui
    
playerOneMoveUp :: UI -> UI
playerOneMoveUp ui =
    if (ui ^. barPlayerOne . locationRowL) > 0
        then ui & barPlayerOne . locationRowL %~ subtract 1
        else ui

pauseGame :: UI -> UI
pauseGame ui =
    ui & status .~ 0

--  Edita "level" para modificar el delay.
setLevel :: UI -> Int -> Int -> EventM n (Next UI)
setLevel ui lvl mode = do
    liftIO $ atomically $ writeTVar (ui ^. level) lvl
    continue $ ui & status .~ mode

theMap :: AttrMap
theMap = attrMap
    V.defAttr
    [   (barAttr    , V.white `on` V.white)
    ,   (ballAttr   , V.white `on` V.white)
    ]

barAttr, ballAttr :: AttrName
barAttr     = "barAttr"
ballAttr    = "ballAttr"

initGame :: IO Game
initGame = 
    pure $ Game
        {   _scorePlayerOne = 0
        ,   _scorePlayerTwo = 0
        }

initialSpeed :: Int
initialSpeed = 200000

playGame :: IO Game
playGame = do
    --  let delay = 200000
    chan <- newBChan 10
    tv   <- atomically $ newTVar initialSpeed
    _ <- forkIO $ forever $ do
        writeBChan chan Tick
        int <- readTVarIO tv
        threadDelay int
    initialGame <- initGame
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    xRand <- randomRIO (0,1) 
    yRand <- randomRIO (0,1) 
    ui <- customMain initialVty buildVty (Just chan) app UI
    --ui <- defaultMain app $ UI
        { _game             = initialGame
        , _barPlayerOne     = Location (1, 9)
        , _barPlayerTwo     = Location (76, 9)
        , _ball             = Location (39, 12)
        , _xBall            = newX xRand
        , _yBall            = newY yRand
        , _status           = 1
        , _previousStatus   = 1
        , _level            = tv
        }
    return $ ui ^. game
    --  g <- withBorderStyle unicode $
    --  borderWithLabel (str "Pong") $
    --  (center (str " ") <+> vBorder <+> center (str " "))

newX :: Int -> Xvalue
newX xRand = 
    if xRand == 0
        then Derecha
        else Izquierda

newY :: Int -> Yvalue
newY yRand = 
    if yRand == 0
        then Arriba
        else Abajo

main :: IO ()
main = do
    _ <- playGame
    putStrLn "Thanks for playing Pong!"