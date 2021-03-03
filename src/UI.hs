{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module UI where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Lens
import Control.Concurrent.STM

type Name = ()

data Game = Game
    {   _scorePlayerOne :: Int
    ,   _scorePlayerTwo :: Int
    } deriving (Eq, Show)
makeLenses ''Game

data Xvalue = Derecha | Izquierda
    deriving (Eq, Show)
data Yvalue = Arriba | Abajo
    deriving (Eq, Show)



data Tick = Tick

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
        
        8   ->  [gameOverClassic ui]
        9   ->  [gameOverMachine ui]
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

            gameOverClassic ui' = 
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                $ padTopBottom 2
                $ hCenter gameOverTitle
                <=> center (getClassicWinner ui')
                --(str "Final score: " <+> str (show (ui ^. game . scorePlayerOne)))
                <=> hCenter (padBottom Max (str "\n\n\n(b): Main menu"))

            gameOverMachine ui' = 
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                $ padTopBottom 2
                $ hCenter gameOverTitle
                <=> center (getMachineWinner ui')
                <=> hCenter (padBottom Max (str "\n\n\n(b): Main menu"))

            gameOverWall     =
                setAvailableSize (80, 24)
                $ hLimit 80
                $ vLimit 24
                $ withBorderStyle unicode
                $ borderWithLabel (str "Pong")
                $ padTopBottom 2
                $ hCenter gameOverTitle
                <=> padTop (Pad 2) (center (str "Final score: " <+> str (show (ui ^. game . scorePlayerOne))))
                <=> hCenter (padBottom Max (str "\n\n\n(b): Main menu"))

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



getClassicWinner :: UI -> Widget Name
getClassicWinner ui
    |   ui ^. game . scorePlayerOne > ui ^. game . scorePlayerTwo   = hCenter (str "\n\n\n\nPlayer One wins!\n\n") <=> hCenter (str (show (ui ^. game . scorePlayerOne)) <+> str " - " <+> str (show (ui ^. game . scorePlayerTwo)))
    |   otherwise   = hCenter (str "\n\n\n\nPlayer Two wins!\n\n") <=> hCenter (str (show (ui ^. game . scorePlayerOne)) <+> str " - " <+> str (show (ui ^. game . scorePlayerTwo)))

getMachineWinner :: UI -> Widget Name
getMachineWinner ui
    |   ui ^. game . scorePlayerOne > ui ^. game . scorePlayerTwo   = hCenter (str "\n\n\n\nPlayer One wins!\n\n") <=> hCenter (str (show (ui ^. game . scorePlayerOne)) <+> str " - " <+> str (show (ui ^. game . scorePlayerTwo)))
    |   otherwise   = hCenter (str "\n\n\n\nThe machine wins!\n\n") <=> hCenter (str (show (ui ^. game . scorePlayerTwo)) <+> str " - " <+> str (show (ui ^. game . scorePlayerOne)))

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

barAttr, ballAttr :: AttrName
barAttr     = "barAttr"
ballAttr    = "ballAttr"