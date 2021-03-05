module Pong where

import UI

import Brick
import Brick.BChan
import Control.Lens
import qualified Graphics.Vty as V
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import System.Random

--App's Initialization
app :: App UI Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

--Moves the ball one step to the top
ballUp :: UI -> UI
ballUp ui =
    ui & ball . locationRowL %~ subtract 1

--Moves the ball one step to the left
ballLeft :: UI -> UI
ballLeft ui =
    ui & ball . locationColumnL %~ subtract 1

--Moves the ball one step to the bottom
ballDown :: UI -> UI
ballDown ui =
    ui & ball . locationRowL %~ (+ 1)

--Moves the ball one step to the right
ballRight :: UI -> UI
ballRight ui =
    ui & ball . locationColumnL %~ (+ 1)

--Controls every step of the time
handleTick :: UI -> EventM Name (Next UI)
handleTick ui
    |   ui ^. status == 0 =  continue ui                                             -- if the game is paused
    |   ui ^. ball . locationColumnL < ui ^. barPlayerOne . locationColumnL =        -- if player two scores a point
            if ui ^. game . scorePlayerTwo < 6                                       -- if this is the 7th point of the player, the game ends
                then continue  $ reset $ pointTwo ui
                else continue $ pointTwo $ ui & status .~ 8
    |   ui ^. ball . locationColumnL > ui ^. barPlayerTwo . locationColumnL =        -- if player one scores a point
            if ui ^. game . scorePlayerOne < 6                                       -- if this is the 7th point of the player, the game ends
                then continue $ reset $ pointOne ui
                else continue $ pointOne $ ui & status .~ 8
    |   otherwise =                                                                  -- if none of the players scores a point, the ball will keep moving
            case ui ^. ball . locationRowL of
                1   -> continue $ uiFinal $ borderTop $ touchBar ui             -- when the ball touches the top, changes its directions to the bottom
                22  -> continue $ uiFinal $ borderBottom $ touchBar ui             -- when the ball touches the bottom, changes its directions to the top
                _   -> continue $ uiFinal $ touchBar ui                             -- otherwise, keeps moving

                where
                    uiHorizontal ui'= if (ui' ^. xBall) == Izquierda then ballLeft ui' else ballRight ui'
                    uiFinal ui' = if (ui' ^. yBall) == Arriba then ballUp $ uiHorizontal ui' else ballDown $ uiHorizontal ui'

--Increases the player one score on 1
pointOne :: UI -> UI
pointOne ui = ui & game . scorePlayerOne %~ (+ 1)

--Increases the player two score on 1
pointTwo :: UI -> UI
pointTwo ui = ui & game . scorePlayerTwo %~ (+ 1)

--Controls time steps on 'against machine' mode
handleTickMachine :: UI -> EventM Name (Next UI)
handleTickMachine ui
    |   ui ^. status == 0 = continue ui 
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
                1   -> continue $ uiFinal $ borderTop $ touchBar ui
                22  -> continue $ uiFinal $ borderBottom $ touchBar ui
                _   -> continue $ uiFinal $ touchBar ui

                where
                    uiHorizontal ui'= if (ui' ^. xBall) == Izquierda then ballLeft ui' else ballRight ui'
                    --The machine bars move up or down so does the ball
                    uiFinal ui' = if (ui' ^. yBall) == Arriba then ballUp $ uiHorizontal $ machineBarUp ui' else ballDown $ uiHorizontal $ machineBarDown ui'  

--If the ball is in the machine field, the bar will folow the ball's directions on the Y axis
machineBarUp :: UI -> UI
machineBarUp ui =
    if (((ui ^. xBall) == Derecha) && ((ui ^. ball . locationColumnL) > 39)) && ((ui ^. barPlayerTwo . locationRowL) > 0) then ui & barPlayerTwo . locationRowL %~ subtract 1 else ui

--If the ball is in the machine field, the bar will folow the ball's directions on the Y axis
machineBarDown :: UI -> UI
machineBarDown ui =
    if (((ui ^. xBall) == Derecha) && ((ui ^. ball . locationColumnL) > 39)) && ((ui ^. barPlayerTwo . locationRowL) < 18) then ui & barPlayerTwo . locationRowL %~ (+ 1) else ui

--Controls time steps on 'against wall' mode
handleTickWall :: UI -> EventM Name (Next UI)
handleTickWall ui
    |   ui ^. status == 0 = continue ui
    |   ui ^. ball . locationColumnL < ui ^. barPlayerOne . locationColumnL = continue $ ui & status .~ 10                                    -- GAME OVER
    |   ui ^. ball . locationColumnL == 77 = continue $ uiFinal $ pointOne ui & xBall .~ Izquierda                                            -- The player one scores when the ball touches the right wall
                                                                                                                                              -- And ball direction changes
    |   otherwise =
            case ui ^. ball . locationRowL of
                1   -> continue $ uiFinal $ borderTop $ touchBarWall ui
                22  -> continue $ uiFinal $ borderBottom $ touchBarWall ui
                _   -> continue $ uiFinal $ touchBarWall ui

                where
                    uiHorizontal ui'= if (ui' ^. xBall) == Izquierda then ballLeft ui' else ballRight ui'
                    uiFinal ui' = if (ui' ^. yBall) == Arriba then ballUp $ uiHorizontal ui' else ballDown $ uiHorizontal ui'

-- Reset the locations of the bars and the ball when someone scores a point
reset :: UI -> UI
reset ui = UI
    { _game             = ui ^. game
    , _barPlayerOne     = Location (1, 9)
    , _barPlayerTwo     = Location (76, 9)
    , _ball             = Location (39, 12)
    , _xBall            = ui ^. xBall 
    , _yBall            = ui ^. yBall
    , _status           = ui ^. status
    , _previousStatus   = ui ^. previousStatus
    , _level            = ui ^. level
    }

--Verifies if the ball is touching any bar    
touchBar :: UI -> UI
touchBar ui
    |   ((ui ^. ball . locationRowL >= ui ^. barPlayerOne . locationRowL) && (ui ^. ball . locationRowL <= (ui ^. barPlayerOne . locationRowL)+5)) && ((ui ^. ball . locationColumnL >= ui ^. barPlayerOne . locationColumnL) && (ui ^. ball . locationColumnL <= (ui ^. barPlayerOne . locationColumnL)+3)) = ui & xBall .~ Derecha
    |   ((ui ^. ball . locationRowL >= ui ^. barPlayerTwo . locationRowL) && (ui ^. ball . locationRowL <= (ui ^. barPlayerTwo . locationRowL)+5)) && ((ui ^. ball . locationColumnL <= ui ^. barPlayerTwo . locationColumnL) && (ui ^. ball . locationColumnL >= (ui ^. barPlayerTwo . locationColumnL)-2)) = ui & xBall .~ Izquierda
    |   otherwise = ui

--Only verifies if the ball is touching the player's bar on the wall mode
touchBarWall :: UI -> UI
touchBarWall ui = 
    if ((ui ^. ball . locationRowL >= ui ^. barPlayerOne . locationRowL) && (ui ^. ball . locationRowL <= (ui ^. barPlayerOne . locationRowL)+5)) && ((ui ^. ball . locationColumnL >= ui ^. barPlayerOne . locationColumnL) && (ui ^. ball . locationColumnL <= (ui ^. barPlayerOne . locationColumnL)+3))
        then ui & xBall .~ Derecha
        else ui

--Set the ball's directions to the bottom (Because is touching the top border)
borderTop :: UI -> UI
borderTop ui = ui & yBall .~ Abajo

--Set the ball's directions to the top (Because is touching the bottom border)
borderBottom :: UI -> UI
borderBottom ui = ui & yBall .~ Arriba

-- Sets the current status as the previous
setPreviousStatus :: UI -> UI
setPreviousStatus ui =
    ui & previousStatus .~ (ui ^. status)


handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)
handleEvent ui event =
    case ui ^. status of
        --  Paused game
        0   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue pause
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue pause
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        pause = ui & status .~ (ui ^. previousStatus)   --  Vuelve al estado en el que estaba.
        --  Main screen
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

        --  Select classic level
        2   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar '1') []))   -> setLevel ui 150000 3
                    (VtyEvent (V.EvKey (V.KChar '2') []))   -> setLevel ui 80000 3
                    (VtyEvent (V.EvKey (V.KChar '3') []))   -> setLevel ui 50000  3
                    
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        --  Playing classic mode
        3   ->  case event of
                    --  Pause or quit the game
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue $ pauseGame $ setPreviousStatus ui
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue $ pauseGame $ setPreviousStatus ui
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    --  Tick
                    (AppEvent Tick)                         -> handleTick ui
                    --  Controls
                    (VtyEvent (V.EvKey V.KDown []))         -> continue playerTwoMoveDown
                    (VtyEvent (V.EvKey V.KUp []))           -> continue playerTwoMoveUp
                    (VtyEvent (V.EvKey (V.KChar 's') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'S') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'w') []))   -> continue $ playerOneMoveUp ui
                    (VtyEvent (V.EvKey (V.KChar 'W') []))   -> continue $ playerOneMoveUp ui
                    --  Any other key
                    _                                   -> continue ui
                    where
                        goBack ui' = ui' & status .~ 2
                        playerTwoMoveDown = if (ui ^. barPlayerTwo . locationRowL) < 18 then ui & barPlayerTwo . locationRowL %~ (+ 1) else ui
                        playerTwoMoveUp = if (ui ^. barPlayerTwo . locationRowL) > 0 then ui & barPlayerTwo . locationRowL %~ subtract 1 else ui

        --  Select machine level
        4   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar '1') []))   -> setLevel ui 150000 5
                    (VtyEvent (V.EvKey (V.KChar '2') []))   -> setLevel ui 80000 5
                    (VtyEvent (V.EvKey (V.KChar '3') []))   -> setLevel ui 50000 5

                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui

                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        --  Playing against the machine
        5   ->  case event of
                    --  Pause or quit the game
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue $ pauseGame $ setPreviousStatus ui
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue $ pauseGame $ setPreviousStatus ui
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    --  Tick
                    (AppEvent Tick)                         -> handleTickMachine ui 
                    --  Controls
                    (VtyEvent (V.EvKey (V.KChar 's') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'S') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'w') []))   -> continue $ playerOneMoveUp ui
                    (VtyEvent (V.EvKey (V.KChar 'W') []))   -> continue $ playerOneMoveUp ui
                    --Any other key
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 4

         --  Select wall level
        6   ->  case event of
                    (VtyEvent (V.EvKey (V.KChar '1') []))   -> setLevel ui 150000 7
                    (VtyEvent (V.EvKey (V.KChar '2') []))   -> setLevel ui 80000 7
                    (VtyEvent (V.EvKey (V.KChar '3') []))   -> setLevel ui 50000 7

                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui

                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 1

        --  Playing against the Wall
        7   ->  case event of
                    --  Pause or quit the game
                    (VtyEvent (V.EvKey (V.KChar 'p') []))   -> continue $ pauseGame $ setPreviousStatus ui
                    (VtyEvent (V.EvKey (V.KChar 'P') []))   -> continue $ pauseGame $ setPreviousStatus ui
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    --  Tick
                    (AppEvent Tick)                         -> handleTickWall ui 
                    --  Controls
                    (VtyEvent (V.EvKey (V.KChar 's') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'S') []))   -> continue $ playerOneMoveDown ui
                    (VtyEvent (V.EvKey (V.KChar 'w') []))   -> continue $ playerOneMoveUp ui
                    (VtyEvent (V.EvKey (V.KChar 'W') []))   -> continue $ playerOneMoveUp ui
                    -- Any other key
                    _                                       -> continue ui
                    where
                        goBack ui' = ui' & status .~ 6

        --  Game Over Classic
        8   ->  case event of
                    --  Quit game
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    _   ->  continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        --  Game Over Machine
        9   ->  case event of
                    --  Quit game
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    _   ->  continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        --  Game Over Wall
        10  ->  case event of
                    --  Quit game
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    _   ->  continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        --  Instructions
        11  ->  case event of
                    --  Quit game
                    (VtyEvent (V.EvKey (V.KChar 'q') []))   -> halt ui
                    (VtyEvent (V.EvKey (V.KChar 'Q') []))   -> halt ui
                    --  Go back
                    (VtyEvent (V.EvKey (V.KChar 'b') []))   -> continue $ goBack $ resetScores ui
                    (VtyEvent (V.EvKey (V.KChar 'B') []))   -> continue $ goBack $ resetScores ui
                    _   ->  continue ui
                    where
                        goBack ui' = ui' & status .~ 1
        _   ->  halt ui

-- Reset the locations of the bars and the ball, and both players' scores
resetScores :: UI -> UI
resetScores ui = UI
    { _game             = Game { _scorePlayerOne = 0, _scorePlayerTwo = 0 }
    , _barPlayerOne     = Location (1, 9)
    , _barPlayerTwo     = Location (76, 9)
    , _ball             = Location (39, 12)
    , _xBall            = ui ^. xBall
    , _yBall            = ui ^. yBall
    , _status           = ui ^. status
    , _previousStatus   = ui ^. previousStatus
    , _level            = ui ^. level
    }

--Moves player one's bar to the bottom when S key is pressed
playerOneMoveDown :: UI -> UI
playerOneMoveDown ui =
    if (ui ^. barPlayerOne . locationRowL) < 18
        then ui & barPlayerOne . locationRowL %~ (+ 1)
        else ui

--Moves player one's bar to the top when W key is pressed
playerOneMoveUp :: UI -> UI
playerOneMoveUp ui =
    if (ui ^. barPlayerOne . locationRowL) > 0
        then ui & barPlayerOne . locationRowL %~ subtract 1
        else ui

--Pause the game
pauseGame :: UI -> UI
pauseGame ui =
    ui & status .~ 0

--  Edit the 'levle' attribute to set the time delay
setLevel :: UI -> Int -> Int -> EventM n (Next UI)
setLevel ui lvl mode = do
    liftIO $ atomically $ writeTVar (ui ^. level) lvl
    continue $ ui & status .~ mode

--This map defines the styles for every game element
theMap :: AttrMap
theMap = attrMap
    V.defAttr
    [   (barAttr    , V.white `on` V.white)
    ,   (ballAttr   , V.white `on` V.white)
    ]

--Initializes both players scores on 0 
initGame :: IO Game
initGame = 
    pure $ Game
        {   _scorePlayerOne = 0
        ,   _scorePlayerTwo = 0
        }

--Creates the custom main with the initial UI and delay
playGame :: IO UI
playGame = do
    chan <- newBChan 10
    tv   <- atomically $ newTVar 0
    _ <- forkIO $ forever $ do
        writeBChan chan Tick
        int <- readTVarIO tv
        threadDelay int
    initialGame <- initGame
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    xRand <- randomRIO (0,1)                                             -- Gets a random number between 0 and 1
    yRand <- randomRIO (0,1)                                             -- Gets another random number between 0 and 1
    ui <- customMain initialVty buildVty (Just chan) app UI
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
    return $ ui

--Returns a Xvalue according to the random number given
newX :: Int -> Xvalue
newX xRand = 
    if xRand == 0
        then Derecha
        else Izquierda

--Returns a Yvalue according to the random number given
newY :: Int -> Yvalue
newY yRand = 
    if yRand == 0
        then Arriba
        else Abajo

