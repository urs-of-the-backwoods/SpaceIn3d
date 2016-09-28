{-# LANGUAGE OverloadedStrings #-}

module Switch (
        newSwitchActor
    ) where

import HGamer3D

import Data
import Actor
import Input
import GameLoop
import Music
import Status
import Fly
import Animate


import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

-- SWITCH ACTOR
-- ------------
-- switchA needs to be fast to be repsonsive to single keys
-- 

data MyActors = MyActors {
    keyA :: Actor,
    musicA :: Actor,
    gameLoopA :: Actor,
    animateA :: Actor,
    flyingA :: Actor,
    statusBarA :: Actor
}

type GsaR = (HG3D)
type GsaS = (MyActors, TextData, T.Text, GameState)

newSwitchActor :: HG3D -> Entity -> IO Actor
newSwitchActor hg3d cam = do
    flyA <- newFlyingActor cam
    actor <- newActor
    runActor actor gameSwitchActorF hg3d (MyActors undefined undefined undefined undefined flyA undefined, undefined, undefined, ProgramInitializing)
    return actor 

gameSwitchActorF :: Actor -> Message -> ReaderStateIO GsaR GsaS ()
gameSwitchActorF switchA msg = do

    hg3d <- lift ask
    (myActors, startScreenText, name, gameState) <- get

    let returnStay = return () 
    let returnMoveTo state = put (myActors, startScreenText, name, state) >> return ()

    -- HGamer3D website, space invaders, switch actor
    case gameState of

        ProgramInitializing -> 
            case msg of
                StartProgram -> do
                    -- initialize music, keys and start screen
                    mA <- liftIO $ newMusicActor hg3d
                    kA <- liftIO $ newKeyActor hg3d switchA
                    startScreenText' <- liftIO $ showInitScreen hg3d
                    liftIO $ sendMsg mA StartMusic
                    -- create hearbeat of program
                    let cycleLoop n m = do
                            if n == 0 
                                then sendMsg switchA SlowCycle
                                else return ()
                            sendMsg kA PollKeys
                            sendMsg switchA FastCycle
                            sleepFor (msecT 30)
                            cycleLoop (if n == 0 then m else n - 1) m
                    liftIO $ forkIO $ cycleLoop 0 10

                    put (myActors {keyA = kA, musicA = mA}, startScreenText', name, InitScreen)
                _ -> returnStay
    -- end of website text

        InitScreen -> 

            case msg of

                SingleKey k -> do
                    if k == "Return" 
                        then do
                            -- hide start screen, stop music
                            name' <- liftIO $ getName startScreenText
                            liftIO $ hideInitScreen startScreenText
                            liftIO $ sendMsg (musicA myActors) StopMusic
                            -- initialize status bar
                            sA <- liftIO $ newStatusBarActor hg3d 0 name' "building"
                            -- initialize game data
                            keys <- liftIO $ genKeys
                            invaders <- liftIO $ gameDataFromBuildData hg3d keys buildInvadersData
                            canons <- liftIO $ gameDataFromBuildData hg3d keys buildCanonData
                            -- start gameloop actors and animation
                            glA <-  liftIO $ newGameLoopActor switchA (musicA myActors) sA keys invaders canons
                            aA <- liftIO $ newAnimateActor keys invaders
                            put (myActors {gameLoopA = glA, animateA = aA, statusBarA = sA}, startScreenText, name', PlayGame) >> return ()
                            -- send data to gameloop actor
                            liftIO $ sendMsg glA $ ActualInvaderData invaders
                            liftIO $ sendMsg glA $ ActualCanonData canons
                            liftIO $ sendMsg sA (SetMode "playing")
                        else returnStay

                _ -> returnStay


        PlayGame -> do

--            liftIO $ print ((isJust slotMoveData), (isJust slotCanonData), (isJust slotCollData)) 
            case msg of

                SlowCycle -> liftIO $ sendMsg (animateA myActors) SlowCycle
                FastCycle -> liftIO $ sendMsg (gameLoopA myActors) FastCycle
                SingleKey k -> do
                    case k of
                        "Space" -> do
                            liftIO $ sendMsg (gameLoopA myActors) Shoot
                            returnStay
                        "F1" -> liftIO (sendMsg (statusBarA myActors) (SetMode "paused ...")) >> returnMoveTo Flying
                        "F2" -> liftIO (sendMsg (flyingA myActors) ResetCamPosition) >> returnStay
                        _ -> returnStay

                KeysPressed keys -> do
                    if ("Left" `elem` keys) && (not ("Right" `elem` keys))
                        then do
                            liftIO $ sendMsg (gameLoopA myActors) MoveLeft
                            returnStay
                        else returnStay

                    if ("Right" `elem` keys) && (not ("Left" `elem` keys)) 
                        then do
                            liftIO $ sendMsg (gameLoopA myActors) MoveRight
                            returnStay
                        else returnStay


                GameLostOverrun -> do
                    liftIO $ sendMsg (statusBarA myActors) (SetMode "game lost!")
                    liftIO $ showLost hg3d
                    returnMoveTo FinalScore

                GameWon -> do
                    liftIO $ sendMsg (statusBarA myActors) (SetMode "game won!")
                    liftIO $ showWon hg3d
                    returnMoveTo FinalScore

                _ -> returnStay


        Flying -> 

            case msg of

                SlowCycle -> liftIO $ sendMsg (animateA myActors) SlowCycle

                SingleKey k -> do
                    case k of
                        "F1" -> liftIO (sendMsg (statusBarA myActors) (SetMode "playing")) >> returnMoveTo PlayGame
                        "W" -> liftIO (sendMsg (flyingA myActors) MoreSpeed) >> returnStay
                        "S" -> liftIO (sendMsg (flyingA myActors) LessSpeed) >> returnStay
                        "Q" -> liftIO (sendMsg (flyingA myActors) ZeroSpeed) >> returnStay
                        "F2" -> liftIO (sendMsg (flyingA myActors) ResetCamPosition) >> returnStay
                        "F3" -> liftIO (sendMsg (flyingA myActors) SaveCamPosition) >> returnStay
                        "F4" -> liftIO (sendMsg (flyingA myActors) RestoreCamPosition) >> returnStay
                        _ -> returnStay

                KeysPressed keys -> do
                    mapM (\k -> do
                        case k of
                            "A" -> liftIO $ sendMsg (flyingA myActors) YawLeft
                            "D" -> liftIO $ sendMsg (flyingA myActors) YawRight
                            "Up" -> liftIO $ sendMsg (flyingA myActors) PitchUp 
                            "Down" -> liftIO $ sendMsg (flyingA myActors) PitchDown
                            "Left" -> liftIO $ sendMsg (flyingA myActors) RollLeft 
                            "Right" -> liftIO $ sendMsg (flyingA myActors) RollRight
                            _ -> return ()
                        ) keys
                    returnStay

                _ -> returnStay



        FinalScore ->
            case msg of

                SlowCycle -> liftIO $ sendMsg (animateA myActors) SlowCycle

                _ -> returnStay                



data TextData = TextData Entity Entity Entity Entity Entity

getName :: TextData -> IO T.Text
getName (TextData _ _ _ _ eName) = readC eName ctEditText

hideInitScreen :: TextData -> IO ()
hideInitScreen (TextData e1 e2 e3 e4 eName) = mapM (\e -> setC e ctScreenRect (Rectangle (-1000) (-1000) 0 0)) [e1, e2, e3, e4, eName] >> return ()

showResultScreen :: HG3D -> T.Text -> IO ()
showResultScreen hg3d result = do

    eT1 <- newE hg3d [
        ctText #: result,
        ctScreenRect #: Rectangle 250 200 100 25
        ]

    sleepFor (secT 5)
    return ()

showWon hg3d = showResultScreen hg3d "Congratulation, you made it!"
showLost hg3d = showResultScreen hg3d "You lost, try again!"


showInitScreen :: HG3D -> IO TextData
showInitScreen hg3d = do

    eT1 <- newE hg3d [
        ctText #: "Space Invaders 3D",
        ctScreenRect #: Rectangle 250 100 100 25
        ]

    eT2 <- newE hg3d [
        ctText #: "programmed by: Peter Althainz\nusing the fabulous HGamer3D toolset\nSeptember 2016\n\nhttp://www.hgamer3d.org",
        ctScreenRect #: Rectangle 220 150 100 60
        ]

    eT3 <- newE hg3d [
        ctText #: "dear brave hero, please type in your name: ",
        ctScreenRect #: Rectangle 220 250 100 60
        ]

    eName <- newE hg3d [
        ctEditText #: "The Brave Hero",
        ctScreenRect #: Rectangle 220 280 200 30
        ]

    eT4 <- newE hg3d [
        ctText #: (T.pack . unlines $ [
            "Keys:",
            "F1 - switch pause flight mode - play mode",
            "F2 - reset camera position",
            "F3 - store camera position (flight mode)",
            "F4 - restore camera position (flight mode)\n",
            "Flight Mode: WSADQ Up/Down/Left/Right",
            "Play Mode: Left/Right/Space" ]),
        ctScreenRect #: Rectangle 220 350 100 60
        ]

    return $ TextData eT1 eT2 eT3 eT4 eName


buildInvadersData :: [BuildElement]
buildInvadersData = [
        BEOne Ship (0, 100),
        BERow Boulder (-45, -50) 40 4,
        BERow (Invader 3) (-60, 85) 15 11,
        BERow (Invader 2) (-60, 70) 15 11,
        BERow (Invader 2) (-60, 55) 15 11,
        BERow (Invader 1) (-60, 40) 15 11,
        BERow (Invader 1) (-60, 25) 15 11
    ]

buildCanonData :: [BuildElement]
buildCanonData = [
        BEOne Canon (0, -65),
        BERow Canon (-60, -80) 15 2,
        BERow Shot (-1000, 0) 5 5
    ]

