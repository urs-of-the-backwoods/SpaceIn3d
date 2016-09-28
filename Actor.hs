{-# LANGUAGE OverloadedStrings #-}

module Actor where

import HGamer3D

import Data

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Unique


-- ACTORS
-- ------

-- define all messages in this program, high level between actors
-- HGamer3D website, space invaders, messages
data Message = StartProgram
             StartMusic | StopMusic | PlayShot | PlayNoShot | PlayExplosion | PlayStep
             | KeysPressed [T.Text] | SingleKey T.Text | PollKeys 
             | FastCycle | SlowCycle 
             | MoveLeft | MoveRight | Shoot 
             | RollRight | RollLeft | PitchUp | PitchDown | YawLeft | YawRight | MoreSpeed | LessSpeed | ZeroSpeed  
             | ResetCamPosition | RestoreCamPosition | SaveCamPosition
             | DisplayStatus | HideStatus | AddCount Int | SetMode T.Text 
             | ActualInvaderData GameData | ActualCanonData GameData | ActualCollData [Unique] 
             | CanonStep GameData [Unique] | MoveStep GameData [Unique] | CollisionStep GameData GameData
             | GameLostOverrun | GameWon
-- end of website text

-- HGamer3D website, space invaders, actors
newtype Actor = Actor (MVar Message) 

newActor :: IO Actor
newActor = do
    mv <- newEmptyMVar
    return (Actor mv)

type ReaderStateIO r s a = StateT s (ReaderT r IO) a

runActor :: Actor -> (Actor -> Message -> ReaderStateIO r s () ) -> r -> s -> IO ()
runActor a@(Actor mv) f ri si = do
    let loop mv s = do
            msg <- takeMVar mv
            (_, s') <- runReaderT (runStateT (f a msg) s) ri
            loop mv s'
    forkIO $ loop mv si
    return ()

sendMsg :: Actor -> Message -> IO ()
sendMsg (Actor mv) m = putMVar mv m
-- end of website text
