{-# LANGUAGE OverloadedStrings #-}

module GameLoop (
        newGameLoopActor
    ) where

import HGamer3D

import Data
import Actor
import Move
import Canon
import Collision


import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Data.Unique
import Data.Maybe


-- GAMELOOP ACTOR
-- --------------
-- runs multiple computations in parallel, by invoking three other actors:
-- move actor: movement of invaders
-- canon actor: movement of canon
-- coll actor: collision detection
-- 

-- HGamer3D website, space invaders, gameloop actor
data MyActors = MyActors {
    moveA :: Actor,
    canonA :: Actor,
    collA :: Actor,
    switchA :: Actor
}

type GlaR = MyActors
type GlaS = (Maybe GameData, Maybe GameData, Maybe [Unique])

newGameLoopActor :: Actor -> Actor -> Actor -> Keys -> GameData -> GameData -> IO Actor
newGameLoopActor switchA musicA statusA keys invaders canons = do

    actor <- newActor

    collA <- newCollisionActor actor keys
    moveA <- newMoveActor collA musicA statusA keys
    canonA <- newCanonActor collA musicA keys

    runActor actor gameLoopActorF (MyActors moveA canonA collA switchA) (Nothing, Nothing, Just [])
    return actor


gameLoopActorF :: Actor -> Message -> ReaderStateIO GlaR GlaS ()
gameLoopActorF loopA msg = do

    myActors <- lift ask
    (slotMoveData, slotCanonData, slotCollData) <- get

    case msg of

        ActualCanonData canonData -> put (slotMoveData, Just canonData, slotCollData) >> return ()
        ActualInvaderData moveData -> put (Just moveData, slotCanonData, slotCollData) >> return ()
        ActualCollData collData -> put (slotMoveData, slotCanonData, Just collData) >> return ()

        FastCycle -> do
            case (slotMoveData, slotCanonData, slotCollData) of
                (Just moveData, Just canonData, Just collData) -> do
        --                            liftIO $ print "good cycle"
                    liftIO $ sendMsg (collA myActors) (CollisionStep canonData moveData)
                    liftIO $ sendMsg (canonA myActors) (CanonStep canonData collData)
                    liftIO $ sendMsg (moveA myActors) (MoveStep moveData collData)
                    put (Nothing, Nothing, Nothing)
                    return ()
                _ -> do
                    liftIO $ print "missed cycle"
                    return ()

        MoveLeft -> liftIO $ sendMsg (canonA myActors) MoveLeft
        MoveRight -> liftIO $ sendMsg (canonA myActors) MoveRight
        Shoot -> liftIO $ sendMsg (canonA myActors) Shoot

        GameLostOverrun -> liftIO $ sendMsg (switchA myActors) GameLostOverrun
        GameWon -> liftIO $ sendMsg (switchA myActors) GameWon

        _ -> return ()
-- end of website text



  