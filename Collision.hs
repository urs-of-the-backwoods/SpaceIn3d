{-# LANGUAGE OverloadedStrings #-}

module Collision (
        newCollisionActor
    ) where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import qualified Data.Traversable as Tr
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Tree
import Data.Maybe

import Data
import Actor

-- COLLISION ACTOR
-- ---------------
-- computes collisions and sends back info to other actors 

type CoaR = (Actor, Keys)
type CoaS = (Maybe GameData, Maybe GameData)  

newCollisionActor :: Actor -> Keys -> IO Actor
newCollisionActor gameLoopA keys = do
    actor <- newActor
    runActor actor collisionActorF (gameLoopA, keys) (Nothing, Nothing)
    return actor

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

collisionActorF :: Actor -> Message -> ReaderStateIO CoaR CoaS ()
collisionActorF collA msg = do

    (gameLoopA, keys) <- lift ask
    (slotMoveData, slotCanonData) <- get

    case msg of

        ActualCanonData canonData -> if (isJust slotMoveData) 
            then do
                let moveData = fromJust slotMoveData
                runCollisionDetection gameLoopA keys moveData canonData
                return ()
            else do
                put (slotMoveData, Just canonData)
                return ()

        ActualInvaderData moveData ->  if (isJust slotCanonData) 
            then do
                let canonData = fromJust slotCanonData
                runCollisionDetection gameLoopA keys moveData canonData
                return ()
            else do
                put (Just moveData, slotCanonData)
                return ()

        _ -> return ()


isCollision :: Keys -> NodeData -> NodeData -> Bool
isCollision keys i s = let
    (kent, kdim, kpos, khits, kanim, kuni) = keys
    (x, y) = i ! kpos
    (w, h) = (diWidth (i ! kdim), diHeight (i ! kdim))
    (x', y') = s ! kpos
    (w', h') = (diWidth (s ! kdim), diHeight (s ! kdim))

    xmin = min (x - (w `div` 2)) (x' - (w' `div` 2))
    xmax = max (x + (w `div` 2)) (x' + (w' `div` 2))
    ymin = min (y - abs (h `div` 2)) (y' - abs (h' `div` 2))
    ymax = max (y + abs (h `div` 2)) (y' + abs (h' `div` 2))

    wg = xmax - xmin
    hg = ymax - ymin

    coll = wg <= (w' + w) && hg <= (h' + h)
    in coll

-- removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

runCollisionDetection :: Actor -> Keys -> GameData -> GameData -> ReaderStateIO CoaR CoaS ()
runCollisionDetection gameLoopA keys invaderData canonData = do
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    let invaderList = flatten invaderData
    let invaders = filter (\(nt, nd) -> case nt of
            (Invader _) -> let (x, y) = nd ! kpos in if x < (-500) then False else True
            _ -> False
            ) invaderList

    if length invaders == 0
        then do
            liftIO $ sendMsg gameLoopA GameWon
            return ()
        else do
            let boulders = filter (\(nt, nd) -> case nt of
                    Boulder -> let (x, y) = nd ! kpos in if x < (-500) then False else True
                    _ -> False
                    ) invaderList
            let invsAndBoulders = invaders ++ boulders
            let shots = filter (\(nt, nd) -> nt == Shot) (flatten canonData)

            -- collision invader boulder -> game end
            let colsBoulders = [ (s ! kuni, i ! kuni) | (_, s) <- boulders, (_, i) <- invaders, isCollision keys i s]
            if length colsBoulders > 0
                then do
                    liftIO $ sendMsg gameLoopA GameLostOverrun
                    return ()
                else do
                    let cols = concatMap (\(a, b) -> [a, b]) [ (s ! kuni, i ! kuni) | (_, s) <- shots, (_, i) <- invsAndBoulders, isCollision keys i s]

                    liftIO $ sendMsg gameLoopA $ ActualInvaderData invaderData 
                    liftIO $ sendMsg gameLoopA $ ActualCanonData canonData 
                    liftIO $ sendMsg gameLoopA $ ActualCollData cols
                    put (Nothing, Nothing)
                    return ()
