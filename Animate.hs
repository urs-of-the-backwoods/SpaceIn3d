{-# LANGUAGE OverloadedStrings #-}

module Animate (
        newAnimateActor
    ) where

import HGamer3D

import qualified Data.Traversable as Tr
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data
import Actor


-- ANIMATION ACTOR
-- ---------------

type AnaR = Keys
type AnaS = (Int, GameData)

mapAccumLM f a xs = runStateT (Tr.mapM (StateT . f) xs) a

newAnimateActor :: Keys -> GameData -> IO Actor
newAnimateActor keys gameData = do
    actor <- newActor
    runActor actor animateActorF keys (0, gameData)
    return actor

animateActorF :: Actor -> Message -> ReaderStateIO AnaR AnaS ()
animateActorF animA msg = do

    (count, gameData) <- get
    put (count + 1, gameData)

    case msg of
        SlowCycle -> animateLevel
        _ -> return ()

-- HGamer3D website, space invaders, animate
animateLevel :: ReaderStateIO AnaR AnaS ()
animateLevel = do

    keys <- lift ask
    let (kent, kdim, kpos, khits, kanim, kuni) = keys
    (count, gameData) <- get

    (gameData', _) <- liftIO $ mapAccumLM (\(nodeType, nodeData) (nt, nd) -> case nodeType of
                (Invader _) -> do
                    let ai = nodeData ! kanim
                    let ai' = getCurrentAnimation ai count
                    let nodeData' = setData kanim ai' nodeData
                    return ((nodeType, nodeData'), (nodeType, nodeData')) -- set acc to nodeData of Invader

                PixelA -> do
                    let ai = nd ! kanim         -- accu, this is the previous Invader node info!
                    if aiSwapNow ai
                        then do
                            let (x, y) = nodeData ! kpos
                            case aiType ai of
                                PixelA -> setC (nodeData ! kent) ctPosition $ relativePosFromPixelPos (nd ! kdim) (x, y)
                                PixelB -> setC (nodeData ! kent) ctPosition (Vec3 (-1000.0) 0 0)
                                _ -> return ()
                        else return ()
                    return ((nodeType, nodeData), (nt, nd))

                PixelB ->  do
                    let ai = nd ! kanim         -- accu, this is the previous Invader node info!
                    if aiSwapNow ai
                        then do
                            let (x, y) = nodeData ! kpos
                            case aiType ai of
                                PixelB -> setC (nodeData ! kent) ctPosition $ relativePosFromPixelPos (nd ! kdim) (x, y)
                                PixelA -> setC (nodeData ! kent) ctPosition (Vec3 (-1000.0) 0 0)
                                _ -> return ()
                        else return ()
                    return ((nodeType, nodeData), (nt, nd))

                _ -> return ((nodeType, nodeData), (nt, nd)) -- noch changes

            ) (undefined, undefined) gameData

    put (count, gameData')
    return ()
-- end of website text
