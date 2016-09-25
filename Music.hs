{-# LANGUAGE OverloadedStrings #-}

module Music (
        newMusicActor
    ) where

import HGamer3D

import qualified Data.Traversable as Tr
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data
import Actor

-- MUSIC ACTOR
-- -----------

type MaR = (Entity, Entity, Entity, Entity)
type MaS = ()

newMusicActor :: HG3D -> IO Actor
newMusicActor hg3d = do
    music <- newE hg3d [ 
        ctSoundSource #: Music "Sounds/RMN-Music-Pack/OGG/CD 3 - Clash of Wills/3-04 Joyful Ocean.ogg" 1.0 True "Music", 
        ctPlayCmd #: Stop ] 
    no_shot <- newE hg3d [ 
        ctSoundSource #: Sound "Sounds/inventory_sound_effects/ring_inventory.wav" 1.0 False "Sounds",
        ctPlayCmd #: Stop ]
    shot <- newE hg3d [ 
        ctSoundSource #: Sound "Music/shoot.wav" 1.0 False "Sounds",
        ctPlayCmd #: Stop ]
    invader_killed <- newE hg3d [ 
        ctSoundSource #: Sound "Music/invaderkilled.wav" 1.0 False "Sounds",
        ctPlayCmd #: Stop ]

    actor <- newActor
    runActor actor musicActorF (music, shot, no_shot, invader_killed) ()
    return actor


musicActorF :: Actor -> Message -> ReaderStateIO MaR MaS ()
musicActorF musicA m = do
    (music, shot, no_shot, invader_killed) <- lift ask

    case m of 
        InitActor -> return ()

        PlayShot -> liftIO (setC shot ctPlayCmd Play) >> return ()
        PlayNoShot -> liftIO (setC no_shot ctPlayCmd Play) >> return ()
        PlayExplosion -> liftIO (setC invader_killed ctPlayCmd Play) >> return ()

        StartMusic -> liftIO (setC music ctPlayCmd Play) >> return ()
        StopMusic -> liftIO (setC music ctPlayCmd Stop) >> return ()
        _ -> return ()



