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

type MaR = (Entity, Entity, Entity, Entity, Entity)
type MaS = ()

newMusicActor :: HG3D -> IO Actor
newMusicActor hg3d = do
    music <- newE hg3d [ 
        ctSoundSource #: SoundSource Music "Sounds/RMN-Music-Pack/OGG/CD 3 - Clash of Wills/3-04 Joyful Ocean.ogg" True 1.0 "Music", 
        ctPlayCmd #: Stop ] 
    no_shot <- newE hg3d [ 
        ctSoundSource #: SoundSource Sound "Sounds/inventory_sound_effects/ring_inventory.wav" False 1.0 "Sounds",
        ctPlayCmd #: Stop ]
    shot <- newE hg3d [ 
        ctSoundSource #: SoundSource Sound "Music/shoot.wav" False 1.0 "Sounds",
        ctPlayCmd #: Stop ]
    invader_killed <- newE hg3d [ 
        ctSoundSource #: SoundSource Sound "Music/invaderkilled.wav" False 1.0 "Sounds",
        ctPlayCmd #: Stop ]
    invader_step <- newE hg3d [ 
        ctSoundSource #: SoundSource Sound "Music/fastinvader1.wav" False 1.0 "Sounds",
        ctPlayCmd #: Stop ]

    actor <- newActor
    runActor actor musicActorF (music, shot, no_shot, invader_killed, invader_step) ()
    return actor


musicActorF :: Actor -> Message -> ReaderStateIO MaR MaS ()
musicActorF musicA m = do
    (music, shot, no_shot, invader_killed, invader_step) <- lift ask

    case m of 
        PlayShot -> liftIO (setC shot ctPlayCmd Play) >> return ()
        PlayNoShot -> liftIO (setC no_shot ctPlayCmd Play) >> return ()
        PlayExplosion -> liftIO (setC invader_killed ctPlayCmd Play) >> return ()
        PlayStep -> liftIO (setC invader_step ctPlayCmd Play) >> return ()

        StartMusic -> liftIO (setC music ctPlayCmd Play) >> return ()
        StopMusic -> liftIO (setC music ctPlayCmd Stop) >> return ()
        _ -> return ()



