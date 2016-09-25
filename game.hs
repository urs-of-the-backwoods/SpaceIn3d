{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D
import Actor
import Switch


-- MAIN PROGRAM
-- ------------

gameLogic :: HG3D -> IO ()
gameLogic hg3d = do


    -- intialize cam
    cam <- initializeCam hg3d

    -- create main switch actor and send message
    switchA <- newSwitchActor hg3d
    sendMsg switchA StartProgram
    return ()

main = do
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()

initializeCam hg3d = do
    -- create minimum elements, like a camera and a light
    eCam <- newE hg3d [
        ctCamera #: FullViewCamera,
        ctPosition #: Vec3 1 1 (-30.0),
        ctLight #: Light PointLight 1.0 1000.0 1.0,
        ctOrientation #: unitU 
        ]
    return eCam


