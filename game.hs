{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D
import Actor
import Switch
import Data


-- MAIN PROGRAM
-- ------------

gameLogic :: HG3D -> IO ()
gameLogic hg3d = do


    -- intialize cam
    cam <- initializeCam hg3d

    -- create main switch actor and send message
    switchA <- newSwitchActor hg3d cam
    sendMsg switchA StartProgram
    return ()

main = do
    runGame standardGraphics3DConfig gameLogic (msecT 20)
    return ()

initializeCam hg3d = do

    let (pos', ori') = (Vec3 1 1 (-30.0), unitU)
    let (pos, ori) = cam_pos_1
    -- create minimum elements, like a camera and a light
    eCam <- newE hg3d [
        ctCamera #: FullViewCamera,
        ctPosition #: pos,
        ctOrientation #: ori 
        ]
    l1 <- newE hg3d [
        ctPosition #: (Vec3 (-20.0) 5.0 (-30.0)),
        ctLight #: Light PointLight 1.0 70.0 1.0,
        ctOrientation #: unitU 
        ]

    l2 <- newE hg3d [
        ctLight #: Light (SpotLight (Deg 70) 1.0) 1.0 200.0 1.0, 
        ctPosition #: Vec3 (10) (10) (-10.0)
        ]
    l3 <- newE hg3d [
        ctLight #: Light (SpotLight (Deg 70) 1.0) 1.0 200.0 1.0, 
        ctPosition #: Vec3 (-10) (10) (-10.0)
        ]

    return eCam


