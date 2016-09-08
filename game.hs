{-# LANGUAGE OverloadedStrings #-}

module Main where

import HGamer3D

import qualified Data.Text as T
import Control.Concurrent
import Control.Monad
import System.Exit

import qualified Data.Map as M
import qualified Data.HMap as HM
import qualified Data.Text as T
import Data.Tree
import Data.Maybe
import qualified Data.Data as D
import qualified Data.Traversable as Tr
import qualified Data.Foldable as Fd
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Debug.Trace

-- ACTORS
-- ------

-- define all messages in this program, high level between actors

data Message = InitActor | StopActor -- intialize and stop actor
             | InitMusic | StartMusic | StopMusic | PlayShot | PlayExplosion -- music actor
             | StartProgram | BuildDone | KeysPressed [T.Text] | SingleKey T.Text -- screen actor
             | InitKeys | PollKeys -- key input actor
             | BuildLevel [BuildElement] | MoveLeft | MoveRight | Shoot | MovementCycle -- movement actor
             | RollRight | RollLeft | PitchUp | PitchDown -- flying control
             | YawLeft | YawRight 
             | MoreSpeed | LessSpeed | ZeroSpeed 
             | ResetCamPosition | RestoreCamPosition | SaveCamPosition
             | DisplayStatus | HideStatus | SetName T.Text | SetCount Int | SetMode T.Text -- status bar actor
             deriving (Eq, Ord, Show)

-- we are going for an actor model with forkIO starting an actor and MVar being 
-- the message passing channel, actors are bigger pieces of logic 

newtype Actor = Actor (MVar Message)

newActor :: IO Actor
newActor = do
    mv <- newEmptyMVar
    return (Actor mv)

type ReaderStateIO r s a = StateT s (ReaderT r IO) a

runActor :: Actor -> (Message -> ReaderStateIO r s () ) -> r -> s -> IO ()
runActor a@(Actor mv) f ri si = do
    let loop mv s = do
            msg <- takeMVar mv
            (_, s') <- runReaderT (runStateT (f msg) s) ri
            loop mv s'
    forkIO $ loop mv si
    sendMsg a InitActor

stopActor a = sendMsg a StopActor

sendMsg :: Actor -> Message -> IO ()
sendMsg (Actor mv) m = putMVar mv m


-- MAIN PROGRAM
-- ------------

gameLogic :: HG3D -> IO ()
gameLogic hg3d = do

    -- create basic keys
    kent <- HM.createKey
    kdim <- HM.createKey
    kpos <- HM.createKey
    khits <- HM.createKey
    ksize <- HM.createKey

    -- intialize cam
    cam <- initializeCam hg3d

    -- create actors
    [moveA, flyingA, musicA, screenA, keyA, statusBarA] <- mapM (const newActor) [1..6]

    -- interconnect and run them
    runActor statusBarA statusBarActorF hg3d (undefined, undefined, undefined)
    runActor flyingA flyingActorF (hg3d, cam) (undefined, undefined)
    runActor musicA musicActorF hg3d (undefined, undefined, undefined)
    runActor moveA movementActorF (hg3d, screenA, kent, kdim, kpos, khits) (0, undefined, undefined, undefined)
    runActor screenA gameScreenActorF (hg3d, moveA, musicA, flyingA, statusBarA) (undefined, undefined, ProgramInitializing)
    runActor keyA keyInputActorF (hg3d, screenA) (undefined, [])

    -- generating messages to keep game rolling
    forkIO $ forever $ do
        sendMsg keyA PollKeys
        sendMsg screenA MovementCycle
        sleepFor (msecT 15)
        return ()

    -- start with game logic by starting first screen
    sendMsg screenA StartProgram

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


-- MUSIC ACTOR
-- -----------

type MaR = HG3D
type MaS = (Entity, Entity, Entity)

musicActorF :: Message -> ReaderStateIO MaR MaS ()
musicActorF m = do
    hg3d <- lift ask
    (music, shot, collision) <- get

    case m of 
        InitActor -> do
            music <- liftIO $ newE hg3d [ 
                ctSoundSource #: Music "Sounds/RMN-Music-Pack/OGG/CD 3 - Clash of Wills/3-04 Joyful Ocean.ogg" 1.0 True "Music", 
                ctPlayCmd #: Stop ] 
            shot <- liftIO $ newE hg3d [ 
                ctSoundSource #: Sound "Sounds/inventory_sound_effects/ring_inventory.wav" 1.0 False "Sounds",
                ctPlayCmd #: Stop ]
            collision <- liftIO $ newE hg3d [ 
                ctSoundSource #: Sound "Sounds/inventory_sound_effects/metal-clash.wav" 1.0 False "Sounds",
                ctPlayCmd #: Stop ]
            put (music, shot, collision)
            return ()

        PlayShot -> liftIO (setC shot ctPlayCmd Play) >> return ()
        PlayExplosion -> liftIO (setC collision ctPlayCmd Play) >> return ()

        StartMusic -> liftIO (setC music ctPlayCmd Play) >> return ()
        StopMusic -> liftIO (setC music ctPlayCmd Stop) >> return ()



-- STATUS BAR ACTOR
-- ----------------

type SbaR = HG3D
type SbaS = (Entity, Entity, Entity)

statusBarActorF :: Message -> ReaderStateIO SbaR SbaS ()
statusBarActorF msg = do

    hg3d <- lift ask
    (textLeft, textMiddle, textRight) <- get

    let setY y' e = do
            r@(Rectangle x y w h) <- readC e ctScreenRect
            setC e ctScreenRect (Rectangle x y' w h)
            return ()

    case msg of

        InitActor -> do
            eL <- liftIO $ newE hg3d [
                ctText #: "hero: ",
                ctScreenRect #: Rectangle 10 (-1000) 100 25
                ]

            eM <- liftIO $ newE hg3d [
                ctText #: "count: ",
                ctScreenRect #: Rectangle 350 (-1000) 100 25
                ]

            eR <- liftIO $ newE hg3d [
                ctText #: "playing",
                ctScreenRect #: Rectangle 590 (-1000) 100 25
                ]

            put (eL, eM, eR)
            return ()

        DisplayStatus -> liftIO (mapM (\e -> setY 10 e) [textLeft, textMiddle, textRight]) >> return ()
        HideStatus -> liftIO (mapM (\e -> setY (-1000) e) [textLeft, textMiddle, textRight]) >> return ()
        SetName name -> liftIO (setC textLeft ctText ("hero: " `T.append` name)) >> return ()
        SetCount count -> liftIO (setC textMiddle ctText (T.pack ("count: " ++ (show count)))) >> return ()
        SetMode mode -> liftIO (setC textRight ctText mode)  >> return ()

        _ -> return ()


-- SCREEN ACTOR
-- ------------

data GameState = ProgramInitializing | InitScreen | BuildField | PlayGame | Flying | FinalScore deriving (Eq, Ord, Show)

type GsaR = (HG3D, Actor, Actor, Actor, Actor)
type GsaS = ((Entity, Entity, Entity, Entity, Entity), T.Text, GameState)

gameScreenActorF :: Message -> ReaderStateIO GsaR GsaS ()
gameScreenActorF msg = do

    (hg3d, moveA, musicA, flyingA, statusBarA) <- lift ask
    (screenText, name, gameState) <- get

    let returnStay = return () 
    let returnMoveTo state = put (screenText, name, state) >> return ()

    case gameState of

        ProgramInitializing -> 
            case msg of
                StartProgram -> do
                    (eT1, eT2, eT3, eT4, eName) <- liftIO $ showInitScreen hg3d
                    liftIO $ sendMsg musicA StartMusic
                    put ((eT1, eT2, eT3, eT4, eName), name, InitScreen) >> return ()
                _ -> returnStay

        InitScreen -> 

            case msg of

                SingleKey k -> do
                    if k == "Return" 
                        then do
                            let (eT1, eT2, eT3, eT4, eName) = screenText
                            name' <- liftIO $ readC eName ctEditText
                            liftIO $ sendMsg moveA (BuildLevel buildData1)
                            liftIO $ sendMsg musicA StopMusic
                            liftIO $ mapM (\e -> setC e ctScreenRect (Rectangle (-1000) (-1000) 0 0)) [eT1, eT2, eT3, eT4, eName]
                            put (undefined, name', BuildField) >> return ()
                        else returnStay

                _ -> returnStay

        BuildField -> 
            case msg of
                BuildDone -> do
                    liftIO $ sendMsg statusBarA (SetName name)
                    liftIO $ sendMsg statusBarA (SetCount 0)
                    liftIO $ sendMsg statusBarA (SetMode "playing")
                    liftIO $ sendMsg statusBarA (DisplayStatus)
                    returnMoveTo PlayGame
                _ -> returnStay

        PlayGame -> 

            case msg of

                SingleKey k -> do
                    case k of
                        "Space" -> do
                            liftIO $ sendMsg moveA Shoot
                            returnStay
                        "F1" -> liftIO (sendMsg statusBarA (SetMode "paused ...")) >> returnMoveTo Flying
                        "F2" -> liftIO (sendMsg flyingA ResetCamPosition) >> returnStay
                        _ -> returnStay

                MovementCycle -> liftIO (sendMsg moveA MovementCycle) >> returnStay

                KeysPressed keys -> do
                    if ("Left" `elem` keys) && (not ("Right" `elem` keys))
                        then do
                            liftIO $ sendMsg moveA MoveLeft
                            returnStay
                        else returnStay

                    if ("Right" `elem` keys) && (not ("Left" `elem` keys)) 
                        then do
                            liftIO $ sendMsg moveA MoveRight
                            returnStay
                        else returnStay

                _ -> returnStay


        Flying -> 

            case msg of

                SingleKey k -> do
                    case k of
                        "F1" -> liftIO (sendMsg statusBarA (SetMode "playing")) >> returnMoveTo PlayGame
                        "W" -> liftIO (sendMsg flyingA MoreSpeed) >> returnStay
                        "S" -> liftIO (sendMsg flyingA LessSpeed) >> returnStay
                        "Q" -> liftIO (sendMsg flyingA ZeroSpeed) >> returnStay
                        "F2" -> liftIO (sendMsg flyingA ResetCamPosition) >> returnStay
                        "F3" -> liftIO (sendMsg flyingA SaveCamPosition) >> returnStay
                        "F4" -> liftIO (sendMsg flyingA RestoreCamPosition) >> returnStay
                        _ -> returnStay

                KeysPressed keys -> do
                    mapM (\k -> do
                        case k of
                            "A" -> liftIO $ sendMsg flyingA YawLeft
                            "D" -> liftIO $ sendMsg flyingA YawRight
                            "Up" -> liftIO $ sendMsg flyingA PitchUp 
                            "Down" -> liftIO $ sendMsg flyingA PitchDown
                            "Left" -> liftIO $ sendMsg flyingA RollLeft 
                            "Right" -> liftIO $ sendMsg flyingA RollRight
                            _ -> return ()
                        ) keys
                    returnStay

                _ -> returnStay



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

    return (eT1, eT2, eT3, eT4, eName)


-- KEY INPUT ACTOR
-- ---------------

type KiaR = (HG3D, Actor)
type KiaS = (Var [KeyEvent], [T.Text])

keyInputActorF :: Message -> ReaderStateIO KiaR KiaS ()
keyInputActorF msg = do

    (hg3d, screenA) <- lift ask
    (keyevts, keysdown) <- get

    case msg of 

        InitActor -> do
            keyevts' <- liftIO $ makeVar []
            let handleKey k = updateVar keyevts' (\l -> (k : l, ()))
            ieh <- liftIO $ newE hg3d [ctInputEventHandler #: DefaultEventHandler, ctKeyEvent #: NoKeyEvent]
            liftIO $ registerCallback hg3d ieh ctKeyEvent handleKey
            put (keyevts', keysdown) >> return ()

        PollKeys -> do
            keys <- liftIO $ updateVar keyevts (\l -> ([], l))
            keysdown' <- foldM (\kd k -> do
                    case k of
                        KeyDown _ _ k -> do
                            let kd' = if not (k `elem` kd) then (k : kd) else kd
                            return kd'

                        KeyUp _ _ k -> do
                            let kd' = filter (\k' -> k' /= k) kd
                            liftIO $ sendMsg screenA (SingleKey k)
                            return kd'
                ) keysdown (reverse keys)

            if length keysdown' > 0 
                then liftIO $ sendMsg screenA (KeysPressed keysdown')
                else return ()

            put (keyevts, keysdown') >> return ()


-- MOVEMENT ACTOR
-- --------------

type MoaR = (HG3D, Actor, KEnt, KDim, KPos, KHits)
type MoaS = (Int, PixelPos, PixelPos, GameData)

movementActorF :: Message -> ReaderStateIO MoaR MoaS ()
movementActorF m = do

    (hg3d, screenA, kent, kdim, kpos, khits) <- lift ask
    (c, canonPos, canonMove, gameData) <- get

    case m of

        BuildLevel buildData1 -> do
            createLevel buildData1
            liftIO  $ sendMsg screenA BuildDone
            return ()

        MoveLeft -> do
            let (x, y) = canonMove
            let (px, py) = canonPos
            if (px > -65)
                then put (c, (px - 1, py), (x - 1, y), gameData)
                else return ()

        MoveRight ->  do
            let (x, y) = canonMove
            let (px, py) = canonPos
            if (px < 100)
                then put (c, (px + 1, py), (x + 1, y), gameData)
                else return ()

        Shoot -> do
            bulletAvailable <- shoot
            return ()

        MovementCycle -> do

            let moves = c `div` nWait + (movesRightLeft `div` 2) -- start in the middle 
            let bMove = (c `mod` nWait == 0) && ((c `div` nWait) < (( 2 * movesRightLeft + 2 ) * movesDown) ) 
            let move = moves `mod` (2 * movesRightLeft + 2)

            gameData' <- mapM (\(nodeType, nodeData) -> do

                nodeData' <- case nodeType of
                    -- invaders stepping
                    (Invader _) -> do invaderMove nodeData bMove move

                    -- shots moving
                    Shot -> do
                        nd <- moveNode nodeData (0, 2)
                        let (x, y) = nd ! kpos
                        if y > 100 
                            then return (deactivateShot nd kpos)
                            else return nd

                    -- canon move
                    Canon -> moveNode nodeData canonMove

                    -- no move
                    _ -> return nodeData

                return (nodeType, nodeData')
                ) gameData

            put (c + 1, canonPos, (0, 0), gameData') 

        _ -> return ()

-- data for all moving parts are kept in a flexible tree data structure
-- each node of this tree has a node type and node data

data NodeType = Canon | ReserveCanon | Boulder | Invader Int | Ship | Shot 
              | InvaderRow | CanonRow | BoulderRow | ShotRow
              | Pixel | Empty
              deriving (Eq, Show, Ord)

type NodeData = HM.HMap

initData :: HM.HKey t a -> a -> NodeData
initData k v = HM.singleton k v

setData :: HM.HKey t a -> a -> NodeData -> NodeData
setData k v nd = HM.insert k v nd

(!) :: NodeData -> HM.HKey t a -> a
(!) nd k = case HM.lookup k nd of
    Just v -> v
    Nothing -> trace ("NodeData ! not there") undefined

type GameData = Tree (NodeType, NodeData)

isNodeType :: GameData -> NodeType -> Bool
isNodeType (Node (nt, _) _) nt' = nt == nt'

-- keys for node data

type KPos = HM.HKey HM.T PixelPos
type KHits = HM.HKey HM.T HitInfo
type KDim = HM.HKey HM.T DimInfo
type KEnt = HM.HKey HM.T Entity

getChildren :: GameData -> [GameData]
getChildren (Node _ c) = c

addChildren :: GameData -> [GameData] -> GameData
addChildren (Node v c) c' = Node v (c ++ c')

removeChildren :: GameData -> GameData
removeChildren (Node v c) = Node v []

-- position, size data, all positions are given in pixel pos and converted to Vec3

type PixelPos = (Int, Int)   

pixelWidth :: Float
pixelWidth = 0.1

lineWidth :: Float
lineWidth = 0.02

data DimInfo = DimInfo {
    diWidth :: Int,
    diHeight :: Int,
    diCenterX :: Float,
    diCenterY :: Float,
    diOffX :: Float,
    diOffY :: Float }
    deriving (Eq, Show, Ord)

diFromSize :: Int -> Int -> DimInfo
diFromSize width height = let
    cx = ((fromIntegral width) * pixelWidth + (fromIntegral (width - 1)) * lineWidth) / 2.0 
    cy = ((fromIntegral height) * pixelWidth + (fromIntegral (height - 1)) * lineWidth) / 2.0 
    ox = if (width `mod` 2) > 0 then (pixelWidth + lineWidth) * (-0.5) else 0.0     
    oy = if (height `mod` 2) > 0 then (pixelWidth + lineWidth) * (-0.5) else 0.0 
    in (DimInfo width height cx cy ox oy)

posFromPixelPos :: DimInfo -> PixelPos -> Vec3
posFromPixelPos di (x, y) = let
    delta = pixelWidth + lineWidth
    in Vec3 ((fromIntegral x) * delta + (diOffX di)) ((fromIntegral y) * delta + (diOffY di)) 0.0

-- damage, hits

data HitInfo = HitInfo {
    hiHits :: Int,
    hiMaxHits :: Int
    }
    deriving (Eq, Show, Ord)

hiNew :: Int -> HitInfo
hiNew maxHits = HitInfo 0 maxHits

takeHits :: HitInfo -> Int -> HitInfo
takeHits (HitInfo h mh) hits = (HitInfo (h + hits) mh)

damage :: HitInfo -> Float
damage (HitInfo h m) = if h >= m then 1.0 else (fromIntegral h) / (fromIntegral m)

-- create a node with subnodes for moveable item and cubes below

createMoveNode :: NodeType -> PixelPos -> ReaderStateIO MoaR MoaS GameData
createMoveNode nodeType pos = do

    (hg3d, screenA, kent, kdim, kpos, khits) <- lift ask

    let arts = artwork M.! nodeType
    let pp = pixelPairs arts
    let (w, h) = dimArt arts
    let dim = diFromSize w h
    let hits = hiNew (hitData M.! nodeType)

    let delta = pixelWidth + lineWidth
    let mat = matArt arts

    eGeo <- liftIO $ newE hg3d [
        ctGraphicsElement #: (),
        ctScale #: Vec3 1.0 1.0 1.0,
        ctPosition #: posFromPixelPos dim pos,
        ctOrientation #: unitU
        ]

    let nd = setData kdim dim $ setData kent eGeo $ setData kpos pos $ initData khits hits

    eGeoId <- liftIO $ idE eGeo

    pes <- liftIO $ mapM (\(x, y) -> do
        e <- newE hg3d [
            ctGeometry #: ShapeGeometry Cube,
            ctMaterial #: mat,
            ctScale #: Vec3 pixelWidth pixelWidth (2 * pixelWidth),
            ctPosition #: ((Vec3 ((fromIntegral x) * delta) ((fromIntegral y) * delta) 0.0) &- (Vec3 (diCenterX dim) (diCenterY dim) 0.0) ), 
            ctOrientation #: unitU,
            ctParent #: eGeoId
            ]
        return (e, (x - ((diWidth dim) `div` 2), y - ((diHeight dim) `div` 2)))
        ) pp

    return $ Node (nodeType, nd) (map (\(e, p) -> Node (Pixel, (setData kent e (initData kpos p))) []) pes)


createLevel :: [BuildElement] -> ReaderStateIO MoaR MoaS ()
createLevel bd = do

    (hg3d, screenA, kent, kdim, kpos, khits) <- lift ask
    (cycles, _, _, _) <- get

    nodes <- mapM (\be -> case be of
                            BEOne nt pix -> createMoveNode nt pix
                            BERow nt pix@(x, y) space count -> do
                                let (nt', nt'') = case nt of
                                        (Invader n) -> (InvaderRow, Invader n)
                                        Boulder -> (BoulderRow, Boulder)
                                        Canon -> (CanonRow, ReserveCanon)
                                        Shot -> (ShotRow, Shot)
                                subnodes <- mapM (\pix' -> createMoveNode nt'' pix') [(x' + x, y) | x' <- [0, space .. ((count-1)*space)]] 
                                return (Node (nt', (HM.singleton kpos pix)) subnodes) 
                    ) bd 

    let tree = Node (Empty, HM.empty) nodes
    put (cycles, (0, -65), (0, 0), tree)
    return ()

-- moving

moveNode :: NodeData -> PixelPos -> ReaderStateIO MoaR MoaS NodeData
moveNode nodeData (x', y') = do

    (hg3d, screenA, kent, kdim, kpos, khits) <- lift ask

    let (x, y) = nodeData ! kpos
    let nodeData' = setData kpos (x + x', y + y') nodeData
    let e = nodeData ! kent
    let di = nodeData ! kdim
    liftIO $ setC e ctPosition $ posFromPixelPos di (x + x', y + y')
    return nodeData'

nWait = 20 :: Int
movesRightLeft = 10 :: Int
movesDown = 15 :: Int

invaderMove :: NodeData -> Bool -> Int -> ReaderStateIO MoaR MoaS NodeData
invaderMove nd bMove move = do

    if bMove 
        then if move < movesRightLeft 
                    then moveNode nd (2,0) 
                    else if move == movesRightLeft
                            then moveNode nd (0, -5) 
                            else if move <= 2 * movesRightLeft
                                        then moveNode nd (-2, 0) 
                                        else moveNode nd (0, -5)
        else return nd


-- shooting

isActiveShot :: NodeData -> KPos -> Bool
isActiveShot nd k = let
    (x, y) = nd ! k
    in x > (-500)

deactivateShot :: NodeData -> KPos -> NodeData
deactivateShot nd kpos = setData kpos (-1000, 0) nd

shoot :: ReaderStateIO MoaR MoaS Bool
shoot = do
    (hg3d, screenA, kent, kdim, kpos, khits) <- lift ask
    (cycles, canonPos, canonMove, gameData) <- get
    let (x, y) = canonPos
    let shootPos = (x, y + 5)
    let ((found', nodeData'), gameData') = Tr.mapAccumL (\(found, nd) (nodeType, nodeData) -> if (not found && nodeType == Shot)
                        then
                            if not (isActiveShot nodeData kpos)
                                then ((True, nodeData), (Shot, setData kpos shootPos nodeData))
                                else ((False, undefined), (nodeType, nodeData))
                        else ((found, nd), (nodeType, nodeData))
                  ) (False, undefined) gameData

    if found'
        then do
            liftIO $ setC (nodeData' ! kent) ctPosition $ posFromPixelPos (nodeData' ! kdim) shootPos
            put (cycles, canonPos, canonMove, gameData')
        else return ()

    return found'


-- FLYING CONTROL ACTOR
-- --------------------

data Speed = Speed Int

type FaR = (HG3D, Entity)
type FaS = (Var Speed, Var (Position, Orientation))

flyingActorF :: Message -> ReaderStateIO FaR FaS ()
flyingActorF msg = do

    let f = 0.01
    (hg3d, cam) <- lift ask
    (speed, campos) <- get

    case msg of

        InitActor -> do
            speed' <- liftIO $ makeVar (Speed 0)
            ori <- liftIO $ readC cam ctOrientation
            pos <- liftIO $ readC cam ctPosition
            campos' <- liftIO $ makeVar (pos, ori)

            let loop = do
                    (Speed s) <- readVar speed'
                    if s /= 0 then forward cam ((fromIntegral s)/30.0) else return ()
                    sleepFor (msecT 30)
                    loop

            liftIO $ forkIO $ forever $ loop
            put (speed', campos')
            return ()

        YawRight -> liftIO  (Main.yaw cam (f)) >> return ()
        YawLeft -> liftIO  (Main.yaw cam (-f)) >> return ()
        RollRight -> liftIO  (Main.roll cam (-f)) >> return ()
        RollLeft -> liftIO  (Main.roll cam f) >> return ()
        PitchUp -> liftIO  (Main.pitch cam f) >> return ()
        PitchDown -> liftIO  (Main.pitch cam (-f)) >> return ()

        MoreSpeed -> liftIO  (updateVar speed (\(Speed i) -> (Speed (i + 1), ()))) >> return ()
        LessSpeed -> liftIO  (updateVar speed (\(Speed i) -> (Speed (i - 1), ()))) >> return ()
        ZeroSpeed -> liftIO  (writeVar speed (Speed 0)) >> return ()

        SaveCamPosition -> do
            ori <- liftIO $ readC cam ctOrientation
            pos <- liftIO $ readC cam ctPosition
            liftIO $ writeVar campos (pos, ori)
            return ()

        RestoreCamPosition -> do
            (pos, ori) <- liftIO $ readVar campos
            liftIO $ setC cam ctOrientation ori
            liftIO $ setC cam ctPosition pos
            return ()

        ResetCamPosition -> do
            liftIO $ setC cam ctOrientation unitU
            liftIO $ setC cam ctPosition (Vec3 1 1 (-30.0))
            return ()


-- flying control, rolling, moving
rotRelativeToObjectAxis :: Entity -> Vec3 -> Float -> IO ()
rotRelativeToObjectAxis e axis val = do
    qob <- readC e ctOrientation
    let odir = actU qob axis
    let qrot = rotU odir val
    let nrot = qrot .*. qob
    setC e ctOrientation nrot
    return ()
yaw e val = rotRelativeToObjectAxis e vec3Y val
roll e val = rotRelativeToObjectAxis e vec3Z val
pitch e val = rotRelativeToObjectAxis e vec3X val

-- function, to move into direction of flight
forward :: Entity -> Float -> IO ()
forward e val = do
    qob <- readC e ctOrientation
    -- this points towards nose
    let vdir = actU qob vec3Z
    updateC e ctPosition (\pos -> pos &+ (val *& vdir))
    return ()


-- Data for Game Initialization
-------------------------------

-- first game level

data BuildElement = BEOne NodeType PixelPos
                    | BERow NodeType PixelPos Int Int -- space, count
                      deriving (Eq, Ord, Show)

buildData1 :: [BuildElement]
buildData1 = [
        BEOne Ship (0, 100),
        BERow (Invader 3) (-60, 85) 15 11,
        BERow (Invader 2) (-60, 70) 15 11,
        BERow (Invader 2) (-60, 55) 15 11,
        BERow (Invader 1) (-60, 40) 15 11,
        BERow (Invader 1) (-60, 25) 15 11,
        BERow Boulder (-45, -50) 40 4,
        BEOne Canon (0, -65),
        BERow Canon (-60, -80) 15 2,
        BERow Shot (-1000, 0) 5 5
    ]

-- hitcounts for different game players

hitData :: M.Map NodeType Int
hitData = M.fromList [ (Shot, 1), (Canon, 0), (ReserveCanon, 0), (Boulder, 0), (Ship, 3), (Invader 1, 1), (Invader 2, 1), (Invader 3, 2) ]


-- graphics for space invader players

type Artwork = ([T.Text], [T.Text], Material)

-- get a list of (x, y) from Artwork
pixelPairs :: Artwork -> [(Int, Int)]
pixelPairs (lineArray, _, _) = let
    lineIdx line = [ i | (i, c) <- zip [0..] (T.unpack line), c == 'x']
    in [ (i, j) | (j, l) <- zip [0..] (reverse lineArray), i <- lineIdx l]

-- get width, height from Artwork
dimArt :: Artwork -> (Int, Int)
dimArt (lineArray, _, _) = let
    width = maximum (map (length . T.unpack) lineArray)
    height = length lineArray
    in (width, height)

matArt :: Artwork -> Material
matArt (_, _, mat) = mat

artwork :: M.Map NodeType Artwork
artwork = M.fromList [
  (Shot,
  (
    [
     "x",
     "x"
     ],
    [
     "x",
     "x"
     ],
     matYellow
  )),
  
  (Canon,
  (
    [
     "     x     ",
     "    xxx    ",
     "    xxx    ",
     " xxxxxxxxx ",
     "xxxxxxxxxxx",
     "xxxxxxxxxxx"
     ],
    [
     "     x     ",
     "    xxx    ",
     "    xxx    ",
     " xxxxxxxxx ",
     "xxxxxxxxxxx",
     "xxxxxxxxxxx"
     ],
     matLime
  )),
  
  (ReserveCanon,
  (
    [
     "     x     ",
     "    xxx    ",
     "    xxx    ",
     " xxxxxxxxx ",
     "xxxxxxxxxxx",
     "xxxxxxxxxxx"
     ],
    [
     "     x     ",
     "    xxx    ",
     "    xxx    ",
     " xxxxxxxxx ",
     "xxxxxxxxxxx",
     "xxxxxxxxxxx"
     ],
     matLime
  )),
  
  (Boulder,
  (
    [
     "    xxxxxxxxxxxx    ",
     "   xxxxxxxxxxxxxx   ",
     "  xxxxxxxxxxxxxxxx  ",
     " xxxxxxxxxxxxxxxxxx ",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxx        xxxxxx",
     "xxxxx          xxxxx",
     "xxxx            xxxx",
     "xxxx            xxxx"
     ],
    [
     "    xxxxxxxxxxxx    ",
     "   xxxxxxxxxxxxxx   ",
     "  xxxxxxxxxxxxxxxx  ",
     " xxxxxxxxxxxxxxxxxx ",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxxxxxxxxxxxxxxxx",
     "xxxxxx        xxxxxx",
     "xxxxx          xxxxx",
     "xxxx            xxxx",
     "xxxx            xxxx"
     ],
     matLime
  )),
  
  (Invader 1,
  (
    ["    xxxx    ",
     " xxxxxxxxxx ",
     "xxxxxxxxxxxx",
     "xxx  xx  xxx",
     "xxxxxxxxxxxx",
     "  xxx  xxx  ",
     " xx  xx  xx ",
     "  xx    xx  "],
    ["    xxxx    ",
     " xxxxxxxxxx ",
     "xxxxxxxxxxxx",
     "xxx  xx  xxx",
     "xxxxxxxxxxxx",
     "   xx  xx   ",
     "  xx xx xx  ",
     "xx        xx"],
     matGreen
  )),
  
  (Invader 2,
  (
    ["  x     x  ",
     "   x   x   ",
     "  xxxxxxx  ",
     " xx xxx xx ",
     "xxxxxxxxxxx",
     "x xxxxxxx x",
     "x x     x x",
     "   xx xx   "],
    ["  x     x  ",
     "x  x   x  x",
     "x xxxxxxx x",
     "xxx xxx xxx",
     "xxxxxxxxxxx",
     " xxxxxxxxx ",
     "  x     x  ",
     " x       x "],
     matRed
  )),
  
  (Invader 3,
  (
    ["   xx   ",
     "  xxxx  ",
     " xxxxxx ",
     "xx xx xx",
     "xxxxxxxx",
     " x xx x ",
     "x      x",
     " x    x "],
    ["   xx   ",
     "  xxxx  ",
     " xxxxxx ",
     "xx xx xx",
     "xxxxxxxx",
     "  x  x  ",
     " x xx x ",
     "x x  x x"],
     matMaroon
  )),
  
  (Ship,
  (
    ["     xxxxxx     ",
     "   xxxxxxxxxx   ",
     "  xxxxxxxxxxxx  ",
     " xx xx xx xx xx ",
     "xxxxxxxxxxxxxxxx",
     "  xxx  xx  xxx  ",
     "   x        x   "],
    ["     xxxxxx     ",
     "   xxxxxxxxxx   ",
     "  xxxxxxxxxxxx  ",
     " xx xx xx xx xx ",
     "xxxxxxxxxxxxxxxx",
     "  xxx  xx  xxx  ",
     "   x        x   "],
     matBlue
  ))
  
  
  ]

