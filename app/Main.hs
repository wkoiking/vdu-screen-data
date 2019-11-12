{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- sdl
import qualified SDL
import qualified SDL.Image
-- base
import Data.Word (Word8, Word16, Word32)
import Foreign.Ptr ( castPtr )
import Data.Int (Int32)
import Control.Monad (replicateM, liftM, liftM2)
import Data.Maybe (catMaybes)
import Data.List (find)
-- vdu-screen-data
import TCMS
-- pretty-show
import Text.Show.Pretty (ppShow)
-- safe-exception
import Control.Exception.Safe
-- cereal
import Data.Serialize (encode, runPut, runGet, PutM, Putter, Serialize, Get, putWord8, getWord8, putWord16be, getWord16be, putWord32be, skip)
-- conduit-combinators
import Conduit hiding (connect, throwM)
import Data.Conduit.Network (sinkSocket, sourceSocket)
-- network
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, sendTo, recv, recvFrom)
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB (toStrict)
-- linear
import Linear.Affine (unP)
-- time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock (diffUTCTime, getCurrentTime, UTCTime(..), NominalDiffTime)
import Data.Time.Calendar (Day, addGregorianMonthsClip, addDays)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (utcToLocalTime, localDay)

newtype ScreenID = ScreenID Word16 deriving (Eq, Show)
newtype KeyID = KeyID Word8 deriving (Eq, Show)

data ResponseID
    = NormalResponse
    | TransitionUnavailableByNoCorrespondingScreenIdAndKeyId 
    | TransitionUnavailableByInoperableKey
    | TransitionUnavailableByOperatedByTrainCrew
    | TransitionUnavailableOthersReason
    | ResponseUnavailableByAccessedByOtherServer
    | ResponseUnavailableByATCNoInUTO
    | ResponseUnavailableByOthersReason
        deriving (Show)

myServerIP :: String
myServerIP = "172.24.101.1"

rakeID :: RakeID
rakeID = RakeID 1
isDM1 :: Bool
isDM1 = True 

screenWidth :: Num a => a
screenWidth = 640
screenHeight :: Num a => a
screenHeight = 480

portNumberVDUScreenDataWord16 :: Word16
portNumberVDUScreenDataWord16 = 0

portNumberVduScreenData :: String
portNumberVduScreenData = ""

addCheckSumWord32 :: ByteString -> ByteString
addCheckSumWord32 bStr = B.append bStr (runPut $ putWord32be checkSum)
 where checkSum :: Word32
       checkSum = sum $ map fromIntegral $ B.unpack bStr

main :: IO ()
main = do
    SDL.initialize [ SDL.InitVideo ]
    SDL.Image.initialize []
    window <- SDL.createWindow
        "VDU Sceen Data Example"
        SDL.defaultWindow {SDL.windowInitialSize = SDL.V2 screenWidth screenHeight}
    SDL.showWindow window

    screen <- SDL.getWindowSurface window
    pixelFormat <- SDL.surfaceFormat screen

    -- •`‰æŠÖ”
    let renderPng :: ByteString -> IO ()
        renderPng png = do
--             image <- SDL.Image.load "C:/Users/wanag/MyCode/vdu-screen-data/loaded.png"
            image <- SDL.Image.decode png
            surface <- SDL.convertSurface image pixelFormat
            SDL.surfaceBlitScaled surface Nothing screen Nothing
            SDL.updateWindowSurface window
            return ()

    let loop :: (ScreenID, [(KeyID, (Word16, Word16, Word16, Word16))]) -> IO ()
        loop screenInfo@(screen, buttons) = do
            event <- SDL.waitEvent
            case SDL.eventPayload event of
                SDL.MouseButtonEvent SDL.MouseButtonEventData{..} -> do
                    case mouseButtonEventMotion of
                        SDL.Pressed -> do
                            let pos = toWord16Point mouseButtonEventPos
                            putStrLn $ show pos
                            let mKey = getKeyIDs pos buttons
                            case mKey of
                                Just key -> do
                                    ((respoinceID, mScreenInfo'), png) <- requestScreen screen key
                                    putStrLn $ show respoinceID
                                    putStrLn $ ppShow mScreenInfo'
                                    renderPng png
                                    case mScreenInfo' of
                                        Just screenInfo' -> loop screenInfo'
                                        Nothing -> loop screenInfo
                                Nothing -> loop screenInfo
                        _           -> loop screenInfo
                SDL.QuitEvent       -> return ()
                _                   -> loop screenInfo


    loop (ScreenID 0, [(KeyID 0, (0, screenWidth, 0, screenHeight))])
    SDL.Image.quit
    SDL.quit
    putStrLn "Exitting"

withSocket
    :: HostName -- ^ IP Address of the target server to communicate
    -> ServiceName -- ^ Port number of the target server to communicate
    -> (Socket -> SockAddr -> IO a)
    -> IO a
withSocket hostname port io = bracket initializer (close . fst) (uncurry io)
 where initializer = do
           addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
           let serveraddr = head addrinfos
           sock <- socket (addrFamily serveraddr) Stream defaultProtocol
           return (sock, addrAddress serveraddr)

requestScreen
--     :: ServerID
--     -> RakeID
--     -> Bool
    :: ScreenID
    -> KeyID
    -> IO ((ResponseID, Maybe (ScreenID, [(KeyID, (Word16, Word16, Word16, Word16))])), ByteString)
requestScreen screenID keyID = do -- return ((NormalResponse, Just (ScreenID 0, [(KeyID 0, (0, screenWidth, 0, screenHeight))])), mempty)
    let host = (if isDM1 then fst else snd) $ makeIPAddrsTCMS rakeID
        dstIpAddr = read host
        srcIPAddr = read myServerIP
    time <- getCurrentTime
    withSocket host portNumberVduScreenData $ \ sock addr -> do
        connect sock addr
        send sock $ addCheckSumWord32 $ encode $ runPut $ do
            putTCMSHeader $ TCMSHeader
                { tcmsCommunicationNumber = 1
                , tcmsRetryCounter = 0
                , tcmsRakeID = rakeID
                , tcmsSrcIPAddr = srcIPAddr
                , tcmsDstIPAddr = dstIpAddr
                , tcmsDstPortNumber = portNumberVDUScreenDataWord16
                , tcmsTransmissionTime = time
                }
            putVduScreenDataRequest screenID keyID
        bstr <- liftM LB.toStrict $ runConduitRes $ sourceSocket sock .| sinkLazy
        let payloadByteString = B.drop 64 $ dropEnd 4 bstr
        let (headerByteString, pngByteString) = B.splitAt (3152 - 64) payloadByteString
        case runGet getScreenHeader headerByteString of
            Right a -> return (a, pngByteString)
            Left err -> throwString err

dropEnd :: Int -> ByteString -> ByteString
dropEnd n bstr = B.take (B.length bstr - n) bstr

putVduScreenDataRequest :: ScreenID -> KeyID -> PutM ()
putVduScreenDataRequest (ScreenID screen) (KeyID key) = do
    putZeroBytes 4
    putWord16be screen
    putWord8 key
    putZeroBytes 9

getScreenHeader :: Get (ResponseID, Maybe (ScreenID, [(KeyID, (Word16, Word16, Word16, Word16))]))
getScreenHeader = do
    responceID <- getResponseID
    isScreen <- getWord8
    skip 1 -- mode <- getCurrentMode
    skip 1
    screenID <- getScreenID
    skip 2
    skip 4 -- size <- getScreenSize
    skip 1 -- n <- numOfKeys
    skip 3
    buttons <- decodeButtons
    return $ (responceID,) $ if isScreen == 1
        then Just (screenID, buttons)
        else Nothing

getScreenSize :: Get (Word16, Word16)
getScreenSize = liftM2 (,) getWord16be getWord16be

getResponseID :: Get ResponseID
getResponseID = do
    n <- getWord8
    return $ case n of
        0x01 -> NormalResponse
        0x11 -> TransitionUnavailableByNoCorrespondingScreenIdAndKeyId 
        0x12 -> TransitionUnavailableByInoperableKey
        0x13 -> TransitionUnavailableByOperatedByTrainCrew
        0x1F -> TransitionUnavailableOthersReason
        0x21 -> ResponseUnavailableByAccessedByOtherServer
        0x22 -> ResponseUnavailableByATCNoInUTO
        0x2F -> ResponseUnavailableByOthersReason
        _    -> ResponseUnavailableByOthersReason

getScreenID :: Get ScreenID
getScreenID = liftM ScreenID getWord16be

getKeyID :: Get KeyID
getKeyID = liftM KeyID getWord8

decodeButtons :: Get [(KeyID, (Word16, Word16, Word16, Word16))]
decodeButtons = liftM catMaybes $ replicateM 256 decodeButton

decodeButton :: Get (Maybe (KeyID, (Word16, Word16, Word16, Word16)))
decodeButton = do
    isOperableKey <- getWord8
    keyID <- getKeyID
    skip 2
    x1 <- getWord16be
    y1 <- getWord16be
    x2 <- getWord16be
    y2 <- getWord16be
    if isOperableKey == 1
        then return $ Just
            (keyID,
                ( min x1 x2
                , max x1 x2
                , min y1 y2
                , max y1 y2
                )
            )
        else return Nothing

getKeyIDs :: (Word16, Word16) -> [(KeyID, (Word16, Word16, Word16, Word16))] -> Maybe KeyID
getKeyIDs pos buttons = liftM fst $ find (isClicked pos . snd) buttons

isClicked :: (Word16, Word16) -> (Word16, Word16, Word16, Word16) -> Bool
isClicked (x, y) (xMin, xMax, yMin, yMax) = and
    [ xMin < x
    , x < xMax
    , yMin < y
    , y < yMax
    ]

toWord16Point :: SDL.Point SDL.V2 Int32 -> (Word16, Word16)
toWord16Point p = let SDL.V2 x y = unP $ fmap fromIntegral p in (x, y)
