{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
module TCMS where

-- base
import Data.Word (Word8, Word16)
import Control.Monad (replicateM, replicateM_, liftM, liftM2, guard)
import Data.Bits (zeroBits)
-- deepseq
import Control.DeepSeq
-- iproute
import Data.IP (IPv4, toHostAddress)
-- bits
import qualified Data.Bits.Coded as B
import qualified Data.Bits.Coding as B
-- cereal
import Data.Serialize (PutM, Putter, Serialize, Get, putWord8, getWord8, putWord16be, getWord16be, putWord32be, skip)
-- time
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import Data.Time.Calendar
-- network
import Network.Socket (HostName)
-- bytes
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Get (MonadGet)

newtype RakeID = RakeID Word16 deriving (Ord, Eq)

putRakeIDAsBCD :: Putter RakeID
putRakeIDAsBCD (RakeID i) = do
    B.runEncode $ putBCD 2 8
    let (q, r) = i `quotRem` 10
    B.runEncode $ do
        putBitsFromWord16 3 q
        putBitsFromWord16 3 r

putBCD :: Int -> Int -> B.Coding PutM ()
putBCD numOfDigit i = mapM_ (putBitsFromWord8 3) $ reverse $ take numOfDigit $ makeDigits i
 where makeDigits :: Int -> [Word8]
       makeDigits i = let (q, r) = i `quotRem` 10
                      in fromIntegral r : makeDigits q

getBCD :: Int -> B.Coding Get Int
getBCD numOfDigit = do
    ds <- replicateM numOfDigit $ B.getBitsFrom 3 (zeroBits :: Word8)
    let ds' = reverse $ map fromIntegral ds
    return $ sum [d * 10 ^ n | (d, n) <- zip ds' [0..]]

skipBits :: MonadGet m => Int -> B.Coding m ()
skipBits i = replicateM_ i B.getBit

skipBit :: MonadGet m => B.Coding m ()
skipBit = skipBits 1

putZeroBits :: MonadPut m => Int -> B.Coding m ()
putZeroBits i = replicateM_ i $ B.putBit False

putZeroBytes :: Int -> PutM ()
putZeroBytes i = replicateM_ i $ putWord8 zeroBits

putBitsFromWord8 :: (MonadPut m) => Int -> Word8 -> B.Coding m ()
putBitsFromWord8 = B.putBitsFrom

putBitsFromWord16 :: (MonadPut m) => Int -> Word16 -> B.Coding m ()
putBitsFromWord16 = B.putBitsFrom

makeIPAddrsTCMS :: RakeID -> (HostName, HostName)
makeIPAddrsTCMS (RakeID n) = (concat ["8.", show n, ".65.24"], concat ["8.", show n, ".6.24"])

data TCMSHeader = TCMSHeader
    { tcmsCommunicationNumber :: !Word16
    , tcmsRetryCounter :: !Word16
    , tcmsRakeID :: !RakeID
    , tcmsSrcIPAddr :: !IPv4
    , tcmsDstIPAddr :: !IPv4
    , tcmsDstPortNumber :: !Word16
    , tcmsTransmissionTime :: !UTCTime
    }

putTCMSHeader :: Putter TCMSHeader
putTCMSHeader (TCMSHeader {..}) = do
    putWord8 0x40 -- message code
    putWord16be tcmsCommunicationNumber
    putWord8 0x00 -- flags
    putWord16be tcmsRetryCounter
    putWord16be 0x0000 -- data handling number
    B.runEncode $ putBCD 2 8
    putRakeIDAsBCD tcmsRakeID
    putZeroBytes 5 -- spare
    putWord16be 0x5900 -- src equipment number
    putWord32be $ toHostAddress tcmsSrcIPAddr
    putWord16be 0xFF10 -- dst equipment number
    putWord32be $ toHostAddress tcmsDstIPAddr
    putWord16be tcmsDstPortNumber
    putTCMSTransmissionTime tcmsTransmissionTime
    putWord8 0x00 -- flags
    putZeroBytes (62 - 36)
    putWord8 0x10

putTCMSTransmissionTime :: Putter UTCTime
putTCMSTransmissionTime utcTime = B.runEncode $ do
    putBCD 2 $ fromIntegral $ yyyy - 2000
    putBCD 2 $ fromIntegral $ month
    putBCD 2 $ fromIntegral $ dd
    putBCD 2 $ fromIntegral $ hh
    putBCD 2 $ fromIntegral $ mm
    putBCD 2 $ fromIntegral $ truncate $ ss
 where (LocalTime day (TimeOfDay hh mm ss)) = utcToLocalTime utc utcTime
       (yyyy, month, dd) = toGregorian day
