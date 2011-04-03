{-# LANGUAGE ScopedTypeVariables #-}
-- |This module defines core functions for tracking the consumption of a
-- ByteString, as well as several helper functions for making tracking
-- ByteStrings easier.
module Data.ByteString.Lazy.Progress(
         trackProgress
       , trackProgressWithChunkSize
       --
       , trackProgressString 
       , trackProgressStringWithChunkSize 
       --
       , bytesToUnittedStr
       )
 where

import           Control.Applicative ((<$>))
import qualified Data.ByteString      as BSS
import           Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe          (isJust)
import           Data.Time.Clock     (getCurrentTime,diffUTCTime,UTCTime)
import           Data.Word           (Word64)
import           System.IO.Unsafe    (unsafeInterleaveIO)

-- |Given a function, return a bytestring that will call that function when it
-- is partially consumed. The Words provided to the function will be the number
-- of bytes that were just consumed and the total bytes consumed thus far.
trackProgress :: (Word64 -> Word64 -> IO ()) ->
                 ByteString ->
                 IO ByteString
trackProgress tracker inputBS =
  BS.fromChunks <$> runTrack 0 (BS.toChunks inputBS)
 where
  runTrack _ [] = return []
  runTrack x (fst:rest) = unsafeInterleaveIO $ do
    let amtRead = fromIntegral $ BSS.length fst
    tracker amtRead (x + amtRead)
    (fst :) <$> runTrack (x + amtRead) rest

-- |Works like 'trackProgress', except uses fixed-size chunks of the given
-- size.  Thus, for this function, the first number passed to your function
-- will always be the given size *except* for the last call to the function,
-- which will be less then or equal to the final size.
trackProgressWithChunkSize :: Word64 -> (Word64 -> Word64 -> IO ()) ->
                              ByteString ->
                              IO ByteString
trackProgressWithChunkSize chunkSize tracker inputBS = runLoop 0  inputBS
 where
  runLoop x bstr | BS.null bstr = return BS.empty
                 | otherwise    = unsafeInterleaveIO $ do
    let (first,rest) = BS.splitAt (fromIntegral chunkSize) bstr
        amtRead      = fromIntegral (BS.length first)
    tracker amtRead (x + amtRead)
    (first `BS.append`) <$> runLoop (x + amtRead) rest

-- |Given a format string (described below), track the progress of a function.
-- The argument to the callback will be the string expanded with the given
-- progress information.
--
-- Format string items:
--
--   * %b is the number of bytes read
--
--   * %B is the number of bytes read, formatted into a human-readable string
--
--   * %c is the size of the last chunk read
--
--   * %C is the size of the last chunk read, formatted human-readably
--
--   * %r is the rate in bytes per second
--
--   * %R is the rate, formatted human-readably
--
--   * %% is the character '%'
--
-- If you provide a total size (the maybe argument, in bytes), then you may
-- also use the following items:
--
--   * %t is the estimated time to completion in seconds
--
--   * %T is the estimated time to completion, formatted as HH:MM:SS
--
--   * %p is the percentage complete
--
trackProgressString :: String -> Maybe Word64 -> (String -> IO ()) ->
                       IO (ByteString -> IO ByteString)
trackProgressString formatStr mTotal tracker = do
  startTime <- getCurrentTime
  return (trackProgress (handler startTime))
 where
  handler startTime chunkSize total = do
    now <- getCurrentTime
    tracker (buildString formatStr startTime now mTotal chunkSize total)

-- |Exactly as 'trackProgressString', but use the given chunkSize instead
-- of the default chunk size.
trackProgressStringWithChunkSize :: String -- ^the format string
                                    -> Word64 -- ^the chunk size
                                    -> Maybe Word64 -- ^total size (opt.)
                                    -> (String -> IO ()) -- ^the action
                                    -> IO (ByteString -> IO ByteString)
trackProgressStringWithChunkSize formatStr chunk mTotal tracker = do
  startTime <- getCurrentTime
  return (trackProgressWithChunkSize chunk (handler startTime))
 where
  handler startTime chunkSize total = do
    now <- getCurrentTime
    tracker (buildString formatStr startTime now mTotal chunkSize total)

-- build a progress string for trackProgressString et al
buildString :: String ->
               UTCTime -> UTCTime -> Maybe Word64 -> Word64 -> Word64 ->
               String
buildString form startTime curTime mTotal chunkSize amtRead = subPercents form
 where
  per_b = show amtRead
  per_B = bytesToUnittedStr amtRead
  per_c = show chunkSize
  per_C = bytesToUnittedStr chunkSize
  diff  = max 1 (round $ toRational $ diffUTCTime curTime startTime)
  rate  = amtRead `div` diff
  per_r = show rate
  per_R = bytesToUnittedStr rate ++ "ps"
  total = case mTotal of
            Just t  -> t
            Nothing -> error "INTERNAL ERROR (needed total w/ Nothing)"
  tleft = (total - amtRead) `div` rate
  per_t = show tleft
  hLeft = tleft `div` (60 * 60)
  mLeft = (tleft `div` 60) `mod` 60
  sLeft =  tleft           `mod` 60
  per_T = showPadded hLeft ++ ":" ++ showPadded mLeft ++
          ":" ++  showPadded sLeft
  perc  = 100 * (fromIntegral amtRead / fromIntegral total) :: Double
  per_p = show (round perc) ++ "%"
  oktot = isJust mTotal
  --
  subPercents []         = []
  subPercents ('%':rest) = subPercents' rest
  subPercents (x:rest)   = x : subPercents rest
  --
  subPercents' []                 = []
  subPercents' ('b':rest)         = per_b ++ subPercents rest
  subPercents' ('B':rest)         = per_B ++ subPercents rest
  subPercents' ('c':rest)         = per_c ++ subPercents rest
  subPercents' ('C':rest)         = per_C ++ subPercents rest
  subPercents' ('r':rest)         = per_r ++ subPercents rest
  subPercents' ('R':rest)         = per_R ++ subPercents rest
  subPercents' ('t':rest) | oktot = per_t ++ subPercents rest
  subPercents' ('T':rest) | oktot = per_T ++ subPercents rest
  subPercents' ('p':rest) | oktot = per_p ++ subPercents rest
  subPercents' ('%':rest)         = "%"   ++ subPercents rest
  subPercents' (x:rest)           = '%' : ('x' : subPercents rest)

-- show a number padded to force at least two digits.
showPadded :: Show a => a -> String
showPadded x = prefix ++ base
 where
  base   = show x
  prefix = case base of
             []  -> "00"
             [x] ->  "0"
             _   ->   ""

-- |Convert a number of bytes to a string represenation that uses a reasonable
-- unit to make the number human-readable.
bytesToUnittedStr :: Word64 -> String
bytesToUnittedStr x
  | x < bk_brk = show x ++ "b"
  | x < km_brk = showHundredthsDiv x k ++ "k"
  | x < mg_brk = showHundredthsDiv x m ++ "m"
  | otherwise  = showHundredthsDiv x g ++ "g"
 where
  bk_brk = 4096
  km_brk = 768 * k
  mg_brk = 768 * m
  --
  k      = 1024
  m      = 1024 * k
  g      = 1024 * m

-- Divide the first number by the second, and convert to a string showing two
-- decimal places.  
showHundredthsDiv   _    0 = error "Should never happen!"
showHundredthsDiv amt size = show ones ++ "." ++ show tenths ++ show hundreths
 where
  divRes :: Double = fromIntegral amt / fromIntegral size
  divRes100        = round (divRes * 100)
  ones             =  divRes100 `div` 100
  tenths           = (divRes100 `div` 10) `mod` 10
  hundreths        =  divRes100           `mod` 10


