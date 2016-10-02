module System.ProgressBar.ByteString(
         mkByteStringProgressBar
       , mkByteStringProgressWriter
       , fileReadProgressBar
       , fileReadProgressWriter
       )
 where

import Data.ByteString.Lazy(ByteString,hGetContents)
import Data.ByteString.Lazy.Progress
import System.IO(Handle,hSetBuffering,hPutChar,hPutStr,BufferMode(..))
import System.IO(openFile,hFileSize,IOMode(..))
import System.ProgressBar(Label, mkProgressBar)

type ℤ = Integer

-- |Track the progress of a ByteString as it is consumed by some computation.
-- This is the most general version in the library, and will render a progress
-- string and pass it to the given function. See other functions for interacting
-- with fixed-size files, the console, or generic Handles.
mkByteStringProgressBar :: ByteString {- The ByteString to track. -} ->
                           (String -> IO ()) {- ^Function to call on update.-}->
                           ℤ     {- ^ Progress bar width -}         ->
                           ℤ     {- ^ The size of the ByteString -} ->
                           Label {- ^ Prefixed label -}             ->
                           Label {- ^ Postfixed label -}            ->
                           IO ByteString
mkByteStringProgressBar input tracker width size prefix postfix =
  trackProgressWithChunkSize bestSize updateFunction input
 where
  bestSize | size `div` 100 < 4096  = fromIntegral $ size `div` 100
           | size `div` 100 < 16384 = 4096
           | otherwise              = 16384
  updateProgressBar                 = mkProgressBar prefix postfix width
  updateFunction _ now              =
    tracker $ updateProgressBar (fromIntegral now) size

-- |As mkByteStringProgressBar, but simply print the output to the given
-- Handle instead of using a callback.
mkByteStringProgressWriter :: ByteString {- ^ The ByteString to track. -} ->
                              Handle {- ^ Handle to write to -} ->
                              ℤ {- ^ Progress bar width -} ->
                              ℤ {- ^ The size of the ByteString -} ->
                              Label {- ^ Prefixed label -} ->
                              Label {- ^ Postfixed label -} ->
                              IO ByteString
mkByteStringProgressWriter input handle width size prefix postfix = do
  hSetBuffering handle NoBuffering
  mkByteStringProgressBar input tracker width size prefix postfix
 where
  tracker str = hPutChar handle '\r' >> hPutStr handle str

-- |Track the loading of a file as it is consumed by some computation. The
-- use of this function should be essentially similar to ByteString's
-- readFile, but with a lot more arguments and side effects.
fileReadProgressBar :: FilePath {- ^ The file to load. -} ->
                       (String -> IO ()) {- ^ Function to call on update. -} ->
                       ℤ {- ^ Progress bar width -} ->
                       Label {- ^ Prefixed label -} ->
                       Label {- ^ Postfixed label -} ->
                       IO ByteString
fileReadProgressBar path tracker width prefix postfix = do
  inHandle   <- openFile path ReadMode
  size       <- hFileSize inHandle
  bytestring <- hGetContents inHandle
  mkByteStringProgressBar bytestring tracker width size prefix postfix

-- |As fileReadProgressBar, but simply write the progress bar to the given
-- Handle instead of calling a generic function.
fileReadProgressWriter :: FilePath {- ^ The file to load. -} ->
                          Handle {- ^ Handle to write to -} ->
                          ℤ {- ^ Progress bar width -} ->
                          Label {- ^ Prefixed label -} ->
                          Label {- ^ Postfixed label -} ->
                          IO ByteString
fileReadProgressWriter path handle width prefix postfix = do
  inHandle   <- openFile path ReadMode
  size       <- hFileSize inHandle
  bytestring <- hGetContents inHandle
  mkByteStringProgressWriter bytestring handle width size prefix postfix

