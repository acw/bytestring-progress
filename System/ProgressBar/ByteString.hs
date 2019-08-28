{-# LANGUAGE OverloadedStrings #-}
module System.ProgressBar.ByteString(
         mkByteStringProgressBar
       , mkByteStringProgressWriter
       , fileReadProgressBar
       , fileReadProgressWriter
       )
 where

import Data.ByteString.Lazy(ByteString,hGetContents)
import Data.ByteString.Lazy.Progress
import Data.Text.Lazy(Text)
import qualified Data.Text.Lazy.IO as T
import Data.Time.Clock(getCurrentTime)
import System.IO(Handle,hSetBuffering,hPutChar,hPutStr,BufferMode(..))
import System.IO(openFile,hFileSize,IOMode(..))
import System.ProgressBar(Label, Progress(Progress), ProgressBarWidth(..),
                          Style(..), Timing(..))
import System.ProgressBar(defStyle, renderProgressBar)

type ℤ = Integer

-- |Track the progress of a ByteString as it is consumed by some computation.
-- This is the most general version in the library, and will render a progress
-- string and pass it to the given function. See other functions for interacting
-- with fixed-size files, the console, or generic Handles.
mkByteStringProgressBar :: ByteString {- The ByteString to track. -} ->
                           (Text -> IO ()) {- ^Function to call on update.-}->
                           ℤ     {- ^ Progress bar width -}         ->
                           ℤ     {- ^ The size of the ByteString -} ->
                           Label () {- ^ Prefixed label -}           ->
                           Label () {- ^ Postfixed label -}          ->
                           IO ByteString
mkByteStringProgressBar input tracker width size prefix postfix =
  do start <- getCurrentTime
     trackProgressWithChunkSize bestSize (updateFunction start) input
 where
  style = defStyle{ stylePrefix  = prefix
                  , stylePostfix = postfix
                  , styleWidth   = ConstantWidth (fromIntegral width) }
  bestSize | size `div` 100 < 4096  = fromIntegral $ size `div` 100
           | size `div` 100 < 16384 = 4096
           | otherwise              = 16384
  updateFunction start _ newAmt           =
    do now <- getCurrentTime
       let progress = Progress (fromIntegral newAmt) (fromIntegral size) ()
           timing = Timing start now
       tracker $ renderProgressBar style progress timing

-- |As mkByteStringProgressBar, but simply print the output to the given
-- Handle instead of using a callback.
mkByteStringProgressWriter :: ByteString {- ^ The ByteString to track. -} ->
                              Handle {- ^ Handle to write to -} ->
                              ℤ {- ^ Progress bar width -} ->
                              ℤ {- ^ The size of the ByteString -} ->
                              Label () {- ^ Prefixed label -} ->
                              Label () {- ^ Postfixed label -} ->
                              IO ByteString
mkByteStringProgressWriter input handle width size prefix postfix = do
  hSetBuffering handle NoBuffering
  mkByteStringProgressBar input tracker width size prefix postfix
 where
  tracker str = T.hPutStr handle "\r" >> T.hPutStr handle str

-- |Track the loading of a file as it is consumed by some computation. The
-- use of this function should be essentially similar to ByteString's
-- readFile, but with a lot more arguments and side effects.
fileReadProgressBar :: FilePath {- ^ The file to load. -} ->
                       (Text -> IO ()) {- ^ Function to call on update. -} ->
                       ℤ {- ^ Progress bar width -} ->
                       Label () {- ^ Prefixed label -} ->
                       Label () {- ^ Postfixed label -} ->
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
                          Label () {- ^ Prefixed label -} ->
                          Label () {- ^ Postfixed label -} ->
                          IO ByteString
fileReadProgressWriter path handle width prefix postfix = do
  inHandle   <- openFile path ReadMode
  size       <- hFileSize inHandle
  bytestring <- hGetContents inHandle
  mkByteStringProgressWriter bytestring handle width size prefix postfix

