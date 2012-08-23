import Network.HTTP                   (simpleHTTP, Request, mkRequest)
import Network.HTTP.Base              (Response(..), RequestMethod(..))
import Network.HTTP.Headers           (HeaderName(..), findHeader)
import Network.Stream                 (ConnError)
import Network.URI                    (parseURI)
import Data.ByteString.Lazy.Progress  (trackProgressString)
import qualified Data.ByteString.Lazy as BS
import System.IO                      (openBinaryFile, hClose, IOMode(..))
import System.Environment             (getArgs)

downloadFile :: String -> FilePath -> IO ()
downloadFile url path = do
  fhndl <- openBinaryFile path WriteMode
  http  <- simpleHTTP dbReq
  case http of
    Left x     -> fail $ "Couldn't download file: " ++ show x
    Right resp -> do
      let size = read `fmap` findHeader HdrContentLength resp
      putStrLn $ "Total size is " ++ show size ++ " bytes."
      track <- trackProgressString formatStr size handler
      track (rspBody resp) >>= BS.hPut fhndl
      hClose fhndl
      putStrLn "Done!"
 where
  dbReq     = mkRequest GET link
  Just link = parseURI url
  formatStr = "\r Downloading file ... %p (%R, estimated done in %T)"
  handler   = putStr

main :: IO ()
main = do
  downloadFile "http://ftp.ndlug.nd.edu/pub/fedora/linux/releases/16/Fedora/x86_64/iso/Fedora-16-x86_64-netinst.iso" "foo.iso"
