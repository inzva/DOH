module Main where

import           Control.Monad.Reader
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import           System.IO (hGetContents, hPutStr, hSeek, openBinaryTempFile, SeekMode (..))
import           Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import           Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import           Network.DigitalOcean

propGetAccounts :: Property
propGetAccounts = monadicIO $ do writtenData <- pick arbitrary
                                 pre $ not (null writtenData)
                                 readData <- run getLine
                                 assert $ writtenData == readData
                  
main :: IO ()
main =
  quickCheck propGetAccounts
