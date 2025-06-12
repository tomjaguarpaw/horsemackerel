{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Bluefin.Eff (Eff, runEff, (:>))
import Bluefin.IO (IOE, effIO)
import Bluefin.Stream (Stream, forEach, yield)
import Bluefin.System.IO
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import System.IO (IOMode (WriteMode))
import System.FilePath (takeDirectory)
import System.Process.Typed

main :: IO ()
main = runEff $ \io -> do
  withFile io "/tmp/horsemackerel.out" WriteMode $ \h -> do
    forEach
      ( \ystdout -> do
          forEach
            ( \yfile -> do
                work io ystdout yfile
            )
            (hPutStr h)
      )
      (effIO io . putStrLn)

work ::
  (e1 :> es, e2 :> es, e3 :> es) =>
  IOE e1 ->
  Stream String e2 ->
  Stream String e3 ->
  Eff es ()
work io ystdout yfile = do
  yield ystdout "horsemackerel Running..."
  input <- effIO io BS.getContents
  yield ystdout "Getting contents..."
  case decode (BL.fromStrict input) :: Maybe Value of
    Nothing -> yield yfile "Invalid JSON"
    Just v -> do
      yield yfile (show v)

      case parseMaybe extractFilePath v of
        Nothing -> yield yfile "Couldn't parse"
        Just s -> do
          yield yfile s
          let base = takeDirectory s
          (_exitCode, stdout, _stderr) <-
              effIO io $ readProcess (proc "git" ["-C", base, "remote", "-v"])
          yield yfile $ "Remotes:\n" ++ BS.unpack (BL.toStrict stdout)

  yield ystdout "Done"

extractFilePath :: Value -> Parser String
extractFilePath = withObject "root" $ \o ->
  o .: "payload" >>= withObject "payload" (\p -> p .: "file_path")
