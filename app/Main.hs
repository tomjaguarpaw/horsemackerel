{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  input <- BS.getContents
  case decode (BL.fromStrict input) :: Maybe Value of
    Just v  -> writeFile "/tmp/horsemackerel.out" (show v)
    Nothing -> writeFile "/tmp/horsemackerel.out" "Invalid JSON"
