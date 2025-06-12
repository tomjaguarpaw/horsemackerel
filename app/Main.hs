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
import Data.Foldable (for_)
import Data.List (nub, stripPrefix)
import System.FilePath (takeDirectory)
import System.IO (IOMode (WriteMode))
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

      case parseMaybe extract v of
        Nothing -> yield yfile "Couldn't parse"
        Just (s, line) -> do
          yield yfile s
          let base = takeDirectory s
          (_exitCode, stdout, _stderr) <-
            effIO io $ readProcess (proc "git" ["-C", base, "remote", "-v"])
          let remotesOutput = BS.unpack (BL.toStrict stdout)
          yield yfile $ "Remotes:\n" ++ remotesOutput

          let remotes = parseRemotes remotesOutput

          (_, gitdirDotGit, _) <-
            effIO io $ readProcess (proc "git" ["-C", base, "rev-parse", "--git-dir"])

          let gitDirDotGitString = BS.unpack (BL.toStrict gitdirDotGit)

          gitdir <- case stripSuffix ".git\n" gitDirDotGitString of
            Nothing -> do
              yield ystdout (show gitDirDotGitString)
              error ("couldn't strip .git from " <> gitDirDotGitString)
            Just g -> pure g

          gitdir_ <- case stripPrefix gitdir s of
            Nothing -> do
              yield ystdout (show (gitdir, s))
              error "couldn't strip gitdir"
            Just f -> pure f

          (_, hashNL, _) <-
            effIO io $ readProcess (proc "git" ["-C", base, "rev-parse", "HEAD"])

          let hashNLString = BS.unpack (BL.toStrict hashNL)

          hash <- case stripSuffix "\n" hashNLString of
            Nothing -> do
              yield ystdout (show hashNLString)
              error ("couldn't strip .git from " <> gitDirDotGitString)
            Just g -> pure g

          for_ remotes $ \remote -> do
            yield ystdout $ webPageOf remote gitdir_ hash line

  yield ystdout "Done"

extractFilePath :: Value -> Parser String
extractFilePath = withObject "root" $ \o ->
  o .: "payload" >>= withObject "payload" (\p -> p .: "file_path")

extractLine :: Value -> Parser Int
extractLine = withObject "root" $ \o ->
  o .: "payload" >>= withObject "payload" (\p -> p .: "line_number")

extract :: Value -> Parser (String, Int)
extract v = (,) <$> extractFilePath v <*> extractLine v

parseRemotes :: String -> [String]
-- FIXME: nub is O(n^2)
parseRemotes s = nub $ do
  flip map (lines s) $ \line ->
    case words line of
      [_remoteName, remoteUrl, _remoteType] -> remoteUrl
      other -> error (show other)

webPageOf :: String -> String -> String -> Int -> String
webPageOf gitUrl path hash line = do
  let (host, gitFile) =
        case break (== ':') gitUrl of
          (host', ':' : gitFile') -> (host', gitFile')
          (_, _) -> error "Bad break"

  case host of
    "git@github.com" -> do
      let basePath = case stripSuffix ".git" gitFile of
            Just s -> s
            Nothing -> error "Basepath"
      "https://github.com/" <> basePath <> "/blob/" <> hash <> "/" <> path <> "#L" <> show line
    "git@git.groq.io" -> do
      let basePath = case stripSuffix ".git" gitFile of
            Just s -> s
            Nothing -> error "Basepath"
      "https://git.groq.io/" <> basePath <> "/-/blob/" <> hash <> "/" <> path <> "#L" <> show line
    other -> error ("Unknown: " <> other)

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suf s = fmap reverse (stripPrefix (reverse suf) (reverse s))

test :: Bool
test = do
  let remoteOutput =
        unlines
          [ "origin  git@github.com:tomjaguarpaw/ad.git (fetch)",
            "origin  git@github.com:tomjaguarpaw/ad.git (push)",
            "remotename    ssh://git@github.com/tomjaguarpaw/effectful.git (fetch)",
            "remotename    ssh://git@github.com/tomjaguarpaw/effectful.git (push)"
          ]

  let expected =
        ["git@github.com:tomjaguarpaw/ad.git",
          "ssh://git@github.com/tomjaguarpaw/effectful.git"]

  let b1 = parseRemotes remoteOutput == expected

  let b2 =
        webPageOf "git@github.com:tomjaguarpaw/ad.git" "Term/app/Main.hs" "abc" 12
          == "https://github.com/tomjaguarpaw/ad/blob/abc/Term/app/Main.hs#L12"

  let b3 =
        webPageOf "git@git.groq.io:code/Groq.git" "Term/app/Main.hs" "abc" 12
          == "https://git.groq.io/code/Groq/-/blob/abc/Term/app/Main.hs#L12"

  b1 && b2 && b3
