module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

main :: IO ()
main = do
  readRefs Nothing >>= print
  return ()

newtype Sha = Sha ByteString
  deriving Show

newtype Ref = Ref Text
  deriving Show

readRefs :: Maybe Ref -> IO [(Ref, Sha)]
readRefs mref = do
  (code, out, _) <- readProcessWithExitCode "git"
    ([ "show-ref", "--" ] ++ maybe [] s mref)
    ""
  if code == ExitSuccess
    then return . map (p . words) $ lines out
    else error "git failed"

  where s (Ref r) =[ T.unpack r]
        p [sha, r] = (Ref $ T.pack r, Sha $ BC.pack sha)
        p _ = error "unexpected git output"
