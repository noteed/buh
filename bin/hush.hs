{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Codec.Compression.Zlib
  ( compressWith, decompress, defaultCompressParams, compressionLevel
  , CompressParams(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Digest.Pure.SHA (showDigest, sha1)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> storeStdin
    [sha] -> restoreStdout sha
    _ -> do
      BC.putStrLn "Usage."
      exitFailure


-- | Oversimple content-addressable storage:
-- Get content from stdin, store it as a Git blob under `/objects` and print
-- the Git SHA1 of the stored content to stdout.
-- TODO Use sha-streams instead of strict reading.
-- TODO Share code with buh.hs.
storeStdin :: IO ()
storeStdin = do
  content <- B.getContents
  let l = B.length content
      content' = B.concat ["blob ", BC.pack (show l), "\x00", content]
      sha = showDigest (sha1 content')
      compressed = compress' content'
  createDirectoryIfMissing True (sha1ToDirPath sha)
  B.writeFile (sha1ToPath sha) compressed
  putStrLn sha

-- | Given a SHA1, print to stdout the blob content, if any.
restoreStdout :: String -> IO ()
restoreStdout sha = do
  b <- doesFileExist (sha1ToPath sha)
  -- TODO Use also try/catch.
  if b
    then do
      content <- B.readFile (sha1ToPath sha)
      -- TODO Validate content (header, lenght and SHA1).
      let decompressed = decompress content
          content' = B.drop 1 (BC.dropWhile (/= '\x00')  decompressed)
      B.putStr content'
    else do
      BC.putStrLn "No such blob."
      exitFailure


sha1ToPath :: String -> FilePath
sha1ToPath sha =
  "/objects" </> take 2 sha </> drop 2 sha

sha1ToDirPath :: String -> FilePath
sha1ToDirPath sha =
  "/objects" </> take 2 sha

-- | Same compression than Git, at least on a simple "hello" string. Also,
-- maybe Git can be configured to change the compression level it uses.
compress' :: B.ByteString -> B.ByteString
compress' =
  compressWith defaultCompressParams { compressLevel = compressionLevel 1 }
