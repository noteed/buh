{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Main (main) where

import Codec.Compression.Zlib (compress)
import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.ByteString.Lazy (parse, Result(..))
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.Int
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Format (formatTime)
import Data.Thyme.Time () -- For instance Num POSIXTime (a.k.a. NominalDiffTime)
import Data.Thyme.Time.Core (posixSecondsToUTCTime)
import Data.Word
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (splitDirectories)
import System.IO (hClose, hFlush)
import System.Locale (defaultTimeLocale)
import System.Process
  ( createProcess, proc, readProcessWithExitCode, waitForProcess
  , CreateProcess(..), StdStream(..)
  )

----------------------------------------------------------------------
-- Command-line
----------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"] -> do
      readRefs Nothing >>= print
      readHeads Nothing >>= print
      readObjects
        [ Ref "32ab47487f560b11fdc758eedd9b43ee7aeb8684" -- blob
        , Ref "740fc0e4923e9f1ee5e0488cb1e7877c990a3f69" -- tree
        , Ref "a5b6d23259c76b66c933ba9940f6afcdf1bf3fff" -- commit
        ] >>= print
      revList (Ref "a5b6d23259c76b66c933ba9940f6afcdf1bf3fff") >>= print
      readRevs (Ref "a5b6d23259c76b66c933ba9940f6afcdf1bf3fff") >>= print
    ["ls"] -> do
      hds <- readHeads Nothing
      mapM_ (BC.putStrLn . fst) hds
    ["ls", path] -> do
      case splitDirectories path of
        [hd] -> do
          hds <- readHeads Nothing
          Sha sha <- lookupPath (BC.pack hd) hds
          cs <- readRevs (Ref sha)
          mapM_ (BC.putStrLn . fst) cs
        hd : name : rest -> do
          hds <- readHeads Nothing
          Sha sha <- lookupPath (BC.pack hd) hds
          cs <- readRevs (Ref sha)
          Ref sha <- lookupPath (BC.pack name) cs
          Commit tree <- readCommit $ Ref sha
          enter ls name rest tree
        _ -> error "TODO"
    ["cat-file", path] -> do
      case splitDirectories path of
        hd : name : rest -> do
          hds <- readHeads Nothing
          Sha sha <- lookupPath (BC.pack hd) hds
          cs <- readRevs (Ref sha)
          Ref sha <- lookupPath (BC.pack name) cs
          Commit tree <- readCommit $ Ref sha
          enter cat name rest tree
        _ -> error "TODO"
    ["pack"] -> do
      o <- readBlob $ Ref "32ab47487f560b11fdc758eedd9b43ee7aeb8684"
      writePack "/tmp/t.pack" [o]
    _ -> return ()

ls :: String -> Object -> IO ()
ls p o = case o of
  Commit _ -> error "resolve to a commit"
  Tree es -> mapM_ (BC.putStrLn . fst) $ treeToRefs es
  Blob _ _ _ -> putStrLn p

cat :: String -> Object -> IO ()
cat _ o = case o of
  Commit _ -> error "resolve to a commit"
  Tree es -> error "is a directory"
  Blob _ _ bs -> L.putStr bs

enter f p ps ref = do
  o <- readObject ref
  case ps of
    p':ps' -> case o of
      Blob _ _ _ -> error $ "no file '" ++ p' ++ "'"
      Tree es -> do
        ref <- lookupPath (BC.pack p') $ treeToRefs es
        enter f p' ps' ref
      Commit _ -> error "Deref the tree ?"
    [] -> f p o

lookupPath k es = do
  case lookup k es of
    Just v -> return v
    _ -> error $ "no file '" ++ BC.unpack k ++ "'" -- TODO exitFailure

----------------------------------------------------------------------
-- Read git objects and refs
----------------------------------------------------------------------

-- | Convert a Tree to the type returned by readHeads.
treeToRefs = map (\(_, b, c) -> (b, c))

newtype Sha = Sha ByteString
  deriving (Eq, Show)

newtype Ref = Ref { unRef ::ByteString } -- TODO Can it actually be non-ascii ?
  deriving Show

data Object =
    Blob Ref Int64 L.ByteString
  | Tree [(ByteString, ByteString, Ref)] -- ^ Mode, name, sha
  | Commit Ref -- ^ Tree ref.
  deriving Show

-- | `git show-ref`
readRefs :: Maybe Ref -> IO [(Ref, Sha)]
readRefs mref = do
  (code, out, _) <- readProcessWithExitCode "git"
    ([ "show-ref", "--" ] ++ maybe [] s mref)
    ""
  if code == ExitSuccess
    then return . map (p . words) $ lines out
    else error "git failed"

  where s (Ref r) =[ BC.unpack r]
        p [sha, r] = (Ref $ BC.pack r, Sha $ BC.pack sha)
        p _ = error "unexpected git-show-ref output"

-- | Like readRefs, but return only those matching `refs/heads`.
readHeads :: Maybe Ref -> IO [(ByteString, Sha)]
readHeads mref = do
  refs <- readRefs mref
  return $ map unref $ filter (prefix . fst) refs
  where unref (Ref r, sha) = (BC.drop 11 r, sha)
        prefix (Ref r) = BC.isPrefixOf "refs/heads/" r

-- | `git cat-file --batch`
readObjects :: [Ref] -> IO [Object]
readObjects refs = do
  (Just pIn, Just pOut, _, p) <- createProcess (proc "git"
    [ "cat-file", "--batch"
    ]) { std_in = CreatePipe, std_out = CreatePipe }
  let putRef (Ref r) = do
        BC.hPutStrLn pIn r
        hFlush pIn
      readOne = do
        ws <- BC.words <$> BC.hGetLine pOut
        case ws of
          [sha, typ, size_] | Just (size, _) <- BC.readInteger size_ -> do
            -- TODO hGet takes an Int, maybe we should read again if the Int64
            -- is really useful.
            o <- L.hGet pOut (fromInteger size)
            nl <- BC.hGetLine pOut
            when (nl /= "") $ error "unexpected git-cat-file output (1)"
            case typ of
              "blob" -> return $ Blob (Ref sha) (fromInteger size) o
              "tree" -> do
                let loop xs s = do
                      if L.null s
                        then return . Tree $ reverse xs
                        else do
                          -- Maybe rewrite this with attoparsec.
                          let (a, b) = LC.span (/= ' ') s
                              c = LC.drop 1 b
                              (d, d') = LC.span (/= '\0') c
                              e = LC.drop 1 d'
                              (f, g) = LC.splitAt 20 e
                          loop ((toStrict a,toStrict d, Ref . B16.encode $ toStrict f): xs) g
                loop [] o
              "commit" -> return $ parseCommit o
              _ -> error "unexpected git-cat-file output (2)"
          _ -> error "unexpected git-cat-file output (3)"
  os <- mapM (\r -> putRef r >> readOne) refs
  hClose pIn
  _ <- waitForProcess p
  return os

toStrict = B.concat . L.toChunks

-- | Similar to `readObjects` with a single ref.
readObject :: Ref -> IO Object
readObject ref = do
  os <- readObjects [ref]
  case os of
    [o] -> return o
    _ -> error $ "can't happen"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a blob.
readBlob :: Ref -> IO Object
readBlob ref = do
  o <- readObject ref
  case o of
    Blob _ _ _ -> return o
    _ -> error $ "expected blob object"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a commit.
readCommit :: Ref -> IO Object
readCommit ref = do
  o <- readObject ref
  case o of
    Commit _ -> return o
    _ -> error $ "expected commit object"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a tree.
readTree :: Ref -> IO Object
readTree ref = do
  o <- readObject ref
  case o of
    Tree _ -> return o
    _ -> error $ "expected tree object"

parseCommit :: L.ByteString -> Object
parseCommit bs = case parse p bs of
  Fail _ _ err -> error err
  Done _ r -> r
  where
  p = do
    _ <- A.string "tree "
    treeSha <- A.takeWhile isSha
    when (B.length treeSha /= 40) $ error "unexpected tree ref length"
    _ <- A.char '\n'
    -- TODO
    _ <- A.takeByteString
    A.endOfInput
    return $ Commit (Ref treeSha)

  isSha c = (c >= '0' && c <= '9') ||
            (c >= 'a' && c <= 'f') ||
            (c >= 'A' && c <= 'F')

-- | `git rev-list --pretty=format:%at`
revList :: Ref -> IO [(Ref, UTCTime)]
revList ref = do
  (code, out, _) <- readProcessWithExitCode "git"
    [ "rev-list", "--pretty=format:%at", BC.unpack $ unRef ref ]
    ""
  if code == ExitSuccess
    then return . p [] $ lines out
    else error "git failed"
  where
  -- TODO read
  p xs (l1:l2:rest) = p ((Ref . BC.pack $ drop 7 l1, posixSecondsToUTCTime . fromInteger $ read l2) : xs) rest
  p xs [] = xs
  p _ _ = error "unexpected line from git-rev-list"

-- | Similar to `revList` but the result type matches `readHeads`.
readRevs :: Ref -> IO [(ByteString, Ref)]
readRevs ref = do
  refs <- revList ref
  return . latest $ map f refs
  where
  f (r, t) = (BC.pack $ formatTime locale format t, r)
  locale = defaultTimeLocale
  format = "%Y-%m-%d-%H%M%S"
  latest (x@(_, r) : xs) = ("latest", r) : x : xs
  latest _ = []

----------------------------------------------------------------------
-- Write packfile
----------------------------------------------------------------------

writePack :: FilePath -> [Object] -> IO ()
writePack fn os = L.writeFile fn p
  -- TODO Compute the SHA1 sum on-the-fly.
  where p_ = runPut $ buildPack os
        sha = bytestringDigest $ sha1 p_
        p = p_ `L.append` sha

buildPack :: [Object] -> Put
buildPack os = do
  putByteString "PACK\0\0\0\2"
  putWord32be . fromIntegral $ length os
  mapM_ packObject os

packObject :: Object -> Put
packObject o = do
  case o of
    Blob _ size bs -> do
      encodeLength o size -- Assume that L.length bs == size.
      putLazyByteString $ compress bs
    -- TODO

-- | Variable length unsigned integer encoding.
encodeLength :: Object -> Int64 -> Put
encodeLength o n = loop size b
  where
  --  Object type is in the three last bits of the first nibble
  --  The first bit (not yet set) is the "continue" bit.
  --   /                             / The second nibble contains the size.
  b = (shiftL (objectType o) 4) .|. (fromIntegral n .&. 0x0f)
  size = shiftR n 4
  loop sz c =
    if sz /= 0
    then do
      putWord8 $ c .|. 0x80 -- set the "continue"
      loop (shiftR sz 7) (fromIntegral sz .&. 0x7f) -- and continue with the next 7 bits
    else putWord8 c

objectType :: Object -> Word8
objectType o = case o of
  Commit _ -> 1
  Tree _ -> 2
  Blob _ _ _ -> 3
  -- Tag -> 4
