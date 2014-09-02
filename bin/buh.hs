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
import qualified Data.ByteString.Lazy.Internal as I
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.Int
import Data.IORef
import Data.List (sortBy)
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Format (formatTime)
import Data.Thyme.Time () -- For instance Num POSIXTime (a.k.a. NominalDiffTime)
import Data.Thyme.Time.Core (posixSecondsToUTCTime)
import Data.Word
import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (splitDirectories)
import System.IO
  ( hClose, hFlush, hSeek, hSetFileSize, openFile, Handle, IOMode(..)
  , SeekMode(AbsoluteSeek)
  )
import System.IO.Unsafe (unsafeInterleaveIO)
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
    ["init", path] -> do
      e <- doesDirectoryExist path
      if e
        then error $ "directory " ++ path ++ " already exist"
        else do
          (_, _, _, p) <- createProcess (proc "git"
            [ "init", "--bare", path
            ])
          _ <- waitForProcess p
          return ()
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
    ["ls"] -> enterMasterLatest ls []
    ["ls", "/"] -> do
      putStrLn "blob" -- access logical blobs, not chunks
      putStrLn "branch"
      putStrLn "chunk" -- access all blobs
      putStrLn "commit"
      putStrLn "time" -- same as commit but using the bup format YY-MM-DD-hhmmss instead of a hash
    ["ls", path] -> do
      case splitDirectories path of
        ["/", "branch"] -> do
          hds <- readHeads Nothing
          mapM_ (BC.putStrLn . fst) hds
        rest -> enterMasterLatest ls rest
    ["cat", path] -> do
      case splitDirectories path of
        rest -> enterMasterLatest cat rest
    ["pack"] -> do
      packer <- newPack "objects/pack/p1.pack"
      sha <- pack packer $ Blob 15 "Nice isn't it?\n"
      sha' <- pack packer $ Tree [(normalFile, "README.md", sha)]
      Ref sha'' <- pack packer $ Commit (Just sha') Nothing ""
      completePack packer
      (_, _, _, p) <- createProcess (proc "git"
        [ "index-pack", "-v", "objects/pack/p1.pack"
        ])
      _ <- waitForProcess p
      B.writeFile "refs/heads/master" $ sha'' `BC.append` "\n"

      hds <- readHeads Nothing
      Sha sha <- lookupPath "master" hds
      cs <- readRevs (Ref sha)
      Ref sha' <- lookupPath "latest" cs
      Commit (Just tree) _ _ <- readCommit $ Ref sha'

      packer <- newPack "objects/pack/p2.pack"
      sha <- rewrite packer [] (["new.txt"], Blob 6 "hello\n") tree
      Ref sha'' <- pack packer $ Commit (Just sha) (Just $ Ref sha'') "Added new.txt."
      completePack packer
      (_, _, _, p) <- createProcess (proc "git"
        [ "index-pack", "-v", "objects/pack/p2.pack"
        ])
      _ <- waitForProcess p
      B.writeFile "refs/heads/new-branch" $ sha'' `BC.append` "\n"
      return ()

    xs -> error $ "TODO " ++ show xs

enterMasterLatest :: (String -> Object -> IO a) -> [String] -> IO a
enterMasterLatest f path = do
  hds <- readHeads Nothing
  Sha sha <- lookupPath "master" hds
  cs <- readRevs (Ref sha)
  Ref sha' <- lookupPath "latest" cs
  Commit (Just tree) _ _ <- readCommit $ Ref sha'
  enter f "latest" path tree

ls :: String -> Object -> IO ()
ls p o = case o of
  Commit _ _ _ -> error "resolve to a commit"
  Tree es -> mapM_ (BC.putStrLn . fst) $ treeToRefs es
  Blob _ _ -> putStrLn p

cat :: String -> Object -> IO ()
cat _ o = case o of
  Commit _ _ _ -> error "resolve to a commit"
  Tree _ -> error "is a directory"
  Blob _ bs -> L.putStr bs

enter :: (String -> Object -> IO a) -> String -> [String] -> Ref -> IO a
enter f p ps ref = do
  o <- readObject ref
  case ps of
    p':ps' -> case o of
      Blob _ _ -> error $ "no file '" ++ p' ++ "'"
      Tree es -> do
        ref' <- lookupPath (BC.pack p') $ treeToRefs es
        enter f p' ps' ref'
      Commit _ _ _ -> error "Deref the tree ?"
    [] -> f p o

lookupPath :: ByteString -> [(ByteString, a)] -> IO a
lookupPath k es = do
  case lookup k es of
    Just v -> return v
    _ -> error $ "no file '" ++ BC.unpack k ++ "'" -- TODO exitFailure

-- TODO Handle only the most simple case.
rewrite packer ps ([name], blob) ref = do
  o <- readObject ref
  case ps of
    p':ps' -> case o of
      Blob _ _ -> error $ "todo"
      Tree es -> undefined
      Commit _ _ _ -> error "todo"
    [] -> case o of
      Blob _ _ -> error $ "todo"
      Tree es -> do
        sha <- pack packer blob
        let e = (normalFile, name, sha)
        pack packer $ Tree $ e : remove name es
      Commit _ _ _ -> error "Deref the tree ?"

remove p = filter ((/= p) . fst')
  where fst' (a, _, _) = a

----------------------------------------------------------------------
-- Read git objects and refs
----------------------------------------------------------------------

-- | Convert a Tree to the type returned by readHeads.
treeToRefs :: [(ByteString, ByteString, Ref)] -> [(ByteString, Ref)]
treeToRefs = map (\(_, b, c) -> (b, c))

newtype Sha = Sha ByteString
  deriving (Eq, Show)

newtype Ref = Ref { unRef ::ByteString } -- TODO Can it actually be non-ascii ?
  deriving Show

data Object =
    Blob Int64 L.ByteString
  | Tree [(ByteString, ByteString, Ref)] -- ^ Mode, name, sha
  | Commit (Maybe Ref) (Maybe Ref) ByteString -- ^ Tree ref, parent ref, message.
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
              "blob" -> return $ Blob (fromInteger size) o
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

toStrict :: L.ByteString -> ByteString
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
    Blob _ _ -> return o
    _ -> error $ "expected blob object"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a commit.
readCommit :: Ref -> IO Object
readCommit ref = do
  o <- readObject ref
  case o of
    Commit _ _ _ -> return o
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
    _ <- A.string "tree " -- TODO Optional.
    treeSha <- A.takeWhile isSha
    when (B.length treeSha /= 40) $ error "unexpected tree ref length"
    _ <- A.char '\n'
    -- TODO
    _ <- A.takeByteString
    A.endOfInput
    return $ Commit (Just $ Ref treeSha) Nothing ""

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
  p xs (l1:l2:rest) | "commit":_ <- words l2 =
    p ((Ref . BC.pack $ drop 7 l1, posixSecondsToUTCTime . fromInteger $ 0) : xs) (l2:rest)
  p xs (l1:l2:rest) =
    p ((Ref . BC.pack $ drop 7 l1, posixSecondsToUTCTime . fromInteger $ read l2) : xs) rest
  p xs [l1] | "commit":_ <- words l1 =
    p ((Ref . BC.pack $ drop 7 l1, posixSecondsToUTCTime . fromInteger $ 0) : xs) []
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
--
-- A packfile can be verified with `git verify-pack`. It needs a corresponding
-- `.idx` file which be can generated with `git index-pack`. E.g.:
--
--     > git index-pack t.pack
--     > git verify-pack -v t.pack
--     32ab47487f560b11fdc758eedd9b43ee7aeb8684 blob   749 349 12
--     non delta: 1 object
--     t.pack: ok
--
-- Those two commands don't need a a Git repository to work. On the other
-- hand, to use a command such as `git cat-file`, a real Git repository must
-- be provided:
--
--     > git init --bare repo
--     > cp t.{idx,pack} repo/objects/pack/
--     > cd repo
--     > echo 32ab47487f560b11fdc758eedd9b43ee7aeb8684 | git cat-file --batch
--     32ab47487f560b11fdc758eedd9b43ee7aeb8684 blob 749
--     ... blob content ...
--
-- If the packfile contain a commit, we can pretend HEAD points to it, inspect
-- it, or even do a checkout:
--
--     > echo 709149cd69d4e13c8740e5bb3d832f97fcb08878 > refs/heads/master
--     > git log
--     commit 709149cd69d4e13c8740e5bb3d832f97fcb08878 
--
--     > mkdir ../work
--     > GIT_WORK_TREE=../work git checkout master
--     Already on 'master'
--
----------------------------------------------------------------------

-- | Incrementally build a pack.
newPack :: FilePath -> IO Packer
newPack fn = do
  h <- openFile fn ReadWriteMode
  hSetFileSize h 0 -- TODO Instead use a temporary (and thus new) file,
                   -- moving it to the correct path when it is complete.
  -- The number of objects will be set in `completePack`.
  BC.hPut h "PACK\0\0\0\2\0\0\0\0"
  counter <- newIORef 0
  return Packer
    { packerPack = \o -> do
        modifyIORef counter succ
        let (sha, bs) = runPutM $ putObject o
        L.hPut h bs
        return sha
    , packerComplete = do
        hSeek h AbsoluteSeek 8
        n <- readIORef counter
        L.hPut h . runPut . putWord32be $ n
        hSeek h AbsoluteSeek 0
        content <- hReadAll h
        let sha = bytestringDigest $ sha1 content
        L.hPut h sha
        hClose h
    }

-- | This is the function hGetContentsN from the bytestring package, minus the
-- handle closing bit of code.
hReadAll :: Handle -> IO L.ByteString
hReadAll h = lazyRead -- TODO close on exceptions
  where
  lazyRead = unsafeInterleaveIO loop
  loop = do
    c <- B.hGetSome h I.defaultChunkSize -- only blocks if there is no data available
    if B.null c
      then return I.Empty
      else do
        cs <- lazyRead
        return $ I.Chunk c cs

pack :: Packer -> Object -> IO Ref
pack packer = packerPack packer

pack_ :: Packer -> Object -> IO ()
pack_ packer o = packerPack packer o >> return ()

completePack :: Packer -> IO ()
completePack = packerComplete

data Packer = Packer
  { packerPack :: Object -> IO Ref
  , packerComplete :: IO ()
  }

-- | Write a packfile. The content of the packfile is provided as a
-- `Data.Binary.Put` serializer. The number of objects must be provided
-- explicitely.
writePack :: FilePath -> Int -> Put -> IO ()
writePack fn n os = L.writeFile fn p
  -- TODO Compute the SHA1 sum on-the-fly.
  where p_ = runPut $ buildPack n os
        sha = bytestringDigest $ sha1 p_
        p = p_ `L.append` sha

-- | Build a packfile, minus its SHA1 sum.
buildPack :: Int -> Put -> Put
buildPack n os = do
  putByteString "PACK\0\0\0\2"
  putWord32be . fromIntegral $ n
  os

-- | Serialize an object, using the packfile format.
putObject :: Object -> PutM Ref
putObject o = case o of
  Blob size bs -> do
    putLength 3 size -- Assume that L.length bs == size.
    putLazyByteString $ compress bs
    return $ computeSha o
  Tree es -> do
    let bs = runPut $ putTree es
    putLength 2 $ L.length bs
    putLazyByteString $ compress bs
    return $ computeSha o
  Commit mtree mparent msg -> do
    let bs = runPut $ putCommit mtree mparent Nothing Nothing msg
    putLength 1 $ L.length bs
    putLazyByteString $ compress bs
    return $ computeSha o

-- | Each object stored in a packfile still retain its loose object SHA1 sum.
computeSha :: Object -> Ref
computeSha o =
    Ref . B16.encode . toStrict . bytestringDigest . sha1 . runPut $ putLoose o

-- | Serialize an object using the loose format (but not yet zlib compressed).
putLoose :: Object -> Put
putLoose o = case o of
  Blob size bs -> do
    putByteString "blob "
    putByteString (BC.pack $ show size)
    putWord8 0
    putLazyByteString bs
  Tree es -> do
    let bs = runPut $ putTree es
    putByteString "tree "
    putByteString (BC.pack $ show $ L.length bs)
    putWord8 0
    putLazyByteString bs
  Commit mtree mparent msg -> do
    let bs = runPut $ putCommit mtree mparent Nothing Nothing msg
    putByteString "commit "
    putByteString (BC.pack $ show $ L.length bs)
    putWord8 0
    putLazyByteString bs

-- | Variable length unsigned integer encoding, used in the packfile format.
-- The type of the object is included.
putLength :: Word8 -> Int64 -> Put
putLength t n = loop size b
  where
  --  Object type is in the three last bits of the first nibble
  --  The first bit (not yet set) is the "continue" bit.
  --   /                             / The second nibble contains the size.
  b = (shiftL t 4) .|. (fromIntegral n .&. 0x0f)
  size = shiftR n 4
  loop sz c =
    if sz /= 0
    then do
      putWord8 $ c .|. 0x80 -- set the "continue"
      loop (shiftR sz 7) (fromIntegral sz .&. 0x7f) -- and continue with the next 7 bits
    else putWord8 c

-- | Write triple (Mode, name, sha) as a `tree` object in the packfile format.
putTree :: [(ByteString, ByteString, Ref)] -> Put
putTree es = mapM_ putEntry es'
  where
  es' = sortBy filenames es
  filenames (mode1, n1, _) (mode2, n2, _) = compare (f mode1 n1) (f mode2 n2)
    where f mode n = if mode == subdirectory then n `B.append` "/" else n
  putEntry (mode, name, Ref sha) = do
    putByteString mode
    putWord8 32 -- that is ' '
    putByteString name
    putWord8 0
    case B16.decode sha of
      (sha', rest) | B.null rest -> putByteString sha'
      _ -> error "putEntry: invalid sha"

putCommit :: Maybe Ref -> Maybe Ref -> Maybe (ByteString, UTCTime)
  -> Maybe (ByteString, UTCTime) -> ByteString -> Put
putCommit mtree mparent mauthor mcommitter msg = do
  let opt s f m = do
        maybe (return ()) (\v -> do
          putByteString s
          putByteString $ f v
          putWord8 10 -- that is '\n'
          ) m
  opt "tree " unRef mtree
  opt "parent " unRef mparent
  -- TODO
  putWord8 10
  putByteString msg

normalFile :: ByteString
normalFile = "100644"

subdirectory :: ByteString
subdirectory = "040000"
