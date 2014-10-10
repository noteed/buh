{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Main (main) where

import Codec.Compression.Zlib (compress)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import qualified Control.Exception as C
import Control.Monad (forM, when)
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
import Data.List (groupBy, nubBy, sortBy)
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Format (formatTime)
import Data.Thyme.Time () -- For instance Num POSIXTime (a.k.a. NominalDiffTime)
import Data.Thyme.Time.Core (posixSecondsToUTCTime)
import Data.Word
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (joinPath, splitDirectories, (</>))
import System.IO
  ( hClose, hFlush, hGetContents, hPutStr, hSeek, hSetFileSize, openFile
  , Handle, IOMode(..), SeekMode(AbsoluteSeek)
  )
import qualified System.IO.Streams as S
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
    ["init", gitDir_] -> do
      gitDir <- canonicalizePath gitDir_
      e <- doesDirectoryExist gitDir
      if e
        then error $ "directory " ++ gitDir ++ " already exist"
        else initRepository gitDir
    ["ensure", gitDir_] -> do
      gitDir <- canonicalizePath gitDir_
      ensureGitDir gitDir
    ["test"] -> do
      gitDir <- canonicalizePath ".git"
      readRefs gitDir Nothing >>= print
      readHeads gitDir Nothing >>= print
      readObjects gitDir
        [ Ref "32ab47487f560b11fdc758eedd9b43ee7aeb8684" -- blob
        , Ref "740fc0e4923e9f1ee5e0488cb1e7877c990a3f69" -- tree
        , Ref "a5b6d23259c76b66c933ba9940f6afcdf1bf3fff" -- commit
        ] >>= print
      revList gitDir (Ref "a5b6d23259c76b66c933ba9940f6afcdf1bf3fff") >>= print
      readRevs gitDir (Ref "a5b6d23259c76b66c933ba9940f6afcdf1bf3fff") >>= print
    ["ls", gitDir_] -> do
      gitDir <- canonicalizePath gitDir_
      putStrLn "blob" -- access logical blobs, not chunks
      putStrLn "branch"
      putStrLn "chunk" -- access all blobs
      putStrLn "commit"
      putStrLn "time" -- same as commit but using the bup format YY-MM-DD-hhmmss instead of a hash
    ["ls", gitDir_, path] -> do
      gitDir <- canonicalizePath gitDir_
      case splitDirectories path of
        ["branch"] -> do
          hds <- readHeads gitDir Nothing
          mapM_ (BC.putStrLn . fst) hds
        ["branch", branch] -> do
          enterBranch gitDir branch
        "branch" : branch : commit : rest -> enterBranchCommit gitDir ls branch commit rest
    ["cat", gitDir_, path] -> do
      gitDir <- canonicalizePath gitDir_
      case splitDirectories path of
        "branch" : branch : commit : rest -> enterBranchCommit gitDir cat branch commit rest
    ["pack", gitDir_] -> do
      gitDir <- canonicalizePath gitDir_
      (sha, tree) <- repack gitDir "objects/pack/p1.pack"
        [L "README.md" $ Blob 15 "Nice isn't it?\n"]
        Nothing Nothing
        "Initial commit."
        "refs/heads/master"

      (sha2, tree2) <- repack gitDir "objects/pack/p2.pack"
        [L "new.txt" $ Blob 6 "hello\n"]
        (Just sha) (Just tree)
        "Added new.txt."
        "refs/heads/new-branch"

      (sha3, tree3) <- repack gitDir "objects/pack/p3.pack"
        [T "a" [T "b" [L "c.txt" $ Blob 7 "Super.\n"]]]
        (Just sha2) (Just tree2)
        "Added a/b/c.txt."
        "refs/heads/branch-3"

      _ <- repack gitDir "objects/pack/p4.pack"
        [L "new.txt" $ Blob 4 "bye\n"]
        (Just sha3) (Just tree3)
        "Changed hello to bye."
        "refs/heads/branch-4"

      let blob bs = Blob (L.length bs) bs
      _ <- repack gitDir "objects/pack/p5.pack"
        (groupBlobs
          [ ("README.md", blob "Pack 5\n")
          , ("bin/script.hs", blob "main = putStrLn \"Hello, world!\"\n")
          , ("tests/data/set-1/file-00.txt", blob "10\n")
          , ("tests/data/set-1/file-01.txt", blob "11\n")
          , ("tests/data/EMPTY", blob "")
          , ("tests/data/set-2/file-00.txt", blob "20\n")
          , ("tests/data/set-1/file-02.txt", blob "12\n")
          ])
        Nothing Nothing
        "Initial commit."
        "refs/heads/branch-5"

      return ()

    -- `buh fast-export` is meant to be used with `buh fast-import --files` i.e.
    -- the commit command is not issued.
    -- Example:
    --    buh fast-export .git branch/develop/latest/static/css | buh fast-import --files barerepo
    -- This will create a css directory at the root of the bare repo.
    ["fast-export", gitDir_, path] -> do
      gitDir <- canonicalizePath gitDir_
      checkRepository gitDir
      case splitDirectories path of
        "branch" : branch : commit : rest -> enterBranchCommit gitDir export branch commit rest
    ["fast-import", gitDir_] -> do
      ensureGitDir gitDir_
      gitDir <- canonicalizePath gitDir_
      gitFastImport gitDir False
    ["fast-import", "--files", gitDir_] -> do
      ensureGitDir gitDir_
      gitDir <- canonicalizePath gitDir_
      gitFastImport gitDir True

    xs -> error $ "TODO " ++ show xs

initRepository path = do
  (_, _, _, p) <- createProcess (proc "git"
    [ "init", "--bare", path
    ])
  _ <- waitForProcess p
  return ()

-- | TODO Make the check more comprehensive.
checkRepository path = do
  e <- doesDirectoryExist path
  e' <- doesDirectoryExist $ path </> "objects"
  e'' <- doesDirectoryExist $ path </> "refs"
  if e && e' && e''
    then return ()
    else error "Not a Git repository."

ensureGitDir gitDir = do
  e <- doesDirectoryExist gitDir
  if e
    then return ()
    else do
      createDirectoryIfMissing True gitDir
      initRepository gitDir

-- | If doCommit is True, the commit command is automatically sent before the
-- rest of the input stream.
gitFastImport gitDir doCommit = do
  is <- if doCommit
        then do
          -- If the ref already exists, then continue the commit from it.
          refs <- readHeads gitDir $ Just $ Ref "refs/heads/fast-import"
          commit <- if length refs == 1
                    then S.fromLazyByteString . runPut $ feCommit (Just "refs/heads/fast-import")
                    else S.fromLazyByteString . runPut $ feCommit Nothing
          S.appendInputStream commit S.stdin
        else return S.stdin
  (Just hin, _, _, p) <- createProcess (proc "git"
    [ "fast-import", "--date-format=now"
    ]) { env = Just [("GIT_DIR", gitDir)]
       , std_in = CreatePipe
       }
  sIn  <- S.handleToOutputStream hin >>=
    S.atEndOfOutput (hClose hin)
  S.connect is sIn
  _ <- waitForProcess p
  return ()

enterBranch :: FilePath -> String -> IO ()
enterBranch gitDir branch = do
  hds <- readHeads gitDir Nothing
  Sha sha <- lookupPath (BC.pack branch) hds
  cs <- readRevs gitDir (Ref sha)
  mapM_ (BC.putStrLn . fst) cs

enterBranchCommit :: FilePath -> (FilePath -> String -> Object -> IO a) -> String -> String -> [String] -> IO a
enterBranchCommit gitDir f branch commit path = do
  hds <- readHeads gitDir Nothing
  Sha sha <- lookupPath (BC.pack branch) hds
  cs <- readRevs gitDir (Ref sha)
  Ref sha' <- lookupPath (BC.pack commit) cs
  Commit (Just tree) _ _ <- readCommit gitDir $ Ref sha'
  enter gitDir f "/" path tree

ls :: FilePath -> String -> Object -> IO ()
ls _ p o = case o of
  Commit _ _ _ -> error "resolve to a commit"
  Tree es -> mapM_ (BC.putStrLn . fst) $ treeToRefs es
  Blob _ _ -> putStrLn p

cat :: FilePath -> String -> Object -> IO ()
cat _ _ o = case o of
  Commit _ _ _ -> error "resolve to a commit"
  Tree _ -> error "is a directory"
  Blob _ bs -> L.putStr bs

export :: FilePath -> String -> Object -> IO ()
export gitDir p o = export' gitDir p ps o
  where ps = if p == "/" then [] else [p]

export' :: FilePath -> String -> [String] -> Object -> IO ()
export' gitDir p ps o = case o of
  Commit _ _ _ -> error "resolve to a commit"
  Tree es -> do
    let refs = treeToRefs es
        f (p', ref) = do
          o <- readObject gitDir ref
          export' gitDir (BC.unpack p') (BC.unpack p':ps) o
    mapM_ f refs
  -- Blob _ bs -> putStrLn . ("Exporting " ++) . joinPath $ reverse ps
  Blob _ _ -> L.putStr . runPut $ fileModify (BC.pack . joinPath $ reverse ps, o)

enter :: FilePath -> (FilePath -> String -> Object -> IO a) -> String -> [String] -> Ref -> IO a
enter gitDir f p ps ref = do
  o <- readObject gitDir ref
  case ps of
    p':ps' -> case o of
      Blob _ _ -> error $ "no file '" ++ p' ++ "'"
      Tree es -> do
        ref' <- lookupPath (BC.pack p') $ treeToRefs es
        enter gitDir f p' ps' ref'
      Commit _ _ _ -> error "Deref the tree ?"
    [] -> f gitDir p o

lookupPath :: ByteString -> [(ByteString, a)] -> IO a
lookupPath k es = do
  case lookup k es of
    Just v -> return v
    _ -> error $ "no file '" ++ BC.unpack k ++ "'" -- TODO exitFailure

-- TODO Also make it possible to specify the mode of each entry.
rewrite gitDir packer xs mref = do
  o <- maybe (return $ Tree []) (readObject gitDir) mref
  case o of
    Blob _ _ -> error $ "file exists"
    Tree es -> do
      es' <- forM xs $ \x -> do
        case x of
          L name blob -> do
            sha <- pack packer blob
            return (normalFile, name, sha)
          T name ys -> do
            sha <- rewrite gitDir packer ys $ lookup name $ treeToRefs es
            return (subdirectory, name, sha)
      pack packer $ Tree . nubBy (\(_, a, _) (_, b, _) -> a == b) $ es' ++ es
    Commit _ _ _ -> error "expected tree is a commit"

data T p a = T p [T p a] | L p a
  deriving Show

input1 =
  [ ([], "a", 1)
  ]

input2 =
  [ ([], "a", 1)
  , ([], "b", 2)
  ]

input3 =
  [ ([], "a", 1)
  , ([], "b", 2)
  , (["e"], "c", 3)
  ]

input4 =
  [ ([], "a", 1)
  , ([], "b", 2)
  , (["e"], "c", 3)
  , (["f"], "d", 4)
  ]

input5 =
  [ ([], "a", 1)
  , ([], "b", 2)
  , (["e"], "c", 3)
  , (["f", "g"], "d", 4)
  , (["f"], "i", 6)
  , ([], "h", 5)
  ]

listBlobs = map listBlob . reverse . nubBy f . reverse
  where f (a, _) (b, _) = a == b

listBlob (path, blob) = case splitDirectories $ BC.unpack path of
  [] -> error "at least a filename must be given"
  xs -> (map BC.pack $ init xs, BC.pack $ last xs, blob)

-- | Group blobs into a tree.
groupBlobs = groupBlobs' . listBlobs

groupBlobs' blobs = map (\(_, a, b) -> L a b) direct ++ rest
  where
  rest = map (\(d, bs) -> T d $ groupBlobs' bs) $ map pops $ groupBy f indirect
  pops es@((x:_, _, _):_) = (x, map pop es)
  pops [] = error "can't happen" -- groupBy returns non-empty lists
  pop (_:xs, b, c) = (xs, b, c)
  pop _ = error "can't happen" -- pop is called only on the indirect elements
  (direct, indirect) = span isDirect $ sortBlobs blobs
  isDirect ([], _, _) = True
  isDirect _ = False
  f ([], _, _) ([], _, _) = True -- unused, f is called on the indirect elements
  f (x:_, _, _) (y:_, _, _) = x == y

-- | This is used to group blobs by path, only to arrange them in trees within
-- `groupBlobs`. The order is not the same as Git's Tree object. I.e. objects
-- will be written in the packfile in a slightly different order than they are
-- referenced in the Tree object.
sortBlobs :: Ord p => [([p], p, a)] -> [([p], p, a)]
sortBlobs = sortBy f
  where
  f (ps1, n1, _) (ps2, n2, _) = case compare ps1 ps2 of
    EQ -> compare n1 n2
    x -> x

-- | `repack _ fn` creates a new packfile stored at `fn` containg a tree of blobs
-- shadowing an optional tree. The resulting is referenced by commit, which
-- can receive an optional parent.
repack gitDir fn blobs msha mtree msg branch = do
  packer <- newPack fn
  tree' <- rewrite gitDir packer blobs mtree
  Ref sha' <- pack packer $ Commit (Just tree') msha msg
  completePack packer
  B.writeFile branch $ sha' `BC.append` "\n" -- TODO Use git-update-ref.
  return (Ref sha', tree')

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
readRefs :: FilePath -> Maybe Ref -> IO [(Ref, Sha)]
readRefs gitDir mref = do
  (code, out, _) <- readProcessWithExitCode' "git"
    ([ "show-ref", "--" ] ++ maybe [] s mref)
    [("GIT_DIR", gitDir)]
    ""
  if code == ExitSuccess
    then return . map (p . words) $ lines out
    -- git show-ref returns a exit code 1 when there is no ref.
    -- TODO Differentiate between no ref and other non-zero exit codes.
    else return []

  where s (Ref r) =[ BC.unpack r]
        p [sha, r] = (Ref $ BC.pack r, Sha $ BC.pack sha)
        p _ = error "unexpected git-show-ref output"

-- | Like readRefs, but return only those matching `refs/heads`.
readHeads :: FilePath -> Maybe Ref -> IO [(ByteString, Sha)]
readHeads gitDir mref = do
  refs <- readRefs gitDir mref
  return $ map unref $ filter (prefix . fst) refs
  where unref (Ref r, sha) = (BC.drop 11 r, sha)
        prefix (Ref r) = BC.isPrefixOf "refs/heads/" r

-- | `git cat-file --batch`
-- TODO Keep the process `git cat-file` around and query it instead of
-- respawning it again and again.
readObjects :: FilePath -> [Ref] -> IO [Object]
readObjects gitDir refs = do
  (Just pIn, Just pOut, _, p) <- createProcess (proc "git"
    [ "cat-file", "--batch"
    ])
    { std_in = CreatePipe
    , std_out = CreatePipe
    , env = Just [("GIT_DIR", gitDir)]
    }
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
          x -> error $ "unexpected git-cat-file output (3)" ++ show x
  os <- mapM (\r -> putRef r >> readOne) refs
  hClose pIn
  _ <- waitForProcess p
  return os

toStrict :: L.ByteString -> ByteString
toStrict = B.concat . L.toChunks

-- | Similar to `readObjects` with a single ref.
readObject :: FilePath -> Ref -> IO Object
readObject gitDir ref = do
  os <- readObjects gitDir [ref]
  case os of
    [o] -> return o
    _ -> error $ "can't happen"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a blob.
readBlob :: FilePath -> Ref -> IO Object
readBlob gitDir ref = do
  o <- readObject gitDir ref
  case o of
    Blob _ _ -> return o
    _ -> error $ "expected blob object"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a commit.
readCommit :: FilePath -> Ref -> IO Object
readCommit gitDir ref = do
  o <- readObject gitDir ref
  case o of
    Commit _ _ _ -> return o
    _ -> error $ "expected commit object"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a tree.
readTree :: FilePath -> Ref -> IO Object
readTree gitDir ref = do
  o <- readObject gitDir ref
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
revList :: FilePath -> Ref -> IO [(Ref, UTCTime)]
revList gitDir ref = do
  (code, out, _) <- readProcessWithExitCode' "git"
    [ "rev-list", "--pretty=format:%at", BC.unpack $ unRef ref ]
    [("GIT_DIR", gitDir)]
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
  p xs [] = reverse xs
  p _ _ = error "unexpected line from git-rev-list"

-- | Similar to `revList` but the result type matches `readHeads`.
readRevs :: FilePath -> Ref -> IO [(ByteString, Ref)]
readRevs gitDir ref = do
  refs <- revList gitDir ref
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

-- | Incrementally build a pack. It also builds the index. TODO Build the index
-- as the packfile is built, not afterwards.
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
        indexPack fn
    }

indexPack :: String -> IO ()
indexPack path = do
  (_, _, _, p) <- createProcess (proc "git"
    ["index-pack", "-v", path])
  _ <- waitForProcess p
  return ()

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

----------------------------------------------------------------------
-- serialization to `git fast-import` format
--
-- Example usage:
-- buh fast-export | git fast-import --date-format=now
----------------------------------------------------------------------

fastExport gitDir mfrom files = L.putStr $ toFastExport mfrom files

toFastExport mfrom files = runPut (feCommit mfrom >> feFiles files)

feCommit (mfrom) = do
  putByteString "commit refs/heads/fast-import\n"
  -- mark?
  -- author?
  putByteString "committer Vo Minh Thu <noteed@gmail.com> now\n" -- TODO git date format
  putByteString "data 0\n"
  maybe (return ()) (\ref -> putByteString $ B.concat ["from ", ref, "^0\n"]) mfrom

feFiles = mapM_ fileModify

fileModify (path, Blob n bs) = do
  putByteString "M "
  putByteString normalFile
  putByteString " inline "
  putByteString path
  putByteString "\ndata "
  putByteString . BC.pack $ show n
  putByteString "\n"
  putLazyByteString bs
  putByteString "\n"

-- | Same as System.Process.readProcessWithExitCode but allow to pass an
-- environment.
readProcessWithExitCode'
    :: FilePath                 -- ^ command to run
    -> [String]                 -- ^ any arguments
    -> [(String, String)]       -- ^ environment
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' cmd args env input = do
  (Just inh, Just outh, Just errh, pid) <-
    createProcess (proc cmd args)
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    , env = Just env
    }

  outMVar <- newEmptyMVar

  -- fork off a thread to start consuming stdout
  out  <- hGetContents outh
  forkIO $ C.evaluate (length out) >> putMVar outMVar ()

  -- fork off a thread to start consuming stderr
  err  <- hGetContents errh
  forkIO $ C.evaluate (length err) >> putMVar outMVar ()

  -- now write and flush any input
  when (not (null input)) $ do hPutStr inh input; hFlush inh
  hClose inh -- done with stdin

  -- wait on the output
  takeMVar outMVar
  takeMVar outMVar
  hClose outh

  -- wait on the process
  ex <- waitForProcess pid

  return (ex, out, err)
