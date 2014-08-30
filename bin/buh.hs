{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.Thyme.Clock (UTCTime)
import Data.Thyme.Format (formatTime)
import Data.Thyme.Time () -- For instance Num POSIXTime (a.k.a. NominalDiffTime)
import Data.Thyme.Time.Core (posixSecondsToUTCTime)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath (splitDirectories)
import System.IO (hClose, hFlush)
import System.Locale (defaultTimeLocale)
import System.Process
  ( createProcess, proc, readProcessWithExitCode, waitForProcess
  , CreateProcess(..), StdStream(..)
  )

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
    _ -> return ()

ls p o = case o of
  Commit _ -> error "resolve to a commit"
  Tree es -> mapM_ (BC.putStrLn . fst) $ treeToRefs es
  Blob _ _ _ -> putStrLn p

cat p o = case o of
  Commit _ -> error "resolve to a commit"
  Tree es -> error "is a directory"
  Blob _ _ bs -> BC.putStr bs

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

-- | Convert a Tree to the type returned by readHeads.
treeToRefs = map (\(_, b, c) -> (b, c))

newtype Sha = Sha ByteString
  deriving (Eq, Show)

newtype Ref = Ref { unRef ::ByteString } -- TODO Can it actually be non-ascii ?
  deriving Show

data Object =
    Blob Ref Int ByteString
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
          [sha, typ, size_] | Just (size, _) <- BC.readInt size_ -> do
            o <- B.hGet pOut size
            nl <- BC.hGetLine pOut
            when (nl /= "") $ error "unexpected git-cat-file output (1)"
            case typ of
              "blob" -> return $ Blob (Ref sha) size o
              "tree" -> do
                let loop xs s = do
                      if BC.null s
                        then return . Tree $ reverse xs
                        else do
                          -- Maybe rewrite this with attoparsec.
                          let (a, b) = BC.span (/= ' ') s
                              c = BC.drop 1 b
                              (d, d') = BC.span (/= '\0') c
                              e = BC.drop 1 d'
                              (f, g) = BC.splitAt 20 e
                          loop ((a,d, Ref $ B16.encode f): xs) g
                loop [] o
              "commit" -> return $ parseCommit o
              _ -> error "unexpected git-cat-file output (2)"
          _ -> error "unexpected git-cat-file output (3)"
  os <- mapM (\r -> putRef r >> readOne) refs
  hClose pIn
  _ <- waitForProcess p
  return os

-- | Similar to `readObjects` with a single ref.
readObject :: Ref -> IO Object
readObject ref = do
  os <- readObjects [ref]
  case os of
    [o] -> return o
    _ -> error $ "can't happen"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a commit.
readCommit :: Ref -> IO Object
readCommit ref = do
  o <- readObject ref
  case o of
    Commit tree -> return $ Commit tree
    _ -> error $ "expected commit object"

-- | Similar to `readObjects` (with a single ref), and error out if the result
-- is not a tree.
readTree :: Ref -> IO Object
readTree ref = do
  o <- readObject ref
  case o of
    Tree es -> return $ Tree es
    _ -> error $ "expected tree object"

parseCommit :: ByteString -> Object
parseCommit bs = case A.parseOnly p bs of
  Left err -> error err
  Right r -> r
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
