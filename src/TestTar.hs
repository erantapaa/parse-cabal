
module TestTar where

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as LBS

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip

import Data.List (isSuffixOf, unfoldr)
import qualified Data.Version as V

import Text.ParserCombinators.ReadP

tarEntriesForPath path = do
  let decompress
        | isSuffixOf ".bz2" path = BZip.decompress
        | isSuffixOf ".gz" path  = GZip.decompress
        | isSuffixOf ".tgz" path = GZip.decompress
        | otherwise              = id 
  fmap (Tar.read . decompress) $ LBS.readFile path

listTar path = do
  entries <- tarEntriesForPath path
  let loop (Tar.Next ent next) = do putStrLn $ Tar.entryPath ent
                                    loop next
      loop Tar.Done            = return ()
      loop (Tar.Fail e)        = error $ "failed: " ++ show e
  loop entries

pathParts path = unfoldr go path
  where go [] = Nothing
        go p  = case span (/= '/') p of
                 ([], [])     -> Nothing
                 ([], rest)   -> go (tail rest)
                 (leaf, rest) -> Just (leaf, rest)



hasThreeParts path =
 case pathParts path of
   (pkg : version : cabalname : _) -> Just (pkg, version, cabalname)
   _                               -> Nothing

firstParse :: ReadP a -> String -> Maybe a
firstParse readp str =
  case (readP_to_S readp) str of
    []        -> Nothing
    ((v,_):_) -> Just v

parseVersion :: ReadP V.Version
parseVersion = do v <- V.parseVersion; eof; return v

parsePath :: String -> Maybe (String, V.Version, String)
parsePath path = do
  (pkg, vers, cabal) <- hasThreeParts path
  version <- firstParse parseVersion vers
  return $ (pkg, version, cabal)

-- | list cabal files in an index.tar.gz file
listCabalFiles path = do
  entries <- tarEntriesForPath path
  let loop (Tar.Next ent nxt) = do process ent; loop nxt
      loop (Tar.Done)         = return ()
      loop (Tar.Fail e)       = putStrLn "oops - an error"
      process ent = let epath = Tar.entryPath ent
                    in print $ parsePath epath
  loop entries

printLatestCabalFiles path = do
  entries <- tarEntriesForPath path
  let loop (Tar.Next ent nxt) x = do x' <- process ent x; loop nxt x'
      loop (Tar.Done)         x = return x
      loop (Tar.Fail e)       _ = do putStrLn "oops - an error"; return Nothing
      process ent x  = case parsePath (Tar.entryPath ent) of
                         Nothing             -> return x
                         Just (pkg, vers, _) -> do
                           case x of
                             Nothing            -> return $ Just (pkg, vers)
                             Just (pkg', vers') -> do
                               if pkg' == pkg
                                 then if vers > vers'
                                        then return $ Just (pkg, vers)
                                        else return x
                                 else do putStrLn $ show x
                                         return $ Just (pkg, vers)
  x <- loop entries Nothing
  case x of
    Just y -> putStrLn $ show y
    _      -> return ()

