{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TestTar where

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as LBS

import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip

import Data.List (isSuffixOf, unfoldr)
import qualified Data.Version as V

import Text.ParserCombinators.ReadP

import Pipes 
import qualified Pipes.Prelude as P
import qualified Pipes.Parse as P
import Control.Monad
import Data.Maybe
import Data.Typeable
import Control.Exception

import qualified Data.Map.Strict as Map

data TarException = TarFormatError Tar.FormatError
  deriving (Show, Typeable)

instance Exception TarException where

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
      loop (Tar.Fail e)       = throw (TarFormatError e)
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

tarEntriesProducer path = do
  entries <- lift $  tarEntriesForPath path
  let loop (Tar.Next ent nxt) = do yield ent; loop nxt
      loop (Tar.Done)         = return ()
      loop (Tar.Fail e)       = throw (TarFormatError e)
  loop entries

-- filter a stream of Tar entries selecting only the latest versions
latestVersions m = do
  ment <- P.draw
  case ment of
    Nothing -> return m
    Just ent -> do
      case parsePath (Tar.entryPath ent) of
        Nothing             -> latestVersions m
        Just (pkg, vers, _) -> let m' = Map.insertWith max pkg vers m
                               in latestVersions m'

doit f = forever $ do { x <- await; f x }

pipesListCabalFiles path = runEffect $ for (tarEntriesProducer path) (lift . print . Tar.entryPath)

pipesListLatestCabalFiles path = do
  latest <- P.evalStateT (latestVersions Map.empty) (tarEntriesProducer path)
  forM_ (Map.assocs latest) $ \(k,v) -> do
    putStrLn $ k ++ ": " ++ show v

newPackage Nothing _ = False
newPackage (Just ((oldPkg, _, _), _)) ((newPkg, _, _),_) = oldPkg /= newPkg

laterVersion Nothing _ = True
laterVersion old@(Just ((oldPkg, oldVers, _),_)) ((newPkg, newVers, _),_)
  | oldPkg /= newPkg  = False
  | oldVers < newVers = True
  | otherwise         = False

latestVersions2 = do
  let loop old = do
        mnew <- P.draw
        case mnew of
          Nothing -> return ()
          Just new  -> if newPackage old new
                          then do lift $ print $ fst (fromJust old)
                                  loop (Just new)
                          else if laterVersion old new
                                 then loop (Just new)
                                 else loop old
  loop Nothing

dropNothings = do
  x <- await
  case x of
    Nothing -> return ()
    Just y  -> yield y
  dropNothings

latestVersions3 = do
  let loop0 = do
        mnew <- P.draw
        case mnew of
          Nothing -> return ()
          Just old -> loop old
      newPackage' ((oldPkg,_,_),_) ((newPkg,_,_),_) = oldPkg /= newPkg
      laterVersion' ((oldPkg,oldVers,_),_) ((newPkg,newVers,_),_)
        = oldPkg == newPkg && oldVers < newVers
      showEnt ((pkg,vers,_),content) = 
        putStrLn $ pkg ++ " length: " ++ show (LBS.length content) ++ " version: " ++ show vers
      loop old = do
        mnew <- P.draw
        case mnew of
          Nothing -> lift $ showEnt old
          Just new -> if newPackage' old new
                        then do lift $ showEnt old
                                loop new
                        else if laterVersion' old new
                               then loop new
                               else loop old
  loop0

getContent ent =
  case Tar.entryContent ent of
    Tar.NormalFile bs len -> Just bs
    _                     -> Nothing

pipesParsePath = forever $ do
  ent <- await
  let mr = do content <- getContent ent
              x <- parsePath (Tar.entryPath ent)
              return (x,content)
  case mr of
    Nothing -> return ()
    Just r  -> yield r

showIt = do x <- await
            lift $ print (fst x)
            yield x
            showIt

devNull = do x <- await; devNull

pipesLatestVersions2 path = do
  P.evalStateT latestVersions2 (tarEntriesProducer path >-> pipesParsePath)
  return ()

pipesLatestVersions3 path = do
  P.evalStateT latestVersions3 (tarEntriesProducer path >-> pipesParsePath)
  return ()

-- test2 path = runEffect $ tarEntriesProducer path >-> pipesParsePath >-> showIt >-> devNull

