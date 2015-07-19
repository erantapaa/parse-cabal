module Main where

import qualified Lib as L
import System.Environment
import Data.Maybe
import Control.Monad
import qualified System.IO.Strict as S

import qualified PackageRank as PR
import qualified Data.Map as M
import Data.List (isInfixOf)

main2 :: IO ()
main2 = L.main

-- read and parse all the cabal files
main3 :: IO ()
main3 = do
  paths <- fmap lines getContents
  forM_ paths $ \path -> do
    cbl <- readFile path
    let pair = fmap (\d -> (L.p_name d, L.p_dependencies d)) (L.parseCabal cbl)  
    case pair of
      Nothing -> putStrLn $ path ++ ": unable to parse"
      Just x  -> print x

main4 = do
  paths <- fmap lines S.getContents
  mnodes <- forM paths $ \path -> do
              cbl <- S.readFile path
              let pair = fmap (\d -> (L.p_name d, L.p_dependencies d)) (L.parseCabal cbl)  
              return pair
  let nodes = catMaybes mnodes
      ranking = PR.rankingStd nodes 
  forM_ (M.assocs ranking) $ \(k,v) -> do
    putStrLn $ k ++ " " ++ show v

safeHead a [] = a
safeHead _ (a:_) = a

main5 = do
  args <- getArgs
  let cmd = safeHead "deps" args
  case cmd of
    "deps"  -> showDeps
    "ranks" -> showRanks
    "condtar" -> findConduitTar
    _       -> error $ "unknown command: " ++ cmd

showDeps = do
  paths <- fmap lines S.getContents
  forM_ paths $ \path -> do
    cbl <- S.readFile path
    let pair = fmap (\d -> (L.p_name d, L.p_dependencies d)) (L.parseCabal cbl)  
    case pair of
      Nothing           -> putStrLn $ path ++ ": unable to parse cabal file"
      Just (name, deps) -> emitPackage name deps

showRanks = do
  paths <- fmap lines S.getContents
  mnodes <- forM paths $ \path -> do
              cbl <- S.readFile path
              let pair = fmap (\d -> (L.p_name d, L.p_dependencies d)) (L.parseCabal cbl)  
              return pair
  let nodes = catMaybes mnodes
      ranking = PR.rankingStd nodes 
  forM_ (M.assocs ranking) $ \(k,v) -> do
    putStrLn $ k ++ " " ++ show v

findConduitTar = do
  paths <- fmap lines S.getContents
  forM_ paths $ \path -> do
    cbl <- S.readFile path
    let pair = fmap (\d -> (L.p_name d, L.p_dependencies d)) (L.parseCabal cbl)  
    case pair of
      Nothing           -> putStrLn $ path ++ ": unable to parse cabal file"
      Just (name, deps) -> when (hasConduitAndTar deps) $ do
                             emitPackage name deps; putStrLn ""
                             
hasConduitAndTar deps =
  any ("conduit" `isInfixOf` ) deps && any ("tar" `isInfixOf`) deps

breakLine maxLength xs@[] = (xs,xs)
breakLine maxLength xs@(x:xs')
  | len <= maxLength = let (ys,zs) = breakLine (maxLength-1-len) xs'
                       in (x:ys,zs)
  | otherwise        = ([],xs)
  where len = length x

breakLines maxLength ws =
  case breakLine maxLength ws of
    ([], [])     -> []
    ([], (x:xs)) -> [x] : breakLines maxLength xs
    (xs, ys)     -> xs : breakLines maxLength ys

emitPackage name deps = do
  putStrLn $ name ++ ":"
  forM_ (breakLines 80 deps) $ \ws -> putStrLn $ "  " ++ unwords ws

main = main5

