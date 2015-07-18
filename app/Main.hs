module Main where

import qualified Lib as L
import System.Environment
import Data.Maybe
import Control.Monad
import qualified System.IO.Strict as S

import qualified PackageRank as PR
import qualified Data.Map as M

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

main = main4

