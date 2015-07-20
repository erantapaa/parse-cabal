module Main where

import qualified Lib as L
import System.Environment
import Data.Maybe
import Control.Monad
import qualified System.IO.Strict as S

import qualified PackageRank as PR
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf)
import qualified TarUtil as T
import Pipes
import qualified Pipes.Prelude as P

import Text.Show.Pretty
import qualified Data.ByteString.Lazy.UTF8 as LBS

safeHead a [] = a
safeHead _ (a:_) = a

computeRanks indexTarPath = do
  entries <- T.tarEntriesForPath indexTarPath
  cabals <- P.toListM $ T.pipesTarEntries entries >-> T.pipesSelectCabals >-> T.pipesLatestVersions
  -- create a map of PackageInfo records
  -- create the map of rankings
  let pe_to_info = L.parseCabal . LBS.toString . T.pe_content
      pkgInfoMap = Map.fromList [ (T.pe_package pe, pinfo) | pe <- cabals, Just pinfo <- [ pe_to_info pe ] ]
      nodes = [ (pkg, L.p_dependencies pinfo) | (pkg, pinfo) <- Map.assocs pkgInfoMap ]
      rankingMap = PR.rankingStd nodes
      pkgInfoMap' = Map.fromList [ (pkg, pinfo') | (pkg, pinfo) <- Map.assocs pkgInfoMap,
                                      let pinfo' = pinfo { L.p_rank = Map.findWithDefault 0 pkg rankingMap } ]
  forM_ (Map.assocs pkgInfoMap') $ \(pkg,pinfo) -> do
    putStrLn $ ppShow pinfo

main6 = do
  args <- getArgs
  case args of
    (path:_) -> do computeRanks path
    _        -> error "bad usage"

main = main6

