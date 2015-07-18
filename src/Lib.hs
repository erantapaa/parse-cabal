module Lib
where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version

import Text.Show.Pretty
import Data.List (intercalate)

type Score = Float

data PackageInfo
    = PackageInfo
      { p_name         :: String               -- ^ The name of the package
      , p_version      :: String               -- ^ The latest package version
      , p_dependencies :: String               -- ^ The list of required packages
      , p_author       :: String               -- ^ The author
      , p_maintainer   :: String               -- ^ The maintainer
      , p_category     :: String               -- ^ The package category
      , p_homepage     :: String               -- ^ The home page
      , p_synopsis     :: String               -- ^ The synopsis
      , p_description  :: String               -- ^ The description of the package
      , p_uploaddate   :: String               -- ^ The upload date
      , p_rank         :: ! Score              -- ^ The ranking
      }
    deriving (Show, Eq)

parseCabal cbl =
  case parsePackageDescription cbl of
    ParseFailed _ -> Nothing
    ParseOk _ d   -> let pinfo = PackageInfo
                                 { p_name         = extractPackageName d
                                 , p_version      = intercalate "." $ map show (extractPackageVersion d)
                                 , p_dependencies = intercalate "," (extractDeps d)
                                 , p_author       = extractAuthor d
                                 , p_maintainer   = extractMaintainer d
                                 , p_category     = extractCategory d
                                 , p_homepage     = extractHomePage d
                                 , p_synopsis     = extractSynopsis d
                                 , p_description  = extractDescription d
                                 , p_uploaddate   = ""
                                 , p_rank         = 0
                                 }
                     in Just pinfo

extractPackageName = go . pkgName . package . packageDescription
  where go (PackageName x) = x

extractPackageVersion = versionBranch . pkgVersion . package . packageDescription

extractAuthor = author . packageDescription

extractMaintainer = maintainer . packageDescription

extractCategory = category . packageDescription

extractHomePage = homepage . packageDescription

extractSynopsis = synopsis . packageDescription

extractDescription = description . packageDescription

getPackageName (Dependency (PackageName pkgname) _) = pkgname

extractDeps :: GenericPackageDescription -> [String]
extractDeps d =  map getPackageName (ldeps ++ edeps)
  where ldeps = case (condLibrary d) of
                  Nothing -> []
                  Just c  -> condTreeConstraints c
        edeps = concat $ map (condTreeConstraints . snd) $ condExecutables d

main :: IO ()
main = do cbl <- getContents
          putStrLn $ ppShow $ parseCabal cbl

