module Framework.Model where

import Data.Time       (Day)
import System.FilePath ((</>))

class Loadable a where
  fname :: a -> FilePath
  path  :: FilePath -> a -> FilePath
  path source f = source </> fname f

class Dated a where
  publishedOn :: a -> Day