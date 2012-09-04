{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import           System.Console.GetOpt

-- Config file stuff
data Config = Config { entriesSource :: String
                     , staticSource  :: String
                     , serveStatic   :: Bool
                     , pageOptions   :: PageOptions }
                     deriving (Show)

data PageOptions = PageOptions { title      :: String 
                               , staticRoot :: String }
                               deriving (Show)

instance FromJSON Config where
         parseJSON (Object v) = Config <$>
                   v .: "entriesSource" <*>
                   v .: "staticSource" <*>
                   v .: "serveStatic" <*>
                   v .: "pageOptions"
         parseJSON _ = mzero

instance FromJSON PageOptions where
         parseJSON (Object v) = PageOptions <$>
                   v .: "title" <*>
                   v .: "staticRoot"
         parseJSON _ = mzero

loadConfig :: C.ByteString -> Maybe Config
loadConfig = decode

-- Commandline args
data Options = Options { configFile :: String } deriving Show

defaultOptions :: Options
defaultOptions = Options { configFile = "derpy.cfg" }

options :: [OptDescr (Options -> Options)]
options = [ Option ['c'] ["config"] 
                   (ReqArg (\ f opts -> opts { configFile = f }) "FILE") 
                   "config FILE" ]

getOpts :: [String] -> IO (Options, [String])
getOpts argv =
  case getOpt Permute options argv of 
    (o,n,[])   -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo intro options))
    where intro = "Usage: derpy [OPTION...]"