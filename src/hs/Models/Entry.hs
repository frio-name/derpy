{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Models.Entry        (Entry (..), list, retrieve) where

import Data.List           (intercalate)
import System.Directory    (doesFileExist, getDirectoryContents)
import System.FilePath     ((</>))
import System.IO           ()
import Text.Blaze.Internal (Html)
import Text.Pandoc

class Loadable a where
  fname :: a -> FilePath
  path  :: FilePath -> a -> FilePath
  path source f = source </> fname f

data Entry = Entry { year     :: String
                   , month    :: String
                   , day      :: String
                   , slug     :: String }

render :: String -> Html
render = 
    writeHtml entryWriterOpts . readMarkdown defaultParserState
    where entryWriterOpts = defaultWriterOptions { writerHtml5 = True
                                                 , writerHighlight = True
                                                 }

instance Loadable FilePath where
  fname = id

instance Loadable Entry where
  fname (Entry y m d s) = intercalate "-" [y, m, d, s] ++ ext

ext :: String
ext = ".md"

retrieve :: Loadable a => FilePath -> a -> IO (Maybe Html)
retrieve source loadable = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      entry <- readFile filePath
      return $ Just $ render entry
    else return Nothing
  where filePath = path source loadable

-- I don't like this -- I'd like it to be a Loadable a => FilePath -> IO [a]
-- but oh well, let's just get it up and running for now...
list :: FilePath -> IO [FilePath]
list = getDirectoryContents
