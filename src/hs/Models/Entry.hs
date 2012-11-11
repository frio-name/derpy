{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Models.Entry        (Entry (..), list, retrieve) where

import           Data.List                   (sort)
import           Data.String.Utils           (endswith, split)
import           Data.Time                   (Day)
import           Data.Time.Calendar          (fromGregorian, showGregorian)
import qualified Framework.Model             as M
import           Framework.Template          (Renderable (..))
import           System.Directory            (doesFileExist, getDirectoryContents)
import           System.IO                   ()
import           Text.Pandoc
import           Text.Blaze                  ((!), toHtml)
import           Text.Blaze.Internal         (Html)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

data Markup = Markdown | LiterateHaskell | Unparseable
            deriving (Eq, Show)

data Entry = Entry { title :: String
                   , body  :: String
                   , publishedOn :: Day
                   , markup :: Markup }
                   deriving (Eq, Show)

instance M.Loadable FilePath where
  fname = id

instance Renderable Entry where
  render (Entry _ b _ Markdown)        = (writeHtml entryWriterOpts . readMarkdown defaultParserState) b
  render (Entry _ b _ LiterateHaskell) = (writeHtml entryWriterOpts . readMarkdown defaultParserState
                                                                                       { stateLiterateHaskell = True }) b
  render (Entry _ _ _ Unparseable)     = toHtml ("" :: String)

instance M.Dated Entry where
  publishedOn = publishedOn

renderEntry :: Entry -> H.Html
renderEntry entry = do
  H.div ! A.class_ "entry" $ do
    render entry
    H.span ! A.class_ "meta" $ (H.toHtml . showGregorian . publishedOn) entry
  
entryWriterOpts :: WriterOptions
entryWriterOpts = defaultWriterOptions { writerHtml5 = True
                                       , writerHighlight = True }

determineMarkup :: String -> Markup
determineMarkup fileName
                | endswith ".md"  fileName = Markdown
                | endswith ".lhs" fileName = LiterateHaskell
                | otherwise                = Unparseable

determineDate :: String -> Day
determineDate filename = fromGregorian year month day
                       where [y, m, d] = take 3 $ split "-" filename
                             year      = read y :: Integer
                             month     = read m :: Int
                             day       = read d :: Int
                
retrieve :: FilePath -> FilePath -> IO (Maybe Html)
retrieve source loadable = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      contents <- readFile filePath
      let entry = Entry { title       = "Unimplemented"
                        , body        = contents
                        , publishedOn = determineDate   loadable
                        , markup      = determineMarkup filePath
                        }
      return $ Just $ renderEntry entry
    else return Nothing
  where filePath = M.path source loadable

-- I don't like this -- I'd like it to be a Loadable a => FilePath -> IO [a]
-- but oh well, let's just get it up and running for now...
list :: FilePath -> IO [FilePath]
list p = getDirectoryContents p >>= (return . sort)