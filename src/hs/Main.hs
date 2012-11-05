{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Config
import           Control.Monad
import           Control.Monad.Trans                  (MonadIO(liftIO))
import qualified Data.ByteString.Lazy.Char8    as     C
import           Data.Maybe
import qualified Models.Entry                  as     Entry
import           Network.Wai.Middleware.Static        (staticPolicy, noDots, (>->), addBase)
import           System.Directory                     (doesFileExist)
import           System.Environment                   (getArgs)
import qualified Templates                     as     T
import           Text.Blaze.Renderer.Text
import           Web.Scotty                    hiding (Options)

-- import           Network.HTTP.Types            (status404)

runServer :: Config -> IO ()
runServer config =
  scotty 3000 $ do

    get "/" $ 
      redirect "/blog/entries/"

    get "/blog/entries/" $ do
      entryNames          <- liftIO $ Entry.list (entriesSource config)
      renderableEntries   <- liftIO $ mapM (Entry.retrieve $ entriesSource config) entryNames
      let renderedEntries = catMaybes renderableEntries
      case renderedEntries of
        []   -> (html . renderHtml . T.page          (pageOptions config)) $ T.info "No entries"
        es   -> (html . renderHtml . T.renderEntries (pageOptions config)) $ take 6 $ reverse es
    
    --get "/blog/entries/:year/:month/:day/:slug/" $ \ year month day slug -> do
    --  let reference = Entry.Reference year month day slug
    --  maybeEntry <- liftIO $ Entry.retrieve (entriesSource config) reference 
    --  case maybeEntry of
    --    Just e  -> (html . renderHtml . T.renderEntry) e
    --    Nothing -> do 
    --      status status404
    --      html $ renderHtml (T.base "404 not found" "test")

    get "/humans.txt" $
      file "humans.txt"

    get "/robots.txt" $
      file "robots.txt"

    when (serveStatic config) $
      middleware $ staticPolicy $ noDots >-> addBase (staticSource config)

main :: IO ()
main =  do
  args         <- getArgs
  (opts, _)    <- getOpts args
  configExists <- doesFileExist $ configFile opts
  if configExists
    then do
          cfgFile   <- (readFile . configFile) opts
          let config = loadConfig $ C.pack cfgFile
          case config of
            Just c  -> runServer c
            Nothing -> putStrLn "Config incorrect"
    else ioError (userError "Config file doesn't exist (by default, derpy.cfg from the folder you run this from")
