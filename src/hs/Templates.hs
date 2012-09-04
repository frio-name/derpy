{-# LANGUAGE OverloadedStrings #-}

module Templates where

import           Config                         (PageOptions(..))
import           Data.Monoid                    (mconcat)
import           Data.Text                      (Text)
import           Text.Blaze                     ((!), toValue)
import           System.FilePath                ((</>))
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

entryPartial :: H.Html -> H.Html
entryPartial = H.div ! A.class_ "entry"

info :: Text -> H.Html
info = (H.div ! A.class_ "info") . H.toHtml

-- Rename these to format
-- It should accept Renderable a -- like below
-- format :: Renderable a => a -> H.Html




renderEntry :: PageOptions -> H.Html -> H.Html
renderEntry opts entry =
  page opts $ entryPartial entry

renderEntries :: PageOptions -> [H.Html] -> H.Html
renderEntries opts entries = 
  page opts $ do
    mconcat $ map entryPartial entries

page :: PageOptions -> H.Html -> H.Html
page opts body  = 
  H.docTypeHtml ! A.lang "en" $ do
    H.head $ do
      H.title (H.toHtml titleOpt)
      H.meta ! A.name "author" ! A.content "frio"
      H.meta ! A.charset "utf-8"
      H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge,chrome=1"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Bitter&amp;ver=1"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (toValue $ staticRootOpt </> "css/style.css")
    H.body $ do
      H.div ! A.class_ "container" $ do
        H.div ! A.class_ "row" $ do
          H.div ! A.class_ "span12" $ do
            H.h1 ! A.class_ "brand" $ "frio.name"
            body
      H.script ! A.src (toValue $ staticRootOpt </> "js/libs/modernizr-2.5.3.min.js") $ ""
  where titleOpt      = title opts
        staticRootOpt = staticRoot opts
