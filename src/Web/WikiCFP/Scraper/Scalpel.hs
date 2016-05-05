-- |
-- Module: Web.WikiCFP.Scraper.Scalpel
-- Description: Scraper implementation with Scalpel
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Web.WikiCFP.Scraper.Scalpel
       ( ErrorMsg,
         Scraper',
         confRoot,
         searchRoot
       ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Data.Text (Text)
import Text.HTML.Scalpel (Scraper, (@:), (@=), chroot, text, texts, attr)

import Web.WikiCFP.Scraper.Type (Event)

type ErrorMsg = String

type Scraper' = Scraper Text

-- | Root scraper for conference Events.
confRoot :: Scraper' [Event]
confRoot = undefined

-- | Scrape shortName, URL and longName. Use with the root at @\<tr\>@ tag.
confRow1 :: Scraper' (Text, Text, Text)
confRow1 = do
  (sname, url) <- chroot ("td" @: ["rowspan" @= "2"]) $ ((,) <$> text "a" <*> attr "href" "a")
  lname <- text ("td" @: ["colspan" @= "3"])
  return (sname, url, lname)

-- | Scrape when, where, deadlines in Texts. Use the the root at @\<tr\>@ tag.
confRow2 :: Scraper' (Text, Text, Text)
confRow2 = do
  tds <- texts "td"
  guard $ length tds == 3
  return (tds !! 0, tds !! 1, tds !! 2)

-- | Root scraper for searched Events.
searchRoot :: Scraper' [Event]
searchRoot = undefined
