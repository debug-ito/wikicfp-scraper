-- |
-- Module: Web.WikiCFP.Scraper
-- Description: Scrape WikiCFP web site
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module Web.WikiCFP.Scraper
       ( Event(..),
         HTML,
         scrapeConfEvents,
         scrapeSearchEvents
       ) where

import Data.Text (Text)

import Web.WikiCFP.Scraper.Type (Event(..))


-- | The input HTML data to scrape.
type HTML = Text

-- | Scrape a page of a conference, for example,
-- http://wikicfp.com/cfp/program?id=2671
scrapeConfEvents :: HTML -> Either String [Event]
scrapeConfEvents = undefined

-- | Scrape a page of search results, for example,
-- http://wikicfp.com/cfp/servlet/tool.search?q=cloud&year=t
scrapeSearchEvents :: HTML -> Either String [Event]
scrapeSearchEvents = undefined
