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


-- | TODO: should this be ByteString??
type HTML = Text

scrapeConfEvents :: HTML -> Either String [Event]
scrapeConfEvents = undefined

scrapeSearchEvents :: HTML -> Either String [Event]
scrapeSearchEvents = undefined
