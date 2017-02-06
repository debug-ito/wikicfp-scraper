{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module: Web.WikiCFP.Scraper
-- Description: Scrape WikiCFP web site
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
-- Synopsis:
--
-- > import qualified Network.HTTP as H
-- > import Web.WikiCFP.Scraper (scrapeSearchEvents)
-- > 
-- > main :: IO ()
-- > main =  do
-- >   res <- H.getResponseBody =<< H.simpleHTTP (H.getRequest "http://wikicfp.com/cfp/servlet/tool.search?q=japan&year=t")
-- >   print $ scrapeSearchEvents res
--
-- This module scrapes WikiCFP pages (<http://wikicfp.com/>) for
-- call-for-papers. It helps you stay up to date with deadlines of
-- academic paper submissions.
module Web.WikiCFP.Scraper
       ( -- * Scraper routines
         scrapeConfEvents,
         scrapeSearchEvents,
         -- * Types
         ErrorMsg,
         HTML(..),
         When(..),
         Event(..)
       ) where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.Lazy as LT
import Text.HTML.Scalpel.Core (scrapeStringLike)

import Web.WikiCFP.Scraper.Type (When(..), Event(..))
import Web.WikiCFP.Scraper.Scalpel (ErrorMsg, Scraper', confRoot, searchRoot)


-- | Types of input HTML data to scrape.
class HTML a where
  decodeToText :: a -> Either ErrorMsg Text

instance HTML Text where
  decodeToText = Right

instance HTML LT.Text where
  decodeToText = Right . LT.toStrict

-- | It just assumes UTF-8 encoding.
instance HTML SB.ByteString where
  decodeToText = either (\e -> Left $ "UTF-8 decoding error: " ++ show e) Right . decodeUtf8'

instance HTML LB.ByteString where
  decodeToText = decodeToText . LB.toStrict

instance HTML String where
  decodeToText = Right . pack

runScraper :: Scraper' (Either ErrorMsg a) -> Text -> Either ErrorMsg a
runScraper s input = maybe (Left "Scraping error") id $ scrapeStringLike input s

-- | Scrape a page of a conference, for example,
-- <http://wikicfp.com/cfp/program?id=2671>
scrapeConfEvents :: HTML input => input -> Either ErrorMsg [Event]
scrapeConfEvents t = runScraper confRoot =<< decodeToText t

-- | Scrape a page of search results, for example,
-- <http://wikicfp.com/cfp/servlet/tool.search?q=cloud&year=t>
scrapeSearchEvents :: HTML input => input -> Either ErrorMsg [Event]
scrapeSearchEvents t = runScraper searchRoot =<< decodeToText t

