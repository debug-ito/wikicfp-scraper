{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- |
-- Module: Web.WikiCFP.Scraper
-- Description: Scrape WikiCFP web site
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
-- 
module Web.WikiCFP.Scraper
       ( When(..),
         Event(..),
         ErrorMsg,
         HTML(..),
         scrapeConfEvents,
         scrapeSearchEvents
       ) where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.Lazy as LT

import Web.WikiCFP.Scraper.Type (When(..), Event(..))

type ErrorMsg = String

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


-- | Scrape a page of a conference, for example,
-- http://wikicfp.com/cfp/program?id=2671
scrapeConfEvents :: HTML input => input -> Either ErrorMsg [Event]
scrapeConfEvents = undefined

-- | Scrape a page of search results, for example,
-- http://wikicfp.com/cfp/servlet/tool.search?q=cloud&year=t
scrapeSearchEvents :: HTML input => input -> Either ErrorMsg [Event]
scrapeSearchEvents = undefined
