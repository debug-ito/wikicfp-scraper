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
import Control.Monad (guard, forM_)
import Data.Text (Text, pack)
import Data.Attoparsec.Text (Parser, parseOnly, skipSpace, string, endOfInput)
import Text.HTML.Scalpel (Scraper, (@:), (@=), chroot, text, texts, attr)

import Web.WikiCFP.Scraper.Type (Event)

type ErrorMsg = String

type Scraper' = Scraper Text

-- | Root scraper for conference Events.
confRoot :: Scraper' [Event]
confRoot = undefined

-- | Root scraper for searched Events.
searchRoot :: Scraper' [Event]
searchRoot = undefined


-- | Intermediate result of parsing under events \<table\>.
data EventRow = EventRowHeader
              | EventRow1 Text Text Text -- ^ shortName, URL and longName
              | EventRow2 Text Text Text -- ^ when, where, deadlines
              deriving (Eq,Ord,Show)
  

-- | Scrape header row. Use with the root at @\<tr\>@ tag.
eventRowHeader :: Scraper' EventRow
eventRowHeader = do
  tds <- texts "td"
  guard $ length tds == 4
  -- We cannot use OverloadedStrings with scalpel (as of 0.3.0.1),
  -- because it requires explicit (:: String) declarations everywhere!
  let expected_labels = map pack ["Event", "When", "Where", "Deadline"]
  forM_ [0..3] $ \i -> guard $ parsable (spacedText $ expected_labels !! i) $ tds !! i
  return EventRowHeader

-- | Scrape shortName, URL and longName. Use with the root at @\<tr\>@ tag.
eventRow1 :: Scraper' EventRow
eventRow1 = do
  (sname, url) <- chroot ("td" @: ["rowspan" @= "2"]) $ ((,) <$> text "a" <*> attr "href" "a")
  lname <- text ("td" @: ["colspan" @= "3"])
  return $ EventRow1 sname url lname

-- | Scrape when, where, deadlines in Texts. Use the the root at @\<tr\>@ tag.
eventRow2 :: Scraper' EventRow
eventRow2 = do
  tds <- texts "td"
  guard $ length tds == 3
  return $ EventRow2 (tds !! 0) (tds !! 1) (tds !! 2)


-- * Parsers

parsable :: Parser a -> Text -> Bool
parsable p t = either (const False) (const True) $ parseOnly p t

spacedText :: Text -> Parser Text
spacedText expected = skipSpace *> string expected <* skipSpace <* endOfInput

