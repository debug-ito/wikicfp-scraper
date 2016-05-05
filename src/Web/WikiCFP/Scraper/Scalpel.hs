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

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad (guard, forM_)
import Data.Text (Text, pack)
import Data.Time (Day)
import Data.Attoparsec.Text (Parser, parseOnly, skipSpace, string, endOfInput)
import Text.HTML.Scalpel (Scraper, (@:), (@=), chroot, chroots, text, texts, attr)

import Web.WikiCFP.Scraper.Type (Event(..), When(..))

type ErrorMsg = String

type Scraper' = Scraper Text

-- | Root scraper for conference Events.
confRoot :: Scraper' (Either ErrorMsg [Event])
confRoot = undefined

-- | Root scraper for searched Events.
searchRoot :: Scraper' (Either ErrorMsg [Event])
searchRoot = undefined

-- | Scrape events from a table. Use with the root at @\<table\>@ tag.
eventsTable :: Scraper' (Either ErrorMsg [Event])
eventsTable = rowsToEvents <$> chroots "tr" (eventRow1 <|> eventRow2 <|> eventRowHeader)

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

rowsToEvents :: [EventRow] -> Either ErrorMsg [Event]
rowsToEvents [] = return []
rowsToEvents (r:rest) = case r of
  EventRowHeader -> rowsToEvents_noHeader rest
  _ -> Left ("The first row must be the header, but it was " ++ show r)

rowsToEvents_noHeader :: [EventRow] -> Either ErrorMsg [Event]
rowsToEvents_noHeader [] = return []
rowsToEvents_noHeader ((EventRow1 sn url ln) : (EventRow2 when wher dl) : rest) =
  (:) <$> createEvent sn url ln when wher dl <*> rowsToEvents_noHeader rest
rowsToEvents_noHeader rows = Left ("Error while parsing rows: " ++ show rows)

createEvent :: Text -> Text -> Text -> Text -> Text -> Text -> Either ErrorMsg Event
createEvent sname url lname when wher dl = do
  when' <- parseWhen when
  wher' <- parseWhere wher
  dl' <- parseDeadlines dl
  return Event { eventShortName = sname,
                 eventURL = url,
                 eventLongName = lname,
                 eventWhen = when',
                 eventWhere = wher',
                 eventDeadlines = dl'
               }

-- * Parsers

parsable :: Parser a -> Text -> Bool
parsable p t = either (const False) (const True) $ parseOnly p t

spacedText :: Text -> Parser Text
spacedText expected = skipSpace *> string expected <* skipSpace <* endOfInput

parseWhen :: Text -> Either ErrorMsg (Maybe When)
parseWhen = undefined

parseWhere :: Text -> Either ErrorMsg (Maybe Text)
parseWhere = undefined

parseDeadlines :: Text -> Either ErrorMsg [Day]
parseDeadlines = undefined
