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

import Control.Applicative ((<$>), (<*>), (<|>), (<*), (*>), optional)
import Control.Monad (guard, forM_, mzero)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Time (Day, fromGregorian)
import Data.Attoparsec.Text (Parser, parseOnly, skipSpace, string, endOfInput, decimal, takeText, char)
import Text.HTML.Scalpel (Scraper, (@:), (@=), (//), chroot, chroots, text, texts, attr, hasClass)

import Web.WikiCFP.Scraper.Type (Event(..), When(..))

type ErrorMsg = String

type Scraper' = Scraper Text

-- | Root scraper for conference Events.
confRoot :: Scraper' (Either ErrorMsg [Event])
confRoot = do
  ret_list <- chroots ("div" @: [hasClass "contsec"] // "table") $ eventsTable
  return $ concat <$> (sequence $ ret_list)

-- | Root scraper for searched Events.
searchRoot :: Scraper' (Either ErrorMsg [Event])
searchRoot = undefined

-- | Scrape events from a table. Use with the root at @\<table\>@ tag.
eventsTable :: Scraper' (Either ErrorMsg [Event])
eventsTable = do
  rows <- chroots "tr" (eventRow1 <|> eventRow2 <|> eventRowHeader)
  case rows of
    (EventRowHeader : rest) -> return $ rowsToEvents_noHeader rest
    _ -> mzero
    

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

rowsToEvents_noHeader :: [EventRow] -> Either ErrorMsg [Event]
rowsToEvents_noHeader [] = return []
rowsToEvents_noHeader ((EventRow1 sn url ln) : (EventRow2 when wher dl) : rest) =
  (:) <$> createEvent sn url ln when wher dl <*> rowsToEvents_noHeader rest
rowsToEvents_noHeader rows = Left ("Error while parsing rows: " ++ show rows)

-- | TODO: make it configurable.
urlBase :: Text
urlBase = pack "http://wikicfp.com"

createEvent :: Text -> Text -> Text -> Text -> Text -> Text -> Either ErrorMsg Event
createEvent sname url lname when wher dl = do
  when' <- parseWhen when
  wher' <- parseWhere wher
  dl' <- parseDeadlines dl
  return Event { eventShortName = sname,
                 eventURL = urlBase <> url,
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

spacedText' :: String -> Parser Text
spacedText' = spacedText . pack

string' :: String -> Parser Text
string' = string . pack

parseWhen :: Text -> Either ErrorMsg (Maybe When)
parseWhen = parseOnly (parserWhen <* endOfInput) where
  parserWhen = (spacedText' "N/A" *> pure Nothing)
               <|> (Just <$> parserJustWhen)
  parserJustWhen = When
                   <$> (parserDay <* skipSpace <* (char '-') <* skipSpace)
                   <*> (parserDay <* skipSpace)

parseWhere :: Text -> Either ErrorMsg (Maybe Text)
parseWhere = parseOnly (parserWhere <* endOfInput) where
  parserWhere = (spacedText' "N/A" *> pure Nothing) <|> (Just <$> takeText)

parseDeadlines :: Text -> Either ErrorMsg [Day]
parseDeadlines input = sort <$> parseOnly (parserDeadlines <* endOfInput) input where
  parserDeadlines = do
    primary <- parserDay <* skipSpace
    msecondary <- optional $ (char '(' *> skipSpace *> parserDay <* skipSpace <* char ')')
    return $ maybe [primary] (: [primary]) msecondary

parserDay :: Parser Day
parserDay = impl where
  impl = do
    m <- parserMonth <* skipSpace
    d <- decimal <* (optional $ char ',') <* skipSpace
    y <- decimal
    return $ fromGregorian y m d
  parserMonth =     (string' "Jan" *> pure 1)
                <|> (string' "Feb" *> pure 2)
                <|> (string' "Mar" *> pure 3)
                <|> (string' "Apr" *> pure 4)
                <|> (string' "May" *> pure 5)
                <|> (string' "Jun" *> pure 6)
                <|> (string' "Jul" *> pure 7)
                <|> (string' "Aug" *> pure 8)
                <|> (string' "Sep" *> pure 9)
                <|> (string' "Oct" *> pure 10)
                <|> (string' "Nov" *> pure 11)
                <|> (string' "Dec" *> pure 12)
  
  
