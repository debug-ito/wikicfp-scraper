-- |
-- Module: Web.WikiCFP.Scraper.Type
-- Description: data types for WikiCFP scraper
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Web.WikiCFP.Scraper.Type
       ( Event(..),
       ) where

import Data.Text (Text)
import Data.Time (Day)

-- | A conference event posted to WikiCFP site. It corresponds to a
-- row in the table you see conference pages etc, for example,
-- http://wikicfp.com/cfp/program?id=1172
data Event = Event { eventShortName :: !Text,
                     
                     eventURL :: !Text,
                     -- ^ URL to the WikiCFP page of this event.
                     
                     eventLongName :: !Text, 
                     eventWhenFrom :: !Day,
                     eventWhenTo :: !Day,
                     eventWhere :: !Text,
                     
                     eventDeadlines :: ![Day]
                     -- ^ deadlines are in an ascending order, i.e.,
                     -- the earliest deadline is the head.
                     
                   } deriving (Eq,Ord,Show)
