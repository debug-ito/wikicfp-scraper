module Web.WikiCFP.ScraperSpec (main, spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Time (Day, fromGregorian)
import System.FilePath (FilePath, joinPath)
import Test.Hspec

import Web.WikiCFP.Scraper
  ( scrapeConfEvents,
    Event(..), When(..)
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "scrapeConfEvents" $ do
    forFile "conf_sigmod20160505.html" $ \raw_html -> do
      scrapeConfEvents raw_html `shouldBe`
        Right [ Event { eventShortName = "SIGMOD 2017",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=53445&amp;copyownerid=70798",
                        eventLongName = "2017 ACM SIGMOD Conference",
                        eventWhen = Just $ newWhen (2017, 5, 14) (2017, 5, 19),
                        eventWhere = Just "Raleigh",
                        eventDeadlines = fmap newDay [(2016, 7, 15), (2016, 7, 22)]
                      }
              ]

newDay :: (Integer, Int, Int) -> Day
newDay (y, m, d) = fromGregorian y m d

newWhen :: (Integer, Int, Int) -> (Integer, Int, Int) -> When
newWhen from to = When (newDay from) (newDay to)

testFile :: FilePath -> FilePath
testFile filename = joinPath ["test", "data", filename]

forFile :: FilePath -> (ByteString -> IO ()) -> SpecWith ()
forFile filename action = specify filename $ BS.readFile (testFile filename) >>= action
