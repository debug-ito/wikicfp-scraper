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
      let ret = scrapeConfEvents raw_html
      fmap (take 5) ret `shouldBe`
        Right [ Event { eventShortName = "SIGMOD 2017",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=53445&amp;copyownerid=70798",
                        eventLongName = "2017 ACM SIGMOD Conference",
                        eventWhen = Just $ newWhen (2017, 5, 14) (2017, 5, 19),
                        eventWhere = Just "Raleigh",
                        eventDeadlines = fmap newDay [(2016, 7, 15), (2016, 7, 22)]
                      },
                Event { eventShortName = "SIGMOD 2016",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=44695&amp;copyownerid=76071",
                        eventLongName = "ACM International Conference on Management of Data ",
                        eventWhen = Just $ newWhen (2016, 6, 26) (2016, 7, 1),
                        eventWhere = Just "San Francisco, USA",
                        eventDeadlines = fmap newDay [(2015, 7, 9), (2015, 7, 16)]
                      },
                Event { eventShortName = "SIGMOD 2015",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=37140&amp;copyownerid=43021",
                        eventLongName = "International Conference on Management of Data",
                        eventWhen = Just $ newWhen (2015, 5, 31) (2015, 6, 4),
                        eventWhere = Just "Melbourne, VIC, Australia",
                        eventDeadlines = fmap newDay [(2014, 10, 30), (2014, 11, 6)]
                      },
                Event { eventShortName = "SIGMOD  2014",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=32160&amp;copyownerid=52931",
                        eventLongName = "ACM Conference on Management of Data",
                        eventWhen = Just $ newWhen (2014, 6, 22) (2014, 6, 27),
                        eventWhere = Just "Snowbird, Utah, USA",
                        eventDeadlines = fmap newDay [(2013, 9, 9), (2013, 9, 16)]
                      },
                Event { eventShortName = "SIGMOD 2013",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=24911&amp;copyownerid=5823",
                        eventLongName = "ACM SIGMOD International Conference on Management of Data",
                        eventWhen = Just $ newWhen (2013, 6, 13) (2013, 6, 28),
                        eventWhere = Just "New York",
                        eventDeadlines = fmap newDay [(2012, 11, 13)]
                      }
              ]
      fmap length ret `shouldBe` Right 10


newDay :: (Integer, Int, Int) -> Day
newDay (y, m, d) = fromGregorian y m d

newWhen :: (Integer, Int, Int) -> (Integer, Int, Int) -> When
newWhen from to = When (newDay from) (newDay to)

testFile :: FilePath -> FilePath
testFile filename = joinPath ["test", "data", filename]

forFile :: FilePath -> (ByteString -> IO ()) -> SpecWith ()
forFile filename action = specify filename $ BS.readFile (testFile filename) >>= action
