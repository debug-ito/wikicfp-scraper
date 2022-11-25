module Web.WikiCFP.ScraperSpec
    ( main
    , spec
    ) where

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as BS
import           Data.Time           (Day, fromGregorian)
import           System.FilePath     (FilePath, joinPath)
import           Test.Hspec

import           Web.WikiCFP.Scraper (Event (..), When (..), scrapeConfEvents, scrapeSearchEvents)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_normal
  spec_marginal

spec_normal :: Spec
spec_normal = do
  describe "scrapeConfEvents" $ do
    forFile "conf_sigmod20160505.html" $ \raw_html -> do
      let ret = scrapeConfEvents raw_html
      fmap (take 5) ret `shouldBe`
        Right [ Event { eventShortName = "SIGMOD 2017",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=53445&copyownerid=70798",
                        eventLongName = "2017 ACM SIGMOD Conference",
                        eventWhen = Just $ newWhen (2017, 5, 14) (2017, 5, 19),
                        eventWhere = Just "Raleigh",
                        eventDeadlines = fmap newDay [(2016, 7, 15), (2016, 7, 22)]
                      },
                Event { eventShortName = "SIGMOD 2016",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=44695&copyownerid=76071",
                        eventLongName = "ACM International Conference on Management of Data ",
                        eventWhen = Just $ newWhen (2016, 6, 26) (2016, 7, 1),
                        eventWhere = Just "San Francisco, USA",
                        eventDeadlines = fmap newDay [(2015, 7, 9), (2015, 7, 16)]
                      },
                Event { eventShortName = "SIGMOD 2015",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=37140&copyownerid=43021",
                        eventLongName = "International Conference on Management of Data",
                        eventWhen = Just $ newWhen (2015, 5, 31) (2015, 6, 4),
                        eventWhere = Just "Melbourne, VIC, Australia",
                        eventDeadlines = fmap newDay [(2014, 10, 30), (2014, 11, 6)]
                      },
                Event { eventShortName = "SIGMOD  2014",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=32160&copyownerid=52931",
                        eventLongName = "ACM Conference on Management of Data",
                        eventWhen = Just $ newWhen (2014, 6, 22) (2014, 6, 27),
                        eventWhere = Just "Snowbird, Utah, USA",
                        eventDeadlines = fmap newDay [(2013, 9, 9), (2013, 9, 16)]
                      },
                Event { eventShortName = "SIGMOD 2013",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=24911&copyownerid=5823",
                        eventLongName = "ACM SIGMOD International Conference on Management of Data",
                        eventWhen = Just $ newWhen (2013, 6, 23) (2013, 6, 28),
                        eventWhere = Just "New York",
                        eventDeadlines = fmap newDay [(2012, 11, 13)]
                      }
              ]
      fmap length ret `shouldBe` Right 10

  describe "scrapeSearchEvents" $ do
    forFile "search_cloud20160505.html" $ \raw_html -> do
      let ret = scrapeSearchEvents raw_html
      fmap (take 5) ret `shouldBe`
        Right [ Event { eventShortName = "CloudCom 2016",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=52874&copyownerid=9221",
                        eventLongName = "The 8th IEEE International Conference on Cloud Computing Technology and Science",
                        eventWhen = Just $ newWhen (2016, 12, 12) (2016, 12, 15),
                        eventWhere = Just "Luxembourg City, Luxembourg",
                        eventDeadlines = fmap newDay [(2016, 6, 8), (2016, 6, 15)]
                      },
                Event { eventShortName = "UCC 2016",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=52465&copyownerid=49250",
                        eventLongName = "Utility and Cloud Computing",
                        eventWhen = Just $ newWhen (2016, 12, 6) (2016, 12, 9),
                        eventWhere = Just "Shanghai, China",
                        eventDeadlines = fmap newDay [(2016, 7, 3)]
                      },
                Event { eventShortName = "SI-Cloud-2 2016",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=50034&copyownerid=53027",
                        eventLongName = "International Journal of Services Technology and Management (EI) - Special Issue on Big Data Management in the Cloud",
                        eventWhen = Nothing,
                        eventWhere = Nothing,
                        eventDeadlines = fmap newDay [(2016, 8, 30), (2016, 9, 30)]
                      },
                Event { eventShortName = "IEEE SC 2016",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=52039&copyownerid=85546",
                        eventLongName = "The 6th IEEE International Symposium on Cloud and Service Computing",
                        eventWhen = Just $ newWhen (2016, 12, 8) (2016, 12, 10),
                        eventWhere = Just "Fiji",
                        eventDeadlines = fmap newDay [(2016, 8, 10)]
                      },
                Event { eventShortName = "ISDSA  2016",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=51832&copyownerid=35379",
                        eventLongName = "First International Symposium on Data Science and Applications",
                        eventWhen = Just $ newWhen (2016, 10, 10) (2016, 10, 11),
                        eventWhere = Just "Milan, Italy",
                        eventDeadlines = fmap newDay [(2016, 7, 25)]
                      }
              ]
      fmap length ret `shouldBe` Right 30


spec_marginal :: Spec
spec_marginal = do
  describe "scrapeConfEvents" $ do
    describe "no deadlines" $ do
      forFile "conf_mobisys20160620.html" $ \raw_html -> do
        let ret = scrapeConfEvents raw_html
        fmap (!! 8) ret `shouldBe`
          Right Event { eventShortName = "Mobisys 2008",
                        eventURL = "http://wikicfp.com/cfp/servlet/event.showcfp?eventid=1279&copyownerid=2",
                        eventLongName = "The 6th International Conference on Mobile Systems, Applications, and Services",
                        eventWhen = Just $ newWhen (2008, 6, 10) (2008, 6, 13),
                        eventWhere = Just $ "Breckenridge, CO, USA",
                        eventDeadlines = []
                      }

  describe "scrapeSearchEvents" $ do
    forFile "search_noresult20160516.html" $ \raw_html -> do
      scrapeSearchEvents raw_html `shouldBe` Right []




newDay :: (Integer, Int, Int) -> Day
newDay (y, m, d) = fromGregorian y m d

newWhen :: (Integer, Int, Int) -> (Integer, Int, Int) -> When
newWhen from to = When (newDay from) (newDay to)

testFile :: FilePath -> FilePath
testFile filename = joinPath ["test", "data", filename]

forFile :: FilePath -> (ByteString -> IO ()) -> SpecWith ()
forFile filename action = specify filename $ BS.readFile (testFile filename) >>= action
