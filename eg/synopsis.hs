
import qualified Network.HTTP as H
import Web.WikiCFP.Scraper (scrapeSearchEvents)

main :: IO ()
main =  do
  res <- H.getResponseBody =<< H.simpleHTTP (H.getRequest "http://wikicfp.com/cfp/servlet/tool.search?q=japan&year=t")
  print $ scrapeSearchEvents res
