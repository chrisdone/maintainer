{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.IO as T
import           Data.Yaml (decodeFileThrow)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import           System.Directory

data Config = Config
  { configUsername :: ByteString
  , configToken :: ByteString
  }

instance FromJSON Config where
  parseJSON j = do
    o <- parseJSON j
    Config <$> fmap S8.pack (o .: "username") <*> fmap S8.pack (o .: "token")

main :: IO ()
main = do
  config <- decodeFileThrow "maintainer.yaml"
  results <-
    do exists <- doesFileExist cachefile
       if exists
         then do
           bytes <- L.readFile cachefile
           case decode bytes of
             Just as -> pure as
             Nothing -> error "Couldn't read repos.json. Delete it?"
         else do
           results <-
             downloadPaginated
               (configUsername config, configToken config)
               (\perpage page ->
                  "https://api.github.com/user/repos?per_page=" ++
                  show perpage ++ "&page=" ++ show page)
           L.writeFile cachefile (encode results)
           pure results
  mapM_ (maybe (pure ()) print . HM.lookup "full_name") results
  where
    cachefile = "maintainer.json"

downloadPaginated :: (ByteString,ByteString) -> (Int -> Int -> String) -> IO [Object]
downloadPaginated auth makeUrl = do
  manager <- newManager tlsManagerSettings
  go manager 1 []
  where
    go manager page acc = do
      request <-
        fmap
          (uncurry applyBasicAuth auth)
          (parseRequest (makeUrl perpage page))
      response <-
        httpLbs
          request
            { requestHeaders =
                requestHeaders request <>
                [("User-Agent", "maintainer")]
            }
          manager
      case statusCode (responseStatus response) of
        200 -> do
          case eitherDecode (responseBody response) >>= parseEither parseJSON of
            Left e -> error ("Couldn't decode JSON: " ++ e)
            Right ns -> do
              do print (length ns)
                 if length ns == perpage
                   then go manager (page + 1) (ns : acc)
                   else pure (concat (ns : acc))
        _ -> error ("Bad status code: " ++ show response)
    perpage = 100
