{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import           Data.Char
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusCode)
import           System.Directory
import           System.Environment

main :: IO ()
main = do
  user:tokenfp:_ <- getArgs
  token <- fmap (S8.takeWhile isAlphaNum) (S.readFile tokenfp)
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
               (S8.pack user, token)
               (\perpage page ->
                  "https://api.github.com/user/repos?per_page=" ++
                  show perpage ++ "&page=" ++ show page)
           L.writeFile cachefile (encode results)
           pure results
  putStrLn (take 512 (show results))
  where
    cachefile = "repos.json"

downloadPaginated :: (ByteString,ByteString) -> (Int -> Int -> String) -> IO [Value]
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