{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Extra        (findM)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types        (RootResolver (..), Undefined (..))
import           Data.Text                  (Text)
import           Text.Regex.TDFA            ((=~))
import           Web.Scotty

import           GraphQL.Resolvers

api :: B.ByteString -> IO B.ByteString
api = interpreter rootResolver

main :: IO ()
main = scotty 8080 $ do
  post "/graphql" $ raw =<< (liftIO . api =<< body)
  get "/graphql" $ file "./static/graphql_playground.html"
