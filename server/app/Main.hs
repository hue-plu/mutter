module Main where

import           Control.Monad.Extra            (findM)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Reader           (MonadReader, ReaderT,
                                                 runReaderT)

import qualified Data.ByteString.Lazy.Char8     as B
import           Data.Morpheus                  (interpreter)
import           Data.Morpheus.Document         (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types            (RootResolver (..),
                                                 Undefined (..))
import           Data.Text                      (Text)
import           Text.Regex.TDFA                ((=~))
import           Web.Scotty

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.URL

import           Data.Maybe                     (fromJust)
import           GraphQL.Resolvers

import           GraphQL                        (Web, runWeb)

-- FIXME: from ENV
connectInfo :: ConnectInfo
-- FIXME: handling maybe
connectInfo = fromJust $ parseDatabaseUrl "postgres://postgres:@0.0.0.0:65432/mutter"

api :: B.ByteString -> Web B.ByteString
api = interpreter rootResolver

webServer :: Connection -> IO()
webServer conn =  scotty 8080 $ do
  post "/graphql" $ do
    reqBody <- body
    raw =<< liftIO (runWeb (api reqBody) conn)
  get "/graphql" $ file "./static/graphql_playground.html"

main :: IO ()
main = do
  conn <- connect connectInfo
  webServer conn
