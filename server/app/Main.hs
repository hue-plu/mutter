{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

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

importGQLDocumentWithNamespace "src/schema.gql"

-- * Users

users :: Monad m => [User m]
users = [taro, jiro]

taro :: Monad m => User m
taro =
  User
    { userId = pure 2,
      userName = pure "tanaka taro",
      userMurmurs = pure []
    }

jiro :: Monad m => User m
jiro =
  User
    { userId = pure 3,
      userName = pure "tanaka jiro",
      userMurmurs = pure []
    }

-- * GraphQL Resolvers

-- * MurMurs
murmurs :: Monad m => [Murmur m]
murmurs =
  [ Murmur
      { murmurId = pure 1
      , murmurText = pure "There are murmurs of discontent everywhere."
      , murmurUser = pure taro
      }
  , Murmur
      { murmurId = pure 2
      , murmurText = pure "The brook murmurs over the pebbles."
      , murmurUser = pure taro
      }
  ]


rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          { queryMurmur,
            queryMurmurs,
            queryUser,
            queryUsers
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    queryUser QueryUserArgs {queryUserArgsName} =
      findM (\User {userName} -> (=~ queryUserArgsName) <$> userName) users
    queryUsers = pure users
    queryMurmur QueryMurmurArgs {queryMurmurArgsText} =
      findM (\Murmur {murmurText} -> (=~ queryMurmurArgsText) <$> murmurText) murmurs
    queryMurmurs = pure murmurs

api :: B.ByteString -> IO B.ByteString
api = interpreter rootResolver

main :: IO ()
main = scotty 8080 $ do
  post "/graphql" $ raw =<< (liftIO . api =<< body)
  get "/graphql" $ file "./static/graphql_playground.html"
