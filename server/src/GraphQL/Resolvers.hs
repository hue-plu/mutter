{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module GraphQL.Resolvers where

import           Control.Monad.Extra        (findM)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Morpheus.Types        (RootResolver (..), Undefined (..))
import           Data.Text                  (Text)
import           Text.Regex.TDFA            ((=~))
import           Web.Scotty

import           GraphQL
import           Model

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
