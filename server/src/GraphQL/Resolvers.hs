{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module GraphQL.Resolvers where

import           Control.Monad.Extra (findM)
import           Data.Morpheus.Types (RootResolver (..), Undefined (..))
import           GraphQL             (Murmur (Murmur, murmurText), Query (..),
                                      QueryMurmurArgs (QueryMurmurArgs, queryMurmurArgsText),
                                      QueryUserArgs (QueryUserArgs, queryUserArgsName),
                                      User (User, userName), Web)

import           Model               (murmurs, users)
import           ModelFromDB         (users)
import           Text.Regex.TDFA     ((=~))

rootResolver :: RootResolver Web () Query Undefined Undefined
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
      findM (\User {userName} -> (=~ queryUserArgsName) <$> userName) Model.users
    queryMurmur QueryMurmurArgs {queryMurmurArgsText} =
      findM (\Murmur {murmurText} -> (=~ queryMurmurArgsText) <$> murmurText) murmurs
    queryUsers = ModelFromDB.users
    queryMurmurs = pure murmurs

