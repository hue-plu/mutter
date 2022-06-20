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
                                      User (User, userName), Web)

import           Model               (murmurs, users)
import           Text.Regex.TDFA     ((=~))

rootResolver :: RootResolver Web () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver =
        Query
          {
            queryMurmurs,
            queryUsers
            -- queryMurmur,
            -- queryUser,
          },
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }
  where
    -- queryUser QueryUserArgs {queryUserArgsName} =
    --   findM (\User {userName} -> (=~ queryUserArgsName) <$> userName) users
    -- queryMurmur QueryMurmurArgs {queryMurmurArgsText} =
    --   findM (\Murmur {murmurText} -> (=~ queryMurmurArgsText) <$> murmurText) murmurs
    queryUsers = users
    queryMurmurs = murmurs

