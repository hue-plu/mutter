{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

module Model.Users(users) where

import           Data.Morpheus.Types        (ComposedResolver, QUERY, Resolver,
                                             ResolverQ)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import qualified GraphQL                    as GQL
import           Opaleye                    (Field, FieldNullable, FromFields,
                                             Select, SqlBool, SqlDate,
                                             SqlFloat8, SqlInt4, SqlInt8,
                                             SqlText, Table, Unpackspec,
                                             aggregate, avg, count, groupBy,
                                             ifThenElse, isNull, leftJoin,
                                             matchNullable, restrict, runSelect,
                                             selectTable, showSql, sqlString,
                                             sum, table, tableField, viaLateral,
                                             (.&&), (.++), (.<), (.<=), (.==),
                                             (.===))
import           Prelude                    hiding (sum)

import           ModelFromDB                (runSelectWithConn)

data UserT a b
  = User
    { userId   :: a,
      userName :: b
    }
type User = UserT Int Text
type UserField = UserT (Field SqlInt4) (Field SqlText)

$(makeAdaptorAndInstance "pUser" ''UserT)

usersTable :: Table UserField UserField
usersTable = table "users" (pUser User { userId = tableField "id"
                                       , userName = tableField "name"
                                       })

usersSelect :: Select UserField
usersSelect = selectTable usersTable

userResolver :: User -> ResolverQ () GQL.Web GQL.User
userResolver User {userId, userName} =
  return GQL.User
    { GQL.userId = pure userId,
      GQL.userName = pure userName,
      GQL.userMurmurs = pure []
    }

-- TODO: find single user
-- https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs#L244

users :: ComposedResolver QUERY () GQL.Web [] GQL.User
users = do
  users :: [User] <- runSelectWithConn usersSelect
  traverse userResolver users
