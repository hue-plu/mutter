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


module ModelFromDB where

import           Prelude                         hiding (sum)

import           Opaleye                         (Field, FieldNullable,
                                                  FromFields, Select, SqlBool,
                                                  SqlDate, SqlFloat8, SqlInt4,
                                                  SqlInt8, SqlText, Table,
                                                  Unpackspec, aggregate, avg,
                                                  count, groupBy, ifThenElse,
                                                  isNull, leftJoin,
                                                  matchNullable, restrict,
                                                  runSelect, selectTable,
                                                  showSql, sqlString, sum,
                                                  table, tableField, viaLateral,
                                                  (.&&), (.++), (.<), (.<=),
                                                  (.==), (.===))

import           Control.Monad.Trans             (lift, liftIO)
import           Data.Profunctor.Product         (p1, p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH      (makeAdaptorAndInstance)
import           Data.Text                       (Text)
import           Data.Time.Calendar              (Day)

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.URL

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Reader            (ask)

import           Data.Morpheus.Types             (ComposedResolver, QUERY,
                                                  Resolver, ResolverQ)


import           Data.Maybe
import qualified GraphQL                         as GQL

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

users :: ComposedResolver QUERY () GQL.Web [] GQL.User
users = do
  users :: [User] <- runSelectWithConn usersSelect
  traverse userResolver users

runSelectWithConn ::
  Default FromFields fields haskells =>
  Select fields ->
  ResolverQ () GQL.Web [haskells]
runSelectWithConn select = do
  conn <- lift ask
  liftIO $ Opaleye.runSelect conn select

-- Debug ---------------

printSql :: Default Unpackspec a a => Select a -> IO ()
printSql = putStrLn . fromMaybe "Empty select" . showSql
