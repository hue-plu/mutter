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

module Model.Murmurs(murmurs) where

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

data MurmurT a b
  = Murmur
    { murmurId   :: a,
      murmurText :: b
    }
type Murmur = MurmurT Int Text
type MurmurField = MurmurT (Field SqlInt4) (Field SqlText)

$(makeAdaptorAndInstance "pMurmur" ''MurmurT)

murmursTable :: Table MurmurField MurmurField
murmursTable = table "murmurs" (pMurmur Murmur { murmurId = tableField "id"
                                       , murmurText = tableField "text"
                                       })

murmursSelect :: Select MurmurField
murmursSelect = selectTable murmursTable

taro :: Monad m => GQL.User m
taro =
  GQL.User
    { userId = pure 2,
      userName = pure "tanaka taro",
      userMurmurs = pure []
    }

murmurResolver :: Murmur -> ResolverQ () GQL.Web GQL.Murmur
murmurResolver Murmur {murmurId, murmurText} =
  return GQL.Murmur
    { GQL.murmurId = pure murmurId,
      GQL.murmurText = pure murmurText,
      GQL.murmurUser = pure taro
    }

murmurs :: ComposedResolver QUERY () GQL.Web [] GQL.Murmur
murmurs = do
  murmurs :: [Murmur] <- runSelectWithConn murmursSelect
  traverse murmurResolver murmurs
