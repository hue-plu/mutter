{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module GraphQL where

import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Data.Morpheus.Document     (importGQLDocumentWithNamespace)
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (Connection)

importGQLDocumentWithNamespace "src/schema.gql"

type Web = ReaderT Connection IO

runWeb :: Web a -> Connection -> IO a
runWeb conn = do
  runReaderT conn
