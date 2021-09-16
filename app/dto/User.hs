{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module DTO.User where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.String.Conversions
import GHC.Generics
import Data.Time.Calendar

-- === User DTO  ===
data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
  [User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1) 
  ,User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
  ]

