{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module DTO.BinarySearchResults where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.String.Conversions
import GHC.Generics
import Data.Time.Calendar

-- === Binary Search DTO === 

data BinarySearchResults = BinarySearchResults
  { index :: Int } deriving (Eq, Show, Generic)

instance ToJSON BinarySearchResults

searchResult1 :: BinarySearchResults
searchResult1 = BinarySearchResults 0

