{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where
  import Prelude ()
  import Prelude.Compat

  import Control.Monad.Except
  import Control.Monad.Reader
  import Data.Aeson
  import Data.Aeson.Types
  import Data.Attoparsec.ByteString
  import Data.ByteString (ByteString)
  import Data.List
  import Data.Maybe
  import Data.String.Conversions
  import Data.Time.Calendar
  import GHC.Generics
  import Lucid
  import Network.HTTP.Media ((//), (/:))
  import Network.Wai
  import Network.Wai.Handler.Warp
  import Servant
  import System.Directory
  import Text.Blaze
  import Text.Blaze.Html.Renderer.Utf8
  import Servant.Types.SourceT (source)
  import qualified Data.Aeson.Parser
  import qualified Text.Blaze.Html

  type UserAPI1 = "users" :> Get '[JSON] [User]
              :<|> "binary-search" :> Get '[JSON] BinarySearchResults

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
    [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
    , User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
    ]

 -- === Binary Search DTO === 
  data BinarySearchResults = BinarySearchResults
    { index :: Int } deriving (Eq, Show, Generic)

  instance ToJSON BinarySearchResults

  searchResult1 :: BinarySearchResults
  searchResult1 = BinarySearchResults 0

-- === API Implementation
  server1 :: Server UserAPI1
  server1 = return users1
        :<|> return searchResult1

  userAPI :: Proxy UserAPI1
  userAPI = Proxy

-- === Server Start ===
  app1 :: Application
  app1 = serve userAPI server1

  main :: IO ()
  main = run 8081 app1

