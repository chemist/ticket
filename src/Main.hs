{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Control.Applicative hiding (empty)
import Control.Monad.IO.Class
import Control.Monad.Trans
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import Data.Text.Lazy hiding (map, filter, empty)
import Data.Maybe
import Control.Monad
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Data.Time
import Data.ByteString.Lazy (ByteString)
import qualified Data.Aeson as A
import Data.Acid ( IsAcidic(..)
                 , AcidState(..)
                 , EventState(..)
                 , EventResult(..)
                 , Update(..)
                 , Query(..)
                 , QueryEvent(..)
                 , UpdateEvent(..)
                 , makeAcidic
                 , openLocalState)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Advanced ( query', update' )
import Control.Exception (bracket)
import Data.Data
import GHC.Generics
import Data.SafeCopy
import Data.IxSet
import qualified Data.IxSet as IxSet
import Network.HTTP.Types (Status(..))
import System.Locale (defaultTimeLocale)
import Ticket.Data
import Ticket.Acid

import Debug.Trace

openAcid = do
    l <- openLocalState initialTicket
    return $ Configure 3000 l 
    
closeAcid = createCheckpointAndClose . state 

main = bracket openAcid
               closeAcid
               main'

main'::Configure -> IO ()
main' conf = scotty (port conf) $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    
    get "/" $ file "static/index.html"
    -- get lessons list
    get "/lessons/:utc" $ do
        utc <- param "utc"
        case parseTime defaultTimeLocale "%FT%T%QZ" (unpack utc) of
             Just (x::UTCTime) -> getLessonsList conf x
             Nothing -> status $ Status 400 "Bad Request>"
    -- add new lesson
    post "/lesson/" $ do
        j <- jsonData
        addNewLesson conf j
    -- get  room by lesson
    get "/lesson/:id" $ do
        lid <- LessonId <$> param "id"
        getRooms conf lid
    -- edit lesson where id is lesson id 
    post "/lesson/:id" $ do
        _ <- LessonId <$> param "id"
        j <- jsonData
        addGuestToRoom conf j
        
    -- get guests
    get "/guest/" $ getGuests conf
    -- get guest by id
    get "/guest/:id" $ do
        lid <- GuestId <$> param "id"
        getGuest conf lid
    -- add guest
    post "/guest/" $ do
        j <- jsonData
        addNewGuest conf j

getLessonsList::Configure -> UTCTime -> ActionM ()
getLessonsList conf date = do
    a <- liftIO $ query' (state conf) (CurrentLessons date)
    json a

getRooms::Configure -> LessonId -> ActionM ()
getRooms conf lid = do
    less <- liftIO $ query' (state conf) (LessonById lid)
    json less


addNewLesson::Configure -> Lesson -> ActionM ()
addNewLesson conf less = do
      r <- liftIO $ update' (state conf) (NewLesson less)
      json r


addGuestToRoom::Configure -> Lesson -> ActionM ()
addGuestToRoom conf  less = do
    _ <- liftIO $ update' (state conf) (UpdateLesson less)
    less <- liftIO $ query' (state conf) (LessonById $ lessonId less)
    json less

getGuests::Configure -> ActionM ()
getGuests conf = do
    a <- liftIO $ query' (state conf) QueryGuests 
    json a
    
addNewGuest::Configure -> Guest -> ActionM ()
addNewGuest conf g = do
    a <- liftIO $ update' (state conf) (NewGuest g)
    json a
    
getGuest::Configure -> GuestId -> ActionM ()
getGuest conf lid = do
    a <- liftIO $ query' (state conf) (GuestById lid)
    json a
