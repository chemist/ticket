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
import Data.Text.Lazy hiding (map, filter, empty)
import Control.Monad
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Session 
import Network.Wai.Session.Map
import Data.Time
import Data.Acid ( openLocalState )
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Advanced ( query', update' )
import Control.Exception (bracket)
import System.Locale (defaultTimeLocale)
import Ticket.Data
import Ticket.Acid

import qualified Data.Vault as Vault
import Data.String (fromString)
import Data.Default (def)
import Network.HTTP.Types.Status

-- import Debug.Trace

openAcid ::  IO Configure
openAcid = do
    l <- openLocalState initialTicket
    session <- Vault.newKey
    store <- mapStore_ 
    return $ Configure 3000 l session store
    
closeAcid ::  Configure -> IO ()
closeAcid = createCheckpointAndClose . state 

main::IO ()
main = bracket openAcid
               closeAcid
               main'

main'::Configure -> IO ()
main' conf = scotty (port conf) $ do

    let session = session' conf
        store   = store'   conf
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ withSession store (fromString "session") def session
    
    get "/" $ file "static/index.html"

    post "/login" $ do
        j <- jsonData
        v <- vault <$> request
        let Just (_, sSession) = Vault.lookup session v
        case j of
              Login "chemist" "123" -> do
                  sSession "authorized" True
                  status status200 
                  text "welcom"
              _ -> do
                  sSession "authorized" False
                  status status401
                  text "not authorized"

    -- get lessons list
    get "/lessons/:utc" $ do
        checkAuthorization session
        utc' <- param "utc"
        case parseTime defaultTimeLocale "%FT%T%QZ" (unpack utc') of
             Just (x::UTCTime) -> getLessonsList conf x
             Nothing -> status $ Status 400 "Bad Request>"
    -- add new lesson
    post "/lesson/" $ do
        checkAuthorization session
        j <- jsonData
        addNewLesson conf j
    -- get  room by lesson
    get "/lesson/:id" $ do
        checkAuthorization session
        lid <- LessonId <$> param "id"
        getRooms conf lid
    -- edit lesson where id is lesson id 
    post "/lesson/:id" $ do
        checkAuthorization session
        _ <- LessonId <$> param "id"
        j <- jsonData
        addGuestToRoom conf j
        
    -- get guests
    get "/guest/" $ do
        checkAuthorization session
        getGuests conf
    -- get guest by id
    get "/guest/:id" $ do
        checkAuthorization session
        lid <- GuestId <$> param "id"
        getGuest conf lid
    -- add guest
    post "/guest/" $ do
        checkAuthorization session
        j <- jsonData
        addNewGuest conf j

    -- ^ if not authorized, status 401
    notFound $ do
        v' <- vault <$> request
        let Just (lSession, _) = Vault.lookup session v'
        check <- lSession "authorized"
        unless (check == Just True) $ do
            status status401
            text "not authorized"
        
-- ^ when user not authorized, go next route
checkAuthorization::Vault.Key (Session ActionM Text Bool) -> ActionM ()
checkAuthorization session = do
    v <- vault <$> request
    let Just (lSession, _) = Vault.lookup session v
    check <- lSession "authorized"
    case check of
         Just True -> return ()
         _ -> next

        
getLessonsList::Configure -> UTCTime -> ActionM ()
getLessonsList conf date' = do
    a <- liftIO $ query' (state conf) (CurrentLessons date')
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
    less' <- liftIO $ query' (state conf) (LessonById $ lessonId less)
    json less'

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
