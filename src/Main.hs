{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import Data.Acid ( openLocalState, AcidState(..))
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

import Crypto.PasswordStore
import Data.ByteString (ByteString)

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
main' Configure{..} = scotty port $ do

    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ withSession store' (fromString "session") def session'
    
    get "/" $ file "static/index.html"

    post "/login" $ do
        l <- jsonData
        v <- vault <$> request
        let Just (_, sSession) = Vault.lookup session' v
        check <- liftIO $ query' state (CheckLogin l)
        if check 
           then do
               sSession "authorized" "true"
               sSession "login" (login l)
               status status200
               text "welcom"
           else do
               sSession "authorized" "false"
               sSession "login" (login l)
               status status401
               text "not authorized"

-- admin section
    post "/auth" $ do
        Login{..} <- jsonData
        checkAuthorization session'
        onlyAdminCanDo session' $ do
            hash <-  liftIO $ makePassword password 12
            liftIO $ update' state (NewLogin $ Login login hash)

    get "/users" $ do
        checkAuthorization session'
        onlyAdminCanDo session' $ do
           list <- query' state ListLogin
           json list

    -- get lessons list
    get "/lessons/:utc" $ do
        checkAuthorization session'
        utc' <- param "utc"
        case parseTime defaultTimeLocale "%FT%T%QZ" (unpack utc') of
             Just (x::UTCTime) -> getLessonsList state x
             Nothing -> status $ Status 400 "Bad Request>"
    -- add new lesson
    post "/lesson/" $ do
        checkAuthorization session'
        j <- jsonData
        addNewLesson state j
    -- get  room by lesson
    get "/lesson/:id" $ do
        checkAuthorization session'
        lid <- LessonId <$> param "id"
        getRooms state lid
    -- edit lesson where id is lesson id 
    post "/lesson/:id" $ do
        checkAuthorization session'
        _ <- LessonId <$> param "id"
        j <- jsonData
        addGuestToRoom state j
        
    -- get guests
    get "/guest/" $ do
        checkAuthorization session'
        getGuests state
    -- get guest by id
    get "/guest/:id" $ do
        checkAuthorization session'
        lid <- GuestId <$> param "id"
        getGuest state lid
    -- add guest
    post "/guest/" $ do
        checkAuthorization session'
        j <- jsonData
        addNewGuest state j

    -- ^ if not authorized, status 401
    notFound $ do
        v' <- vault <$> request
        let Just (lSession, _) = Vault.lookup session' v'
        check <- lSession "authorized"
        unless (check == Just "true") $ do
            status status401
            text "not authorized"
        
-- ^ when user not authorized, go next route
checkAuthorization::Vault.Key (Session ActionM Text ByteString) -> ActionM ()
checkAuthorization session = do
    v <- vault <$> request
    let Just (lSession, _) = Vault.lookup session v
    check <- lSession "authorized"
    case check of
         Just "true" -> return ()
         _ -> next

whois::Vault.Key (Session ActionM Text ByteString) -> ActionM ByteString
whois session = do
    v <- vault <$> request
    let Just (lSession, _) = Vault.lookup session v
    who <- lSession "login"
    case who of
         Just l -> return l
         _ -> return ""

onlyAdminCanDo::Vault.Key (Session ActionM Text ByteString) -> ActionM () -> ActionM ()
onlyAdminCanDo session action = do
    l <- whois session
    if l == "admin"
       then action
       else do
           status status403
           text "Требуются административные привилегии"

getLessonsList::AcidState Ticket -> UTCTime -> ActionM ()
getLessonsList state' date' = do
    a <- liftIO $ query' state' (CurrentLessons date')
    json a

getRooms::AcidState Ticket -> LessonId -> ActionM ()
getRooms state' lid = do
    less <- liftIO $ query' state' (LessonById lid)
    json less


addNewLesson::AcidState Ticket -> Lesson -> ActionM ()
addNewLesson state' less = do
      r <- liftIO $ update' state' (NewLesson less)
      json r


addGuestToRoom::AcidState Ticket -> Lesson -> ActionM ()
addGuestToRoom state'  less = do
    _ <- liftIO $ update' state' (UpdateLesson less)
    less' <- liftIO $ query' state' (LessonById $ lessonId less)
    json less'

getGuests::AcidState Ticket -> ActionM ()
getGuests state' = do
    a <- liftIO $ query' state' QueryGuests 
    json a
    
addNewGuest::AcidState Ticket -> Guest -> ActionM ()
addNewGuest state' g = do
    a <- liftIO $ update' state' (NewGuest g)
    json a
    
getGuest::AcidState Ticket -> GuestId -> ActionM ()
getGuest state' lid = do
    a <- liftIO $ query' state' (GuestById lid)
    json a
