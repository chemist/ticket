{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Ticket.Data where

import Data.Text.Lazy hiding (map, filter, empty)
import Data.Maybe
import Data.Time
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
                 
import Data.Data
import GHC.Generics
import Data.SafeCopy
import Data.IxSet
import Data.Aeson
import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Monad 

newtype LessonId = LessonId { unLessonId::Int }
   deriving (Show, Eq, Ord, Data, Enum, Typeable, SafeCopy, Generic)
 
instance ToJSON LessonId where
        toJSON (LessonId i) = toJSON i

instance FromJSON LessonId where
        parseJSON i = LessonId <$> parseJSON i
  
newtype GuestId = GuestId { unGuestId::Int }
   deriving (Show, Eq, Ord, Data, Enum, Typeable, SafeCopy, Generic)

instance ToJSON GuestId where
    toJSON (GuestId i) = toJSON i
    
instance FromJSON GuestId where
    parseJSON i = GuestId <$> parseJSON i

data Guest = GID { guestid :: GuestId }
           | Guest { guestid::GuestId
                   , firstname::Text
                   , secondname::Text
                   , age::Int
                   , phone::Text
                   , comment::Text
                   } deriving (Show, Eq, Data, Ord, Typeable, Generic)
                   
instance ToJSON Guest where
    toJSON (GID x) = object [ "gid"   .= toJSON x ]
    toJSON x = object [ "guestid"     .= (toJSON . guestid) x
                      , "firstname"   .= (toJSON . firstname) x
                      , "secondname"  .= (toJSON . secondname) x
                      , "age"         .= (toJSON . age) x
                      , "phone"       .= (toJSON . phone) x
                      , "comment"     .= (toJSON . comment) x
                      ]
                      
instance FromJSON Guest where
    parseJSON (Object v) = GID   <$> v .: "gid" <|>
                           Guest <$> v .: "guestid" 
                                  <*>  v .: "firstname"
                                  <*>  v .: "secondname"
                                  <*>  v .: "age"
                                  <*>  v .: "phone"
                                  <*>  v .: "comment"
    parseJSON _ = mzero

type RoomId = Int

data Room = Room { roomId::RoomId
                 , guest::Maybe Guest
                 } deriving (Show, Eq, Ord, Data, Typeable, Generic)

type Rooms = [Room]

instance FromJSON Room
instance ToJSON Room

data Lesson =  Lesson { lessonId::LessonId
                      , date::UTCTime
                      , classroom::Int
                      , teacher::String
                      , rooms::Rooms
                      } deriving (Show, Eq, Data, Ord, Typeable, Generic)
                      
instance FromJSON Lesson
instance ToJSON Lesson


$(deriveSafeCopy 0 'base ''Guest)
$(deriveSafeCopy 0 'base ''Lesson)
$(deriveSafeCopy 0 'base ''Room)

newtype FirstName = FirstName Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype SecondName = SecondName Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Age = Age Int deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Phone = Phone Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype ClassRoom = ClassRoom Int deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Teacher = Teacher String deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Word = Word Text deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable Lesson where
    empty = ixSet [ ixFun $ \x -> [utctDay $ date x]
                  , ixFun $ \x -> [lessonId x]
                  ]
                  
instance Indexable Guest where
    empty = ixSet [ ixFun $ \x -> [ FirstName $ firstname x ]
                  , ixFun $ \x -> [ SecondName $ secondname x ]
                  , ixFun $ \x -> [ Phone $ phone x ]
                  , ixFun $ \x -> [ guestid x ]
                  ]
                                 

data Ticket = Ticket { nextLessonId:: LessonId
                     , nextGuestId::GuestId
                     , lesson:: IxSet Lesson 
                     , iguest:: IxSet Guest 
                     } deriving (Data, Typeable)
                       
$(deriveSafeCopy 0 'base ''Ticket)

initialTicket::Ticket
initialTicket = Ticket { nextGuestId = GuestId 1
                       , nextLessonId = LessonId 1
                       , iguest = empty
                       , lesson = empty
                       }
                
initRoom::Rooms
initRoom = map (flip Room Nothing) [1 .. 20]

data Configure = Configure { port:: Int
                           , state::AcidState Ticket
                           }

