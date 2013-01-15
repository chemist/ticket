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
import Control.Applicative ((<$>))

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

data Guest = Guest { guestid::GuestId
                   , firstname::Text
                   , secondname::Text
                   , age::Int
                   , phone::Text
                   , comment::Text
                   } deriving (Show, Eq, Data, Ord, Typeable, Generic)

instance FromJSON Guest
instance ToJSON Guest

type RoomId = Int

data Room = Room { roomId::RoomId
                 , guest::Maybe GuestId
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
                  ]
                                 

data Lessons = Lessons { nextLessonId:: LessonId
                       , lesson:: IxSet Lesson } deriving (Data, Typeable)
                       
data Guests = Guests { nextGuestId::GuestId
                     , iguest:: IxSet Guest } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Lessons)
$(deriveSafeCopy 0 'base ''Guests)

initialGuests::Guests
initialGuests = Guests { nextGuestId = GuestId 1
                       , iguest = empty
                       }
                
initialLessons::Lessons
initialLessons = Lessons { nextLessonId = LessonId 1
                         , lesson = empty
                         }

initRoom::Rooms
initRoom = map (flip Room Nothing) [1 .. 20]

data Configure = Configure { port:: Int
                           , state::AcidState Lessons
                           , guestState::AcidState Guests
                           }

