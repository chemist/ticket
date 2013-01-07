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

data Guest = Guest { firstname::Text
                   , secondname::Text
                   , age::Int
                   , phone::Text
                   , comment::Text
                   } deriving (Show, Eq, Data, Ord, Typeable, Generic)

instance FromJSON Guest
instance ToJSON Guest

type RoomId = Int

data Room = Room { roomId::RoomId
                 , guest::Maybe Guest
                 } deriving (Show, Eq, Ord, Data, Typeable, Generic)

type Rooms = [Room]

data Lesson =  Lesson { lessonId::LessonId
                      , date::UTCTime
                      , lessonType::Int
                      , rooms::Rooms
                      } deriving (Show, Eq, Data, Ord, Typeable, Generic)

instance ToJSON LessonId where
        toJSON (LessonId i) = toJSON i

instance FromJSON LessonId where
        parseJSON a = LessonId <$> parseJSON a

instance FromJSON Room
instance FromJSON Lesson
instance ToJSON Room
instance ToJSON Lesson

newtype Hour = Hour { unHour::Int } 
   deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic, SafeCopy)

instance FromJSON Hour
instance ToJSON Hour

newtype JTime = JTime { utcTime::UTCTime }
   deriving (Show, Eq, Ord, Data, Typeable, Generic, SafeCopy)

instance FromJSON JTime
instance ToJSON JTime


$(deriveSafeCopy 0 'base ''Guest)
$(deriveSafeCopy 0 'base ''Lesson)
$(deriveSafeCopy 0 'base ''Room)

newtype FirstName = FirstName Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype SecondName = SecondName Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Age = Age Int deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Phone = Phone Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype LessonType = LessonType Int deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Word = Word Text deriving (Eq, Ord, Data, Typeable, SafeCopy)

utcToHour::UTCTime -> Hour
utcToHour = Hour . todHour . timeToTimeOfDay . utctDayTime

instance Indexable Guest where
    empty = ixSet [ ixFun $ \x -> [FirstName $ firstname x]
                  , ixFun $ \x -> [SecondName $ secondname x]
                  , ixFun $ \x -> [Age $ age x]
                  , ixFun $ \x -> [Phone $ phone x]
                  ]

instance Indexable Lesson where
    empty = ixSet [ ixFun $ \x -> [utctDay $ date x]
                  , ixFun $ \x -> [utcToHour $ date x]
                  , ixFun $ \x -> [LessonType $ lessonType x]
                  , ixFun $ \x -> [lessonId x]
                  , ixFun $ \x -> makeIx $ rooms x
                  ]
            where makeIx x = map (\(Room _ (Just a)) -> map Word  [firstname a, secondname a, phone a]) $ filter (isJust . guest) x

data Lessons = Lessons { nextLessonId:: LessonId
                       , lesson:: IxSet Lesson } deriving (Data, Typeable)

$(deriveSafeCopy 0 'base ''Lessons)

initialLessons::Lessons
initialLessons = Lessons { nextLessonId = LessonId 1
                         , lesson = empty
                         }

initRoom::Rooms
initRoom = map (flip Room Nothing) [1 .. 20]

workHour::[Hour]
workHour = map Hour [9 .. 20]

data Configure = Configure { port:: Int
                           , state::AcidState Lessons
                           }

