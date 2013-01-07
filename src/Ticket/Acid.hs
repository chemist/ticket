{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ticket.Acid where

import Control.Applicative hiding (empty)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as S
import Data.Text.Lazy hiding (map, filter, empty)
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
                 
import Data.IxSet
import qualified Data.IxSet as IxSet
import Ticket.Data

newLesson::Lesson -> Update Lessons Lesson
newLesson l = do
    lessons@Lessons{..} <- S.get
    let new = Lesson { lessonId = nextLessonId, date = date l, lessonType = lessonType l, rooms = initRoom}
    S.put $ lessons { nextLessonId = succ nextLessonId
                  , lesson = IxSet.insert new lesson}
    return new

updateLesson::Lesson -> Update Lessons Lesson
updateLesson updatedLesson = do
       l@Lessons{..} <- S.get
       S.put $ l { lesson = IxSet.updateIx (lessonId updatedLesson) updatedLesson lesson }
       return updatedLesson


lessonsByDate::UTCTime -> Query Lessons [Lesson]
lessonsByDate d = do
    Lessons{..} <- R.ask
    return $ IxSet.toDescList (Proxy::Proxy Day) $ lesson @= utctDay d

currentLessons::UTCTime -> Query Lessons [Lesson]
currentLessons d = do
    let period = (addDays (-2) $ utctDay d, addDays 20 $ utctDay d)
    Lessons{..} <- R.ask
    return $ IxSet.toDescList (Proxy::Proxy Day) $ lesson @>< period

lessonById::LessonId -> Query Lessons (Maybe Lesson)
lessonById lid = do
    Lessons{..} <- R.ask
    return $ getOne $ lesson @= lid

roomByLessonId::LessonId -> Query Lessons (Maybe Rooms)
roomByLessonId lid = do
    Lessons{..} <- R.ask
    return $ rooms <$> (getOne $ lesson @= lid)

freeHourByDate::TimeZone -> UTCTime -> Query Lessons [Hour]
freeHourByDate z (UTCTime t _) = do
    Lessons{..} <- R.ask
    return $ filter (flip notElem (toHour $ IxSet.toDescList (Proxy::Proxy Day) $ lesson @= t)) workHour
    where toHour = map (\x -> localToHour $ utcToLocalTime z (date x)) 

localToHour::LocalTime -> Hour
localToHour = Hour . todHour . localTimeOfDay

$(makeAcidic ''Lessons
  [ 'newLesson
  , 'updateLesson
  , 'lessonsByDate
  , 'lessonById
  , 'roomByLessonId
  , 'currentLessons
  , 'freeHourByDate
  ])


