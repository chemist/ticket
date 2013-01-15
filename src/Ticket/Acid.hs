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
    let new = Lesson { lessonId = nextLessonId
                     , date = date l
                     , teacher = teacher l
                     , classroom = classroom l
                     , rooms = initRoom
                     }
    S.put $ lessons { nextLessonId = succ nextLessonId
                  , lesson = IxSet.insert new lesson}
    return new

updateLesson::Lesson -> Update Lessons Lesson
updateLesson updatedLesson = do
       l@Lessons{..} <- S.get
       S.put $ l { lesson = IxSet.updateIx (lessonId updatedLesson) updatedLesson lesson }
       return updatedLesson


currentLessons::UTCTime -> Query Lessons [Lesson]
currentLessons d = do
    let period = (addDays (-2) $ utctDay d, addDays 20 $ utctDay d)
    Lessons{..} <- R.ask
    return $ IxSet.toDescList (Proxy::Proxy Day) $ lesson @>< period

lessonById::LessonId -> Query Lessons (Maybe Lesson)
lessonById lid = do
    Lessons{..} <- R.ask
    return $ getOne $ lesson @= lid
    
newGuest::Guest -> Update Guests Guest
newGuest g = do
    guests@Guests{..} <- S.get
    let new = Guest { guestid = nextGuestId
                    , firstname = firstname g
                    , secondname = secondname g
                    , phone = phone g
                    , age = age g
                    , comment = comment g
                    }
    S.put $ guests { nextGuestId = succ nextGuestId
                   , iguest = IxSet.insert new iguest
                   }
    return new
    
guestById::GuestId -> Query Guests (Maybe Guest)
guestById i = do
    Guests{..} <- R.ask
    return $ getOne $ iguest @= i
    
updateGuest::Guest -> Update Guests Guest
updateGuest g = do
    guests@Guests{..} <- S.get
    S.put $ guests { iguest = IxSet.updateIx (guestid g) g iguest }
    return g


$(makeAcidic ''Lessons
  [ 'newLesson
  , 'updateLesson
  , 'lessonById
  , 'currentLessons
  ])
 
$(makeAcidic ''Guests
  [ 'guestById
  ])


