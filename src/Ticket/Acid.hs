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
import Debug.Trace

newLesson::Lesson -> Update Ticket Lesson
newLesson l = do
    lessons@Ticket{..} <- S.get
    let new = Lesson { lessonId = nextLessonId
                     , date = date l
                     , teacher = teacher l
                     , classroom = classroom l
                     , rooms = initRoom
                     }
    S.put $ lessons { nextLessonId = succ nextLessonId
                  , lesson = IxSet.insert new lesson}
    return new

updateLesson::Lesson -> Update Ticket ()
updateLesson updatedLesson = do
       l@Ticket{..} <- S.get
       S.put $ l { lesson = IxSet.updateIx (lessonId updatedLesson) updatedLesson lesson }
       return ()


currentLessons::UTCTime -> Query Ticket [Lesson]
currentLessons d = do
    let period = (addDays (-2) $ utctDay d, addDays 20 $ utctDay d)
    Ticket{..} <- R.ask
    return $ IxSet.toDescList (Proxy::Proxy Day) $ lesson @>< period

lessonById::LessonId -> Query Ticket (Maybe Lesson)
lessonById lid = do
    Ticket{..} <- R.ask
    let res = getOne $ lesson @= lid
        fun::Maybe Guest -> Maybe Guest
        fun (Just x) = getOne $ iguest @= guestid x
        fun Nothing = Nothing
    return $ fromIdToGuest fun <$> res
    
fromIdToGuest::(Maybe Guest -> Maybe Guest) -> Lesson -> Lesson
fromIdToGuest fun less = let rooms' = map (\x -> Room (roomId x) (fun $ guest x)) $ rooms less
                    in less { rooms = rooms' }
                    
newGuest::Guest -> Update Ticket Guest
newGuest g = do
    guests@Ticket{..} <- S.get
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
    
guestById::GuestId -> Query Ticket (Maybe Guest)
guestById i = do
    Ticket{..} <- R.ask
    return $ getOne $ iguest @= i
    
queryGuests::Query Ticket [Guest]
queryGuests = do
    Ticket{..} <- R.ask
    return $ IxSet.toList iguest
    
updateGuest::Guest -> Update Ticket Guest
updateGuest g = do
    guests@Ticket{..} <- S.get
    S.put $ guests { iguest = IxSet.updateIx (guestid g) g iguest }
    return g


$(makeAcidic ''Ticket
  [ 'newLesson
  , 'updateLesson
  , 'lessonById
  , 'currentLessons
  , 'guestById
  , 'queryGuests
  , 'newGuest
  , 'updateGuest
  ])
 


