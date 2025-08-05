module Modulus.BE.DB.Queries.User (
        getUserByEmailQ
      , addUser
      , updateUser
      , getUser
    ) where

import Orville.PostgreSQL 
import Modulus.BE.DB.Internal.Model 
import Data.Text (Text)
import Modulus.BE.DB.Internal.Table (userTable)
import Modulus.BE.DB.Internal.Marshaller (userEmailField)

getUserByEmailQ :: MonadOrville m => Text -> m (Maybe UserRead)
getUserByEmailQ inputEmail = 
    findFirstEntityBy userTable $ where_ (fieldEquals userEmailField inputEmail)

addUser :: MonadOrville m => UserWrite -> m UserRead
addUser = insertAndReturnEntity userTable

updateUser :: MonadOrville m => UserID -> UserWrite -> m ()
updateUser = updateEntity userTable

getUser :: MonadOrville m => UserID -> m (Maybe UserRead)
getUser = findEntity userTable
