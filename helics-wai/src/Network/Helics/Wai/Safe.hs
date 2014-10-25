module Network.Helics.Wai.Safe
    ( 
    -- * middleware
      helics
    , dummyHelics
    -- * getter
    , transactionId
    , lookupTransactionId
    -- * reexports
    , def
    ) where

import Network.Wai
import Network.Helics
import Network.Helics.Internal.Types (TransactionId(..))

import Data.Vault.Lazy as V

-- | helics middleware.
helics :: Key TransactionId -> Middleware
helics key app req send = withTransaction def $ \tid -> do
   setRequestUrl (rawPathInfo req) tid
   app req { vault = insert key tid (vault req) } send

-- | Middleware which add dummy TransactionId to Request. since v0.4.0.
dummyHelics :: Key TransactionId -> Middleware
dummyHelics key app req send = do
    app req { vault = insert key DummyTransactionId (vault req) } send

-- | get TransactionId from request.
transactionId :: Key TransactionId -> Request -> TransactionId
transactionId key req = maybe (error "helics middleware is not installed.") id $
    lookupTransactionId key req

-- | get TransactionId when middleware installed.
lookupTransactionId :: Key TransactionId -> Request -> Maybe TransactionId
lookupTransactionId key req = V.lookup key (vault req)
