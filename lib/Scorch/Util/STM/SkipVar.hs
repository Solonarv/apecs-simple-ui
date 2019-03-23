module Scorch.Util.STM.SkipVar where

import Control.Concurrent.STM

-- | A simple skip variable. This is a special case of a skip channel:
-- it always holds exactly 0 or 1 value.
-- 
-- Writing to a TSVar will never block, and will overwrite 
--
-- Reading from a TSVar will block until a value is available, then
-- remove that value from the TSVar and return it.
newtype TSVar a = TSVar (TVar (Maybe a))

newEmptyTSVar :: STM (TSVar a)
newEmptyTSVar = TSVar <$> newTVar Nothing

newTSVar :: a -> STM (TSVar a)
newTSVar x = TSVar <$> newTVar (Just x)

newEmptyTSVarIO :: STM (TSVar a)
newEmptyTSVarIO = TSVar <$> newTVarIO Nothing

newTSVarIO :: a -> STM (TSVar a)
newTSVarIO x = TSVar <$> newTVarIO (Just x)

readTSVar :: TSVar a -> STM a
readTSVar (TSVar var) = do
  val <- readTVar var
  case val of
    Just x -> x <$ writeTVar var Nothing
    Nothing -> retry

writeTSVar :: TSVar a -> a -> STM ()
writeTSVar (TSVar var) x = writeTVar var (Just x)