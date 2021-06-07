module ReaderT where

import           Control.Monad.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.State  (liftIO)
import           Data.IORef           (IORef, newIORef, readIORef, writeIORef)

square :: ReaderT (IORef Int) IO ()
square = do
  ref <- ask
  liftIO $ do
    x <- readIORef ref
    writeIORef ref (x * x)

main = do
  v <- newIORef 1111
  runReaderT square v
  y <- readIORef v  -- 1234321
  print y
