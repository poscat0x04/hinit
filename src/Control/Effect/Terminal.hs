{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Terminal where

import Control.Algebra
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.Functor
import Data.Kind
import Data.Text (Text, pack)
import Prettyprinter
import Prettyprinter.Render.Terminal
import System.Console.Haskeline
import System.IO

data Terminal (m :: Type -> Type) a where
  PrettyPrint :: Handle -> Doc AnsiStyle -> Terminal m ()
  Prompt :: String -> Terminal m (Maybe Text)

prettyPrint :: Has Terminal sig m => Handle -> Doc AnsiStyle -> m ()
prettyPrint handle doc = send $ PrettyPrint handle $ doc <> "\n"

prompt :: Has Terminal sig m => String -> m (Maybe Text)
prompt = send . Prompt

newtype TerminalC m a = TerminalC {runTerminalC :: InputT m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runTerminal' :: (MonadIO m, MonadMask m) => Settings m -> TerminalC m a -> m a
runTerminal' s = runInputT s . runTerminalC

runTerminal :: (MonadIO m, MonadMask m) => TerminalC m a -> m a
runTerminal = runTerminal' $ setComplete noCompletion defaultSettings

instance (MonadMask m, MonadIO m, Algebra sig m) => Algebra (Terminal :+: sig) (TerminalC m) where
  alg hdl sig ctx = TerminalC $
    case sig of
      L (PrettyPrint handle doc) -> do
        liftIO $ renderIO handle $ layoutSmart defaultLayoutOptions doc
        pure $ ctx $> ()
      L (Prompt p) -> do
        res <- getInputLine p
        pure $ ctx $> fmap pack res
      R other -> withRunInBase $ \runInput -> alg (runInput . runTerminalC . hdl) other ctx
