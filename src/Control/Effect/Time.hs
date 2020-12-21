{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Time where

import Control.Algebra
import Control.Carrier.Lift
import Control.Monad.IO.Class
import Data.Functor
import Data.Kind
import Data.Time (ZonedTime)
import qualified Data.Time as T

data Time (m :: Type -> Type) k where
  GetZonedTime :: Time m ZonedTime

getZonedTime :: Has Time sig m => m ZonedTime
getZonedTime = send GetZonedTime

newtype TimeC m a = TimeC {runTimeC :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance Has (Lift IO) sig m => Algebra (Time :+: sig) (TimeC m) where
  alg hdl sig ctx = TimeC $
    case sig of
      L GetZonedTime -> do
        tz <- sendIO T.getZonedTime
        pure (ctx $> tz)
      R other -> alg (runTimeC . hdl) other ctx
