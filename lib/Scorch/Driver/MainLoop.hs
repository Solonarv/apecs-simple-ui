module Scorch.Driver.MainLoop where

import Data.Void

import Control.Concurrent.Async
import Control.Concurrent.STM

import TimeStep

data DriverConfig = DriverConfig
  { drvSimTPS :: Rational
  , drvMaxFPS :: Rational
  }

data DriverCode s r = DriverCode
  { drvTickSim    :: s -> STM ()
  , drvPrepRender :: s -> STM r
  , drvExecRender :: r -> IO ()
  }

data DriverThreads = DriverThreads
  { drvThStepSim    :: forall void. Async void
  , drvThPrepRender :: forall void. Async void
  , drvThExecRender :: forall void. Async void
  }

waitDriver :: DriverThreads -> IO ()
waitDriver threads = waitAnyCancel
  [ drvThStepSim threads
  , drvThPrepRender threads
  , drvThExecRender threads
  ]

drive :: DriverConfig -> DriverCode s r -> s -> IO DriverThreads
drive cfg code s = do
  simThread <- asyncRepeatedly (drvSimTPS cfg) (atomically $ drvTickSim code s)
  renderVar <- newEmptyTSVarIO
  prepThread <- asyncRepeatedly (drvMaxFPS cfg) . atomically $
    drvPrepRender code s >>= writeTSVar renderVar
  rdrThread <- asyncRepeatedly (drvMaxFPS cfg) $ do
    render <- atomically (readTSVar renderVar)
    drvExecRender code render
  pure DriverThreads
    { drvThStepSim    = vacuous simThread
    , drvThPrepRender = vacuous prepThread
    , drvThExecRender = vacuous rdrThread
    }

stop :: DriverThreads -> IO ()
stop threads = traverse_ cancel
  [ drvThStepSim threads
  , drvThPrepRender threads
  , drvThExecRender threads
  ]