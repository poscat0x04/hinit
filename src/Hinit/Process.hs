module Hinit.Process where

import Control.Effect.Lift
import Control.Effect.Terminal
import Control.Effect.Throw
import Control.Monad
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Hinit.Errors
import Hinit.Types
import Hinit.Utils
import Path
import System.Directory
import System.Exit
import System.IO (hGetContents)
import System.Process

vcsInitProc :: VCS -> Maybe CreateProcess
vcsInitProc Git = Just $ proc "git" ["init"]
vcsInitProc Mercurial = Just $ proc "hg" ["init"]
vcsInitProc Darcs = Just $ proc "darcs" ["init"]
vcsInitProc Pijul = Just $ proc "pijul" ["init"]
vcsInitProc (Other _) = Nothing

guessExecutableExists :: Has (Lift IO) sig m => CmdSpec -> m Bool
guessExecutableExists ShellCommand {} = pure True
guessExecutableExists (RawCommand exe _) = isJust <$> sendIO (findExecutable exe)

initVCS ::
  ( Has (Lift IO) sig m,
    Has (Throw ProcessExitFailure) sig m,
    Has Terminal sig m
  ) =>
  VCS ->
  Path a Dir ->
  m ()
initVCS vcs dir = do
  whenJust (vcsInitProc vcs) $ \process -> do
    exists <- guessExecutableExists $ cmdspec process
    let cp =
          process
            { cwd = Just (toFilePath dir)
            }
    if not exists
      then prettyPrintWarning $ pretty $ VcsCmdNotFound vcs
      else runProc cp

runProc ::
  ( Has (Lift IO) sig m,
    Has (Throw ProcessExitFailure) sig m
  ) =>
  CreateProcess ->
  m ()
runProc cp = do
  ~(_, Just stdout, Just stderr, ph) <-
    sendIO $
      createProcess
        cp
          { std_out = CreatePipe,
            std_err = CreatePipe
          }
  c <- sendIO $ waitForProcess ph
  case c of
    ExitSuccess -> pure ()
    ExitFailure i -> do
      out <- sendIO $ hGetContents stdout
      err <- sendIO $ hGetContents stderr
      throwError $ ProcessExitFailure (cmdspec cp) i out err
