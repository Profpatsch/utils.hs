#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell
{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
import Protolude

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.String (String)
import System.IO (hPutStrLn)
import System.Exit (ExitCode(..))
import System.Directory (makeAbsolute)
import System.Posix.Env (getEnvDefault)
import System.Posix.Files (isDirectory, getFileStatus)
import System.FilePath.Posix (takeDirectory, equalFilePath)

import qualified Data.IORef as Ref
import qualified System.Process as P
import qualified System.FSNotify as FS
import qualified Text.Regex.TDFA as R

import qualified Debug.Trace as D

main :: IO ()
main = do
  pf <- getEnvDefault "projectfolder" "."
  w <- getEnvDefault "watch" (pf <> "/src")
  e  <- getEnvDefault "exe"
  cmd <- getEnvDefault "command" "cabal new-build"
  watcher cmd pf w e

-- | Watch the source folder of a long-running program for changes,
-- then recompile the program with cabal and restart if the compilation
-- succeeded.
watcher :: String
        -- ^ command to invoke for rebuild (in project folder)
        -> FilePath
        -- ^ project folder for cabal new-build
        -> FilePath
        -- ^ file/folder to watch for source code changes
        -> FilePath
        -- ^ location of the program’s executable
        -> IO ()
watcher command projectFolder watch exe = do
  errlog "started"
  FS.withManager $ \mgr -> do

    watchAbs <- makeAbsolute watch
    serverHandle <- rebuildMaybeSpawn Nothing >>= Ref.newIORef :: IO (Ref.IORef (Maybe P.ProcessHandle))
    isBuilding <- Ref.newIORef False :: IO (Ref.IORef Bool)

      -- if it’s a file use the file’s folder and filter only on the file
    (watchFolder, changeFilter) <- isDirectory <$> getFileStatus watchAbs >>= \case
      True  -> pure (              watchAbs, actionOnAllChanges filterTempFiles)
      False -> pure (takeDirectory watchAbs, actionOnAllChanges $ equalFilePath watchAbs)

    -- start a watching job (in the background)
    _ <- FS.watchTree
      mgr          -- manager
      (D.traceShowId watchFolder)       -- directory to watch
      changeFilter
      (\_ -> do
        withBuildCheck isBuilding $
          Ref.readIORef serverHandle
            -- when a file changes, build it the server and on success start it again
            >>= rebuildMaybeSpawn
            >>= Ref.writeIORef serverHandle)

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
  where
    actionOnAllChanges action = \case
      FS.Added fp _ -> action fp
      FS.Modified fp _ -> action fp
      FS.Removed fp _ -> action fp
    -- filter out emacs `/.#foo files
    filterTempFiles :: FilePath -> Bool
    filterTempFiles fp = not $ R.match (R.makeRegex ".*/\\.#.*$" :: R.Regex) fp

    rebuild = do
        (_,_,_,cabP) <- P.createProcess $ (P.shell command)
                          { P.cwd = Just projectFolder , P.delegate_ctlc = True }
        P.waitForProcess cabP
    spawn = P.spawnProcess exe []
    rebuildMaybeSpawn :: Maybe P.ProcessHandle -> IO (Maybe P.ProcessHandle)
    rebuildMaybeSpawn oldServer = rebuild >>= \case
      ExitSuccess -> do
        maybe (pure ()) (\s -> do
          errlog "killing server"
          P.terminateProcess s
          _ <- P.waitForProcess s
          pure ()) oldServer
        errlog "spawning server"
        Just <$> spawn
      ExitFailure _ ->
        hPutStrLn stderr "ERRORS, OLD VERSION USED" >> pure oldServer

    -- changes should be ignored if there is already a build going on
    withBuildCheck :: Ref.IORef Bool -> IO () -> IO ()
    withBuildCheck bRef build = unlessM (Ref.readIORef bRef)
        $ Ref.writeIORef bRef True >> build >> Ref.writeIORef bRef False

errlog :: String -> IO ()
errlog = hPutStrLn stderr
