#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell
{-# LANGUAGE NoImplicitPrelude, LambdaCase #-}
import Protolude

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.String (String)
import System.IO (hPutStrLn)
import System.Exit (ExitCode(..))

import qualified Data.IORef as Ref
import qualified System.Process as P
import qualified System.FSNotify as FS
import qualified Text.Regex.TDFA as R

main :: IO ()
main = do
  -- insert your file paths here
  -- watcher fp wf e
  return ()

-- | Watch the source folder of a long-running program for changes,
-- then recompile the program with cabal and restart if the compilation
-- succeeded.
watcher :: FilePath
        -- ^ project folder for cabal new-build
        -> FilePath
        -- ^ watch folder for the source code
        -> FilePath
        -- ^ location of the program’s executable
        -> IO ()
watcher projectFolder watchFolder exe = do
  errlog "started"
  FS.withManager $ \mgr -> do

    serverHandle <- rebuildMaybeSpawn Nothing >>= Ref.newIORef :: IO (Ref.IORef (Maybe P.ProcessHandle))
    isBuilding <- Ref.newIORef False :: IO (Ref.IORef Bool)

    -- start a watching job (in the background)
    _ <- FS.watchTree
      mgr          -- manager
      watchFolder       -- directory to watch
      (\case
          FS.Added fp _ -> filterTempFiles fp
          FS.Modified fp _ -> filterTempFiles fp
          FS.Removed fp _ -> filterTempFiles fp)
      (\_ -> do
        withBuildCheck isBuilding $
          Ref.readIORef serverHandle
            -- when a file changes, build it the server and on success start it again
            >>= rebuildMaybeSpawn
            >>= Ref.writeIORef serverHandle)

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
  where
    -- filter out emacs `/.#foo files
    filterTempFiles :: FilePath -> Bool
    filterTempFiles fp = not $ R.match (R.makeRegex ".*/\\.#.*$" :: R.Regex) fp

    rebuild = do
        (_,_,_,cabP) <- P.createProcess $ (P.proc "cabal" ["new-build"])
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
