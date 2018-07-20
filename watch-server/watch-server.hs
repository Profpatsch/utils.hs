#!/usr/bin/env nix-shell
#!nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (h: with h; [ protolude fsnotify regex-tdfa optparse-generic ])"
{-# LANGUAGE NoImplicitPrelude, LambdaCase, TypeOperators, DataKinds, DeriveGeneric, OverloadedStrings, RecordWildCards, ViewPatterns #-}
import Protolude hiding (hPutStrLn)

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import System.IO (hPutStrLn)
import System.Exit (ExitCode(..))
import System.Directory (makeAbsolute)
import System.Posix.Files (isDirectory, getFileStatus)
import System.FilePath.Posix (takeDirectory, equalFilePath)

import qualified Data.IORef as Ref
import qualified System.Process as P
import qualified System.FSNotify as FS
import qualified Text.Regex.TDFA as R
import qualified Options.Generic as O


data Args
   = Args {
            projectdir :: Maybe FilePath O.<?> "directory the rebuild command is executed in"
          -- TODO: make NonEmpty (see https://github.com/Gabriel439/Haskell-Optparse-Generic-Library/issues/28)
          , watch      :: [FilePath]     O.<?> "files/directories to be watched"
          , command    :: Text           O.<?> "command to build executable"
          , exe        :: Text           O.<?> "potentially long-running executable to (re-)run after rebuild" }
   deriving (Generic)
instance O.ParseRecord Args

main :: IO ()
main = do
  O.getRecord "rebuild a long-running program on source change" >>= \case
    Args { projectdir = fromMaybe "." . O.unHelpful -> pd
         , watch      = O.Helpful w
         , command    = O.Helpful cmd
         , exe        = O.Helpful e }
      -> watcher cmd pd w e


-- | Watch the source folder of a long-running program for changes,
-- then recompile the program with cabal and restart if the compilation
-- succeeded.
watcher :: Text
        -- ^ command to invoke for rebuild (in project folder)
        -> FilePath
        -- ^ directory the rebuild command is executed in
        -> [FilePath]
        -- ^ files/folders to watch for source code changes
        -> Text
        -- ^ location of the program’s executable
        -> IO ()
watcher command projectDir watches exe = do
  errlog "started"
  FS.withManager $ \mgr -> do

    serverHandle <- rebuildMaybeSpawn Nothing >>= Ref.newIORef
                      :: IO (Ref.IORef (Maybe P.ProcessHandle))
    isBuilding <- Ref.newIORef False :: IO (Ref.IORef Bool)

    let
      watchArgs watch = do
        watchAbs <- makeAbsolute watch
        -- if it’s a file use the file’s folder and filter only on the file
        isDirectory <$> getFileStatus watchAbs >>= \case
          True  -> pure (              watchAbs, actionOnAllChanges filterTempFiles)
          False -> pure (takeDirectory watchAbs, actionOnAllChanges $ equalFilePath watchAbs)

      -- start a watching job (in the background)
      watchAndRebuild watchFolder changeFilter =
        FS.watchTree
          mgr          -- manager
          watchFolder  -- directory to watch
          changeFilter
          (\_ -> do
            withBuildCheck isBuilding $
              Ref.readIORef serverHandle
                -- when a file changes, build it the server and on success start it again
                >>= rebuildMaybeSpawn
                >>= Ref.writeIORef serverHandle)

    -- each --watch entry is registered
    forM_ watches $ uncurry watchAndRebuild <=< watchArgs

    -- (watchFolder, changeFilter) 
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
  where

    -- run the action for all file changes
    actionOnAllChanges action = \case
      FS.Added fp _ -> action fp
      FS.Modified fp _ -> action fp
      FS.Removed fp _ -> action fp

    -- filter out emacs `/.#foo files
    filterTempFiles :: FilePath -> Bool
    filterTempFiles fp = not $ R.match (R.makeRegex (".*/\\.#.*$"::ByteString) :: R.Regex) fp

    rebuild = do
        (_,_,_,cabP) <- P.createProcess $ (P.shell $ toS command)
                          { P.cwd = Just projectDir , P.delegate_ctlc = True }
        P.waitForProcess cabP
    -- TODO: handle Exceptions for this and other system IO
    spawn = P.spawnCommand $ toS exe
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

errlog :: Text -> IO ()
errlog = hPutStrLn stderr . toS
