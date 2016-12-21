{-# LANGUAGE RecordWildCards #-}
import Data.List
import Control.Concurrent
import Control.Monad
import qualified Data.Time.Clock as Cl
import System.Environment
import System.Process
import System.Exit

import Options.Applicative

data Args = Args
  { argExit    :: Int
  , argDelay   :: Double
  , argCommand :: [String] }

args :: Parser Args
args = Args
  <$> option auto (mconcat
    [ short 'e'
    , long  "exit"
    , help  "exitcode on which until should succeed"
    , value 0
    , metavar "<exit code>" ])
  <*> option (auto >>= \i -> if (i < 0)
               then readerAbort (ErrorMsg "delay must be positive")
               else pure i) (mconcat
    [ short 'd'
    , long  "delay"
    , help  "delay between retries, in seconds"
    , value 1
    , metavar "<positive double>" ])
  <*> some (argument str (mconcat
    [ help  "command to retry"
    , metavar "<command>" ]))


main :: IO ()
main = execParser p >>= retry
  where
    p = info (helper <*> args) (mconcat
          [ fullDesc
          , progDesc "rerun command until it succeeds" ])

retry :: Args -> IO ()
retry Args{..} = go
  where
    go = do
      (_,_,_,ph) <-createProcess $ shell (intercalate " " argCommand)
      ex <- ex2int <$> waitForProcess ph
      when (ex /= argExit) $ do
        threadDelay . round $ argDelay * 1e6
        go
    ex2int ExitSuccess = 0
    ex2int (ExitFailure i) = i
