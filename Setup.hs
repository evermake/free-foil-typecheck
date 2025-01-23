{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

-- Source: https://github.com/haskell/cabal/issues/6726#issuecomment-918663262

-- | Custom Setup that runs bnfc to generate the language sub-libraries
-- for the parsers included in Ogma.
module Main (main) where

import Data.List (intercalate)
import Distribution.Simple (defaultMainWithHooks, hookedPrograms, postConf, preBuild, simpleUserHooks)
import Distribution.Simple.Program (Program (..), findProgramVersion, simpleProgram)
import PyF (fmt)
import System.Exit (ExitCode (..))
import System.Process (callCommand)

-- | Run BNFC, happy, and alex on the grammar before the actual build step.
--
-- All options for bnfc are hard-coded here.
main :: IO ()
main =
  defaultMainWithHooks $
    simpleUserHooks
      { hookedPrograms = [bnfcProgram]
      , postConf = \args flags packageDesc localBuildInfo -> do
          let
            isWindows =
#ifdef mingw32_HOST_OS
                      True
#else
                      False
#endif
            -- See the details on the command in https://github.com/objectionary/normalizer/issues/347#issuecomment-2117097070
            command = intercalate "; " $
                [ "set -ex" ] <>
                [ "chcp.com" | isWindows ] <>
                [ "chcp.com 65001" | isWindows ] <>
                [ "cp grammar/hindley-milner.cf grammar/Parser.cf"  -- Workaround to customize generated package name
                , "bnfc --haskell -d -p FreeFoilTypecheck.HindleyMilner --generic -o src grammar/Parser.cf"
                , "rm grammar/Parser.cf"
                , "cd src/FreeFoilTypecheck/HindleyMilner/Parser"
                , "alex Lex.x"
                , "happy Par.y"
                , "true"
                , "cd ../../../.."
                ] <>
                [ "cp grammar/system-f.cf grammar/Parser.cf"  -- Workaround to customize generated package name
                , "bnfc --haskell -d -p FreeFoilTypecheck.SystemF --generic -o src grammar/Parser.cf"
                , "rm grammar/Parser.cf"
                , "cd src/FreeFoilTypecheck/SystemF/Parser"
                , "alex Lex.x"
                , "happy Par.y"
                , "true"
                , "cd ../../../.."
                ]

            fullCommand = [fmt|bash -c ' {command} '|]

          putStrLn fullCommand

          _ <- callCommand fullCommand

          postConf simpleUserHooks args flags packageDesc localBuildInfo
      }

-- | NOTE: This should be in Cabal.Distribution.Simple.Program.Builtin.
bnfcProgram :: Program
bnfcProgram =
  (simpleProgram "bnfc")
    { programFindVersion = findProgramVersion "--version" id
    }
