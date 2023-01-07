module Main where

import Error (checkArgs, usage, checkArgsNoError)
import Datatypes (defaultConf)
import ImageCompressor (checkImageCompressor)
import Control.Monad ()
import Data.Bool ()
import Data.Int ()
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
    args <- getArgs
    checkImageCompressor $ checkArgsNoError args defaultConf
    -- putStr $ show $ checkArgs args defaultConf
    -- case checkArgs args defaultConf of
    --     Nothing -> usage >> exitWith (ExitFailure 84)
    --     Just conf -> checkImageCompressor conf