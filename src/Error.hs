module Error where

import Datatypes (Conf (..))
import Control.Monad (replicateM)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Maybe (isJust, isNothing)
import Data.Int (Int)
import System.Random
import Control.Exception (IOException, catch)

checkArgs :: [String] -> Conf -> Maybe Conf
checkArgs [x] _ = Nothing
checkArgs [] (Conf numC conv inputFP)
    | isNothing numC = Nothing
    | isNothing conv = Nothing
    | isNothing inputFP = Nothing
    | otherwise = Just (Conf numC conv inputFP)
checkArgs ("-n":xs:xss) (Conf numC conv inputFP)
    | isJust (readMaybe xs :: Maybe Int) = checkArgs xss (Conf (readMaybe xs :: Maybe Int) conv inputFP)
    | otherwise = Just (Conf (Just 1) conv inputFP)
checkArgs ("-l":xs:xss) (Conf numC conv inputFP)
    | isJust (readMaybe xs :: Maybe Float) = checkArgs xss (Conf numC (readMaybe xs :: Maybe Float) inputFP)
    | otherwise = Just (Conf (Just 2) conv inputFP)
checkArgs ("-f":xs:xss) (Conf numC conv inputFP)
    | isJust (readMaybe xs :: Maybe String) = checkArgs xss (Conf numC conv (Just xs))
    | otherwise = Just (Conf (Just 3) conv inputFP)
checkArgs _ _ = Nothing

checkArgsNoError :: [String] -> Conf -> Conf
checkArgsNoError ("-n":xs:xss) (Conf numC conv inputFP) = checkArgsNoError xss (Conf (readMaybe xs :: Maybe Int) conv inputFP)
checkArgsNoError ("-l":xs:xss) (Conf numC conv inputFP) = checkArgsNoError xss (Conf numC (readMaybe xs :: Maybe Float) inputFP)
checkArgsNoError ("-f":xs:xss) (Conf numC conv inputFP) = checkArgsNoError xss (Conf numC conv (Just xs))
checkArgsNoError _ (Conf numC conv inputFP) = (Conf numC conv inputFP)

usage :: IO ()
usage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n" >>
    putStrLn "      N      number of colors in the final image" >>
    putStrLn "      L      convergence limit" >>
    putStrLn "      F      path to the file containing the colors of the pixels"

handleExits :: IOException -> IO (Maybe String)
handleExits e = return Nothing

checkFile :: Maybe String -> IO (Maybe String)
checkFile (Just path) = (Just <$> readFile path) `catch` handleExits
checkFile Nothing = exitWith $ ExitFailure 84