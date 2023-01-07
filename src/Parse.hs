module Parse where

import Control.Exception (IOException, catch, try)
import System.IO ()
import Datatypes (ColorRGB (..), Pixel (..), Pixels (..), PositionXY (..), ColorCluster (pixels))

readMaybe :: (Read a) => String -> Maybe a
readMaybe a = case reads a of
    [(x, "")] -> Just x
    _ -> Nothing

parseLine :: [String] -> Maybe Pixels 
parseLine [] = Just $ Pixels []
parseLine (x : xs) = case readMaybe x of
    Nothing -> Nothing
    Just pixel -> case parseLine xs of
        Nothing -> Nothing
        Just (Pixels pixels) -> Just $ Pixels $ pixel : pixels

getPixelsFromContent :: String -> Maybe Pixels
getPixelsFromContent content = case parseLine $ lines content of
    Just (Pixels []) -> Nothing
    a -> a