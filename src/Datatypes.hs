module Datatypes where

import Control.Monad (replicateM)
import Data.Bool (Bool)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.Exit (exitWith, ExitCode (ExitFailure))
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Int (Int)

data Conf = Conf {
    numColor :: Maybe Int,
    convergence :: Maybe Float,
    inputFilePath :: Maybe String
}

defaultConf :: Conf
defaultConf = Conf Nothing Nothing Nothing

-- instance Show Conf where
--     show (Conf numColor convergence inputFilePath)
--         | isNothing numColor = "color is nothing"
--         | isNothing convergence = "convergence is nothing"
--         | isNothing inputFilePath = "filepath is nothing"
--         | otherwise = "color: " ++ show $ fromJust numColor ++ "\nconvergence: " ++ show $ fromJust convergence ++ "\nfilepath: " ++ fromJust inputFilePath ++ "\n"

data PositionXY = PositionXY {
    x :: Int,
    y :: Int
}

instance Show PositionXY where
    show (PositionXY x y) = "(" ++ show x ++ "," ++ show y ++ ")"

data ColorRGB = ColorRGB {
    r :: Int,
    g :: Int,
    b :: Int
}

instance Show ColorRGB where
    show (ColorRGB r g b) = "(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

instance Eq ColorRGB where
    (==) (ColorRGB r1 g1 b1) (ColorRGB r2 g2 b2)
        | r1 /= r2 = False
        | g1 /= g2 = False
        | b1 /= b2 = False
        | otherwise = True

data Pixel = Pixel {
    position :: PositionXY,
    pixelColor :: ColorRGB
}

instance Show Pixel where
    show (Pixel pos color) = show pos ++ " " ++ show color

spaceToComma :: Char -> Char
spaceToComma ' ' = ','
spaceToComma c = c

formatPixelRead :: String -> String
formatPixelRead str = '(' : withComma ++ ")"
  where
    withComma = map spaceToComma str

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c

instance Read Pixel where
  readsPrec _ r = [(uncurry Pixel (uncurry PositionXY pos, uncurry3 ColorRGB col), "")]
    where
      (pos, col) =
        read (formatPixelRead r) :: ((Int, Int), (Int, Int, Int))

newtype Pixels = Pixels {pixel :: [Pixel]}

instance Show Pixels where
    show (Pixels []) = ""
    show (Pixels [pixel]) = show pixel
    show (Pixels (pixel : pixels)) = show pixel ++ "\n" ++ show (Pixels pixels)

data ColorCluster = ColorCluster {
    clusterColor :: ColorRGB,
    pixels :: [Pixel],
    pixelsNumber :: Int
}

instance Show ColorCluster where
    show (ColorCluster color [] _) = "--\n" ++ show color ++ "\n-"
    show (ColorCluster color pixels _) = "--\n" ++ show color ++ "\n-\n" ++ show (Pixels pixels) ++ "\n"

data CompressionType = CompressionType {
    numberOfClusters :: Int,
    convergenceLimit :: Float
}

newtype CompressedImage = CompressedImage {clusters :: [ColorCluster]}

instance Show CompressedImage where
    show (CompressedImage []) = ""
    show (CompressedImage [cluster]) = show cluster
    show (CompressedImage (cluster : clusters)) = show cluster ++ "\n" ++ show (CompressedImage clusters)