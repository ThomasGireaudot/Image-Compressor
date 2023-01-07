module ImageCompressor where

import Datatypes (Conf (..), CompressionType (..), Pixels (..), CompressedImage (..), Pixel (..), ColorCluster (..), ColorRGB (..))
import Error (checkFile)
import Parse (getPixelsFromContent)
import Clusters (generateClusters, putPixelsInClusters, nextClusters, isConvergenceLimitReached)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.Random (StdGen (..))

printClusters :: [ColorCluster] -> IO ()
printClusters [x] = putStrLn $ show x
printClusters (x:xs) = (putStrLn $ show x) >> printClusters xs

endImageCompressor :: [Pixel] -> [ColorCluster] -> IO ()
endImageCompressor pixels emptyClusters = putStr $ show (CompressedImage filledClusters)
    where
        filledClusters = putPixelsInClusters emptyClusters pixels

runImageCompressor :: Float -> Pixels -> [ColorCluster] -> IO ()
runImageCompressor convergenceLimit (Pixels pixels) clusters =  if convergenceCheck == True
    then endImageCompressor pixels clustersStepTwo
    else runImageCompressor convergenceLimit (Pixels pixels) clustersStepTwo
    where
        clustersStepOne = putPixelsInClusters clusters pixels
        clustersStepTwo = nextClusters clustersStepOne
        convergenceCheck = isConvergenceLimitReached convergenceLimit clustersStepOne clustersStepTwo

prepareImageCompressor :: Conf -> String -> IO ()
prepareImageCompressor _ [] = exitWith $ ExitFailure 84
prepareImageCompressor (Conf (Just color) (Just convergence) path) content =
    case getPixelsFromContent content of
        Just pixels -> runImageCompressor convergence pixels $ generateClusters color pixels
        Nothing -> exitWith $ ExitFailure 84

checkImageCompressor :: Conf -> IO ()
checkImageCompressor (Conf color convergence path) = do
    content <- checkFile path
    case content of
        Just content -> prepareImageCompressor (Conf color convergence path) content
        Nothing -> exitWith $ ExitFailure 84