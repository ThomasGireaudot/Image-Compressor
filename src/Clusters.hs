module Clusters where

import Datatypes (ColorCluster (..), ColorRGB (..), Pixel (..), Pixels (..))

compareColorToList :: ColorRGB -> [ColorRGB] -> Bool
compareColorToList _ [] = False
compareColorToList color (x:xs)
    | color == x = True
    | otherwise = compareColorToList color xs

generateColorSet :: Int -> [Pixel] -> [ColorRGB] -> [ColorRGB]
generateColorSet 0 _ _ = []
generateColorSet _ [] _ = []
generateColorSet numberOfCluster ((Pixel _ pixelColor):xs) [(ColorRGB (-1) (-1) (-1))] = pixelColor : generateColorSet (numberOfCluster - 1) xs [pixelColor]
generateColorSet numberOfCluster ((Pixel _ pixelColor):xs) previousSelection = if (compareColorToList pixelColor previousSelection) == True
    then generateColorSet numberOfCluster xs previousSelection
    else pixelColor : generateColorSet (numberOfCluster - 1) xs (previousSelection ++ [pixelColor])

makeClusters :: [ColorRGB] -> [ColorCluster]
makeClusters [] = []
makeClusters (color:xs) = (ColorCluster color [] 0) : makeClusters xs

generateClusters :: Int -> Pixels -> [ColorCluster]
generateClusters numberOfCluster (Pixels pixels) = makeClusters $ generateColorSet numberOfCluster pixels [(ColorRGB (-1) (-1) (-1))]

insertPixelInList :: [Pixel] -> Pixel -> [Pixel]
insertPixelInList xs pixel = foldr (:) [pixel] xs

distanceBetweenColors :: ColorRGB -> ColorRGB -> Float
distanceBetweenColors (ColorRGB rA gA bA) (ColorRGB rB gB bB) = (r * r) + (g * g) + (b * b)
    where
        r = fromIntegral (rA - rB)
        g = fromIntegral (gA - gB)
        b = fromIntegral (bA - bB)

putOnePixelInCluster :: ColorCluster -> [ColorCluster] -> Pixel -> [ColorCluster]
putOnePixelInCluster (ColorCluster color pixels pixelsNumber) [] pixelToPlace = [ColorCluster color pixelList pixelsNumber]
    where pixelList = insertPixelInList pixels pixelToPlace
putOnePixelInCluster (ColorCluster chosenColor chosenPixels chosenPixelsNumber) ((ColorCluster clusterColor clusterPixels clusterPixelsNumber):xs) (Pixel position pixelColor)
    | distanceBetweenColors pixelColor chosenColor > distanceBetweenColors pixelColor clusterColor = chosenCluster : putOnePixelInCluster currentCluster xs pixel
    | distanceBetweenColors pixelColor chosenColor < distanceBetweenColors pixelColor clusterColor = currentCluster : putOnePixelInCluster chosenCluster xs pixel
        where
            currentCluster = ColorCluster clusterColor clusterPixels clusterPixelsNumber
            chosenCluster = ColorCluster chosenColor chosenPixels chosenPixelsNumber
            pixel = Pixel position pixelColor
putOnePixelInCluster _ _ _ = []

putPixelsInClusters :: [ColorCluster] -> [Pixel] -> [ColorCluster]
putPixelsInClusters clusters [] = clusters
putPixelsInClusters (y:ys) (x:xs) = putPixelsInClusters newClusters xs
    where
        newClusters = putOnePixelInCluster y ys x
putPixelsInClusters _ _ = []

total :: [Pixel] -> Int -> Int
total [] _ = 0
total [(Pixel _ (ColorRGB r g b))] rgbIndex
    | rgbIndex == 1 = r
    | rgbIndex == 2 = g
    | rgbIndex == 3 = b
    | otherwise = 0
total ((Pixel _ (ColorRGB r g b)):xs) rgbIndex
    | rgbIndex == 1 = r + total xs 1
    | rgbIndex == 2 = g + total xs 2
    | rgbIndex == 3 = b + total xs 3
    | otherwise = 0

averageFromPixelList :: [Pixel] -> ColorRGB
averageFromPixelList pixels = (ColorRGB ((total pixels 1) `div` listLength) ((total pixels 2) `div` listLength) ((total pixels 3) `div` listLength))
    where
        listLength = length pixels

moveCluster :: ColorCluster -> ColorCluster
moveCluster (ColorCluster color pixels pixelsNumber) = (ColorCluster (averageFromPixelList pixels) [] 0)

nextClusters :: [ColorCluster] -> [ColorCluster]
nextClusters [] = []
nextClusters [x] = [moveCluster x]
nextClusters (x:xs) = moveCluster x : nextClusters xs

calculateConvergence :: ColorCluster -> ColorCluster -> Float
calculateConvergence (ColorCluster oldColor _ _) (ColorCluster newColor _ _) = distanceBetweenColors oldColor newColor

isConvergenceLimitReached :: Float -> [ColorCluster] -> [ColorCluster] -> Bool
isConvergenceLimitReached _ [] [] = True
isConvergenceLimitReached convLimit (oldCluster:xs) (newCluster:ys) = if (calculateConvergence oldCluster newCluster) < convLimit
    then isConvergenceLimitReached convLimit xs ys
    else False