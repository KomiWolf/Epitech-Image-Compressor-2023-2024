{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- DoKMean
-}

module DoKMean (doKMean) where

import System.Random (randomRIO)
import Data.List (minimumBy)
import Data.Function (on)
import MyData
import PrintOutput

randomColor :: [Content] -> IO (Int, Int, Int)
randomColor contents = do
    index <- randomRIO (0, length contents - 1)
    return (rgb (contents !! index))

doDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
doDistance (r1, g1, b1) (r2, g2, b2) = 
    sqrt $ fromIntegral $
        (r1 - r2) * (r1 - r2) + (g1 - g2) * (g1 - g2) + (b1 - b2) * (b1 - b2)

isInOutput :: (Int, Int, Int) -> [Output] -> Bool
isInOutput _ [] = False
isInOutput val (actual:other)
    | (color actual) == val = True
    | otherwise = isInOutput val other

initClusters :: Int -> Int -> [Content] -> [Output] -> IO [Output]
initClusters cNb cTotal contents clusters
    | cNb >= cTotal = return clusters
    | otherwise = do
        randomRgb <- randomColor contents
        let colorExists = isInOutput randomRgb clusters
        if colorExists == True
            then initClusters cNb cTotal contents clusters
            else
                let new = Output { color = randomRgb, content = [] }
                in initClusters (cNb + 1) cTotal contents (clusters ++ [new])

replaceCluster :: Output -> Output -> [Output] -> [Output]
replaceCluster _ _ [] = []
replaceCluster old new (actual:other)
    | actual == old = new : other
    | otherwise = actual : replaceCluster old new other

fillClusters :: [Content] -> [Output] -> [Output]
fillClusters [] clusters = clusters
fillClusters (actual:other) clusters = 
    let distances = map (\c -> (c, doDistance (rgb actual) (color c))) clusters
        minDistanceClusterTuple = minimumBy (compare `on` snd) distances
        minDistance = fst minDistanceClusterTuple
        updated = minDistance { content = (content minDistance) ++ [actual] }
        updatedClusters = replaceCluster minDistance updated clusters
    in  fillClusters other updatedClusters

calculateMean :: [Content] -> (Int, Int, Int) -> Int -> (Int, Int, Int)
calculateMean [] (r,g,b) total = (r `div` total, g `div` total, b `div` total)
calculateMean (actual:other) (r,g,b) total =
    let (x, y, z) = rgb actual
    in calculateMean other (r + x, g + y, b + z) total

updateCentroids :: [Output] -> [Content] -> [Output] -> [Output]
updateCentroids [] fileContent new = fillClusters fileContent new
updateCentroids (actual:other) fileContent new =
    let mean = calculateMean (content actual) (0,0,0) (length (content actual))
        cluster = Output { color = mean, content = [] }
    in  updateCentroids other fileContent (new ++ [cluster])

calculateEachCentroidsDist :: [Output] -> [Output] -> [Float] -> [Float]
calculateEachCentroidsDist [] [] list = list
calculateEachCentroidsDist (_:_) [] list = list
calculateEachCentroidsDist [] (_:_) list = list
calculateEachCentroidsDist (old:olds) (new:news) list =
    let oldColor = color old
        newColor = color new
        distance = doDistance oldColor newColor
    in calculateEachCentroidsDist olds news (list ++ [distance])

checkConvergence :: [Float] -> Float -> Bool
checkConvergence [] _ = True
checkConvergence (actual:other) end
    | actual <= end = checkConvergence other end
    | otherwise = False

updateClusters :: Float -> [Output] -> [Content] -> [Output]
updateClusters end clusters fileContent =
    let updated = updateCentroids clusters fileContent []
        convergenceList = calculateEachCentroidsDist clusters updated []
    in  if checkConvergence convergenceList end == True
            then clusters
            else updateClusters end updated fileContent

doKMean :: Conf -> [Content] -> IO ()
doKMean conf fileContent = do
    let clustersNb = (toVar (nColor conf) 0)
    clusters <- initClusters 0 clustersNb fileContent []
    let end = (toVar (convergence conf) 0.0)
    let updatedClusters = fillClusters fileContent clusters
    let finalClusters = updateClusters end updatedClusters fileContent
    printOutput finalClusters
