{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (Right, Left)

import Control.Monad (liftM)
import Data.List     (elemIndices, transpose)
import System.IO     (BufferMode(..), hSetBuffering, stdin)
import System.Random (randomRIO)


-- Each inner list is a row, starting with the top row
-- A 0 is an empty tile
type Board = [[Int]]

data Direction = Up | Right | Down | Left

charToDirection :: Char -> Maybe Direction
charToDirection 'w' = Just Up
charToDirection 'd' = Just Right
charToDirection 's' = Just Down
charToDirection 'a' = Just Left
charToDirection _   = Nothing

slideLeft :: Board -> Board
slideLeft = map slideRow
  where slideRow [] = []
        slideRow [x] = [x]
        slideRow (x:y:zs)
          | x == 0 = slideRow (y : zs) ++ [0]
          | y == 0 = slideRow (x : zs) ++ [0] -- So that things will combine when 0's are between them
          | x == y = (x + y) : slideRow zs ++ [0]
          | otherwise = x : slideRow (y : zs)

slide :: Direction -> Board -> Board
slide Up    = transpose . slideLeft . transpose
slide Left  = slideLeft
slide Right = mirror . slideLeft . mirror
slide Down  = transpose . mirror . slideLeft . mirror . transpose

mirror :: Board -> Board
mirror = map reverse

-- Tells us if the game is over because there are no valid moves left
stalled :: Board -> Bool
stalled b = all stalled' b && all stalled' (transpose b)
  where stalled' row = notElem 0 row && noNeighbors row
        noNeighbors [ ] = True
        noNeighbors [_] = True
        noNeighbors (x:y:zs)
          | x == y = False
          | otherwise = noNeighbors (y:zs)

-- Returns tuples of the indices of all of the empty tiles
emptyTiles :: Board -> [(Int, Int)]
emptyTiles = concatMap (uncurry search) . zip [0..3]
  where search n = zip (replicate 4 n) . elemIndices 0

-- Given a point, update replaces the value at the point on the board with the given value
updateTile :: (Int, Int) -> Int -> Board -> Board
updateTile (rowI, columnI) value = updateIndex (updateIndex (const value) columnI) rowI
  where updateIndex fn i list = take i list ++ fn (head $ drop i list) : tail (drop i list)

-- Returns a new value for adding to the board
pickNewValue :: IO Int
pickNewValue = liftM (\x -> if x == 1 then 4 else 2) $ randomRIO (1, 10 :: Int)

-- Picks an empty tile to place the new one onto
pickNewTileIndex :: Board -> IO (Int, Int)
pickNewTileIndex b = liftM (tiles !!) $ randomRIO (0, length tiles - 1)
  where tiles = emptyTiles b

-- Adds a tile to a random empty spot.
-- 90% of the time the tile is a 2, 10% of the time it is a 4
addTile :: Board -> IO Board
addTile b = do
    index <- pickNewTileIndex b
    value <- pickNewValue
    return $ updateTile index value b

-- Our main game loop
gameloop :: Board -> IO ()
gameloop b = do
    putStrLn $ boardToString b
    input <- getChar
    putStrLn ""
    let b' = maybe b (`slide` b) $ charToDirection input
    if stalled b'
        then print "Game over."
        else if b' == b -- Only add a new tile if we made a change when sliding
            then gameloop b'
            else addTile b' >>= gameloop

-- Board pretty printing
boardToString :: Board -> String
boardToString = unlines . map show

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    let row = [0,0,0,0]
    let board = [row,row,row,row]
    b1 <- addTile board
    b2 <- addTile b1 -- Add two tiles randomly and start the game!
    gameloop b2
