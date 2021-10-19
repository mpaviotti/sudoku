{-# LANGUAGE InstanceSigs,  FlexibleInstances, DeriveFunctor #-}

module Main where

import System.IO 
import Data.Char
import Data.List
import Data.Functor 
import Control.Applicative
import Control.Monad 
import Data.Array 
import Data.Maybe
import qualified Data.List.Safe as Safe
import Debug.Trace

data ND a = Var a | Choice (ND a) (ND a) | Fail deriving Functor

type Game = Array (Int, Int) (Maybe Int)

instance {-# OVERLAPPING #-} Read Game where 
    readsPrec _ value = [(tryParse value, "")]
        where 
            tryParse :: String -> Game
            tryParse input = listToGame input 

            parseRow :: String -> [Maybe Int]
            parseRow = foldr (\ch xs -> (readGameValue ch):xs) []

            listToGame :: String -> Game
            listToGame input = array ((0,0), (8, 8)) $ parse input 9

            indexRow :: Int -> [Maybe Int] -> [((Int,Int), Maybe Int)]
            indexRow row xs = aux row xs 0
                where
                    aux row [] _ = []
                    aux row (x:xs) n = ((row, n), x) : aux row xs (n+1)

            parse :: String -> Int -> [((Int, Int), Maybe Int)]
            parse str 0 = indexRow 9 (parseRow (take 9 str))
            parse str n = indexRow (9-n) (parseRow (take 9 str)) ++ parse (drop 10 str) (n-1)

            readGameValue :: Char -> Maybe Int 
            readGameValue ch = 
                case ch of {
                    '.' -> Nothing;
                    _ -> Just (read [ch] :: Int) 
                }

instance {-# OVERLAPPING #-} Show Game where
  show :: Game -> String
  show gm = prettify [maybe '.' (head . show) (gm ! (i, j)) | i <- [0..8], j <- [0..8]]
    where
      prettify :: String -> String
      prettify =  addSpaces . addNewLines . addHorizontalLines

      addHorizontalLines :: String -> String
      addHorizontalLines [] = []
      addHorizontalLines str = let (x,y) = splitAt 27 str in
                                x ++ replicate 9 '-' ++ addHorizontalLines y

      addNewLines :: String -> String
      addNewLines [] = []
      addNewLines str = let (x,y) = splitAt 9 str;
                            (x1, x') = splitAt 3 x;
                            (x2, x3) = splitAt 3 x'
                        in
                        x1 ++ "|" ++ x2 ++ "|" ++ x3 ++ ['\n'] ++ addNewLines (drop 9 str)

      addSpaces :: String -> String
      addSpaces = foldr (\ch xs -> " " ++ [ch] ++ xs) []

instance Applicative ND where 
    pure :: a -> ND a
    pure = Var
    (<*>) :: ND (a -> b) -> ND a -> ND b 
    (Var f) <*> (Var x) = Var . f $ x
    (Choice f g) <*> c = Choice (f <*> c) (g <*> c)

instance Monad ND where
    return :: a -> ND a
    return = pure 
    (>>=) :: ND a -> (a -> ND b) -> ND b
    (Var x) >>= f = f x 
    Fail >>= f = Fail 
    (Choice x y) >>= f = Choice (x >>= f) (y >>= f)

instance Show a => Show (ND a) where
  show :: ND a -> String
  show = show . handleList

handle :: (b -> b -> b) -> (a -> b) -> b -> ND a -> b
handle f g z (Var x) = g x
handle f g z (Choice x y) = f (handle f g z x) (handle f g z y)
handle f g z Fail = z
 
handleList :: ND a -> [a]
handleList = handle (++) (:[]) []

once :: ND a -> a
once = head . handleList

(<***>) :: ND a -> ND a -> ND a
x <***> y = Choice x y

listToND :: [Int] -> ND Int
listToND = foldr ((<***>) . return ) Fail

removeMaybes :: [Maybe a] -> [a]
removeMaybes = map (fromMaybe undefined) . filter isJust

submatrix :: Game -> Int -> Int -> Int -> Int -> [Int]
submatrix gm startR endR startC endC = removeMaybes [v | ((i,j),v) <- assocs gm, startR <= i, i < endR, startC <= j, j < endC]

getSquareNumbers :: Game -> Int -> Int -> [Int]
getSquareNumbers gm i j = 
                    let startRow = (i `div` 3) * 3
                        endRow   = startRow + 3
                        startCol =  (j `div` 3) * 3
                        endCol   = startCol + 3
                    in  
                        submatrix gm startRow endRow startCol endCol

removeFromList :: [Int] -> [Int] -> [Int]
removeFromList [] _ = []
removeFromList (x:xs) ys = let reccall = removeFromList xs ys in
                                 if x `elem` ys then reccall else x:reccall

getFreeNumbers :: [Int] -> [Int] -> [Int] -> ND Int 
getFreeNumbers square row col = listToND (removeFromList [1..9] (square `union` row `union` col))

getNColumn :: Game -> Int -> [Maybe Int]
getNColumn gm n = [gm ! (i, n) | i <- [0..8]]

getNColumnNoMaybes :: Game -> Int -> [Int]
getNColumnNoMaybes gm n = removeMaybes $ getNColumn gm n

getFirstColumn :: Game -> [Maybe Int]
getFirstColumn = flip getNColumn 0

getNRow :: Game -> Int -> [Maybe Int]
getNRow gm n = [gm ! (n, i) | i <- [0..8]]

getNRowNoMaybes :: Game -> Int -> [Int]
getNRowNoMaybes gm n = removeMaybes $ getNRow gm n

getFirstRow :: Game -> [Maybe Int]
getFirstRow = flip getNRow 0

getEmptyCell :: Game -> Maybe (Int, Int)
getEmptyCell gm = listToMaybe [ i | (i, Nothing) <- assocs gm ]

loadGame :: FilePath -> IO Game
loadGame file = do {
    contents <- readFile file;
    return . read $ contents
}

solutions :: Game -> ND Game 
solutions gm = case getEmptyCell gm of 
                    Nothing -> do return gm
                    Just (i,j) -> do
                        let squareNumbers = getSquareNumbers gm i j;
                            colNumbers = getNColumnNoMaybes gm j;
                            rowNumbers = getNRowNoMaybes gm i;
                            free = getFreeNumbers (getSquareNumbers gm i j) (getNColumnNoMaybes gm j) (getNRowNoMaybes gm i)
                        num <-free
                        -- trace ("Free Numbers: " ++ show free ++ "\nSquare Numbers : " ++ show squareNumbers ++ "\nColumn Numbers : " ++ show colNumbers ++ "\nRow Numbers : " ++ show rowNumbers)
                        solutions (gm // [((i,j), Just num)])
                        --trace ("Free cell and value " ++ show ((i,j), num) ++ "\n" ++ show gm)  (solutions (gm // [((i,j), Just num)]))

main :: IO ()
main =  do
    file <- openFile "Game.txt" ReadMode
    contents <- hGetContents file
    let game = (read contents :: Game)
    print "Problem:\n"
    print game
    print "Solution:\n"
    print (head . handleList . solutions $ game)
    hClose file
