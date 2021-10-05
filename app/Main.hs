{-# LANGUAGE InstanceSigs, FlexibleInstances, TypeSynonymInstances, DeriveFunctor #-}

module Main where

import System.IO 
import Data.Char
import Data.List
import Data.Functor 
import Control.Applicative
import Control.Monad 
import Data.Array 
import qualified Data.List.Safe as Safe

data ND a = Var a | Choice (ND a) (ND a) | Fail deriving Functor

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

(<***>) :: ND a -> ND a -> ND a 
(<***>) = Choice 

handle :: ND a -> [a]
handle Fail = [] 
handle (Var x) = return x
handle (Choice x y) = handle x ++ handle y

once :: ND a -> a
once = head . handle 

type Game = Array (Int, Int) (Maybe Int)

{--
instance Show Game where
    show :: Game -> String
    show gm = show gm
        where 
            printGame :: Game -> String 
            printGame  = . assocs
            printRow  :: Array i Int -> String 
            printRow = foldr (\x xs -> maybe "." show x ++ xs) "" . assocs 
--}
instance Read Game where 
    readsPrec _ value = [(tryParse 9 value, "")]
        where 
            tryParse :: Int -> String -> Game
            tryParse 0 input = listToGame (columnToList input 0)

            listToGame :: [(Int, [(Int,Int)])] -> Game 
            listToGame = array (0, 9) . intermed
                where 
                    intermed :: [(Int, [(Int, Int)])] -> [(Int, Array Int Int)]
                    intermed = map (fmap (array (0,9)))

            parseRow :: String -> Array Int Int 
            parseRow str = array (0, 9) $ rowToList str 0

            columnToList :: String -> Int -> [(Int, [(Int,Int)])]
            columnToList str n = (n, rowToList (take 9 str) 0) : columnToList (drop 9 str) (n+1)

            rowToList :: String -> Int -> [(Int,Int)]
            rowTolist [] _ = [] 
            rowToList (x:xs) n = maybe rest (\x -> (n + 1, x):rest) (readGameValue x) 
                where
                    rest = rowToList xs (n + 1)

            readGameValue :: Char -> Maybe Int 
            readGameValue ch = 
                case ch of {
                    '.' -> Nothing;
                    _ -> Just (read [ch] :: Int) 
                }

listToND :: [Int] -> ND Int
listToND = foldr ((<***>) . return ) Fail

removeMaybes :: [Maybe a] -> [a]
removeMaybes = map (maybe 0 id) . filter (maybe False (\_ -> True))

submatrix :: Game -> Int -> Int -> Int -> Int -> [Int]
submatrix gm startR endR startC endC = removeMaybes [v | ((i,j),v) <- assocs gm, startR <= i, i < endR, startC <= j, j < endC]

getSquareNumbers :: Game -> Int -> Int -> [Int]
getSquareNumbers gm i j = 
                    let startRow = (i `div` 3)
                        endRow   = startRow + 3
                        startCol =  (j `div` 3)
                        endCol   = startCol + 3
                    in  
                        submatrix gm startRow endRow startCol endCol

getFreeNumbers :: [Int] -> [Int] -> [Int] -> ND Int 
getFreeNumbers square row col = listToND ((square `union` row `union` col) `intersect` [1..9])

getNColumn :: Game -> Int -> [Maybe Int]
getNColumn gm n = [gm ! (i, n) | i <- [1..9]]

getNColumnNoMaybes :: Game -> Int -> [Int]
getNColumnNoMaybes gm n = removeMaybes $ getNColumn gm n

getFirstColumn :: Game -> [Maybe Int]
getFirstColumn = flip getNColumn 0

getNRow :: Game -> Int -> [Maybe Int]
getNRow gm n = [gm ! (n, i) | i <- [1..9]]

getNRowNoMaybes :: Game -> Int -> [Int]
getNRowNoMaybes gm n = removeMaybes $ getNRow gm n

getFirstRow :: Game -> [Maybe Int]
getFirstRow = flip getNRow 0

getEmptyCell :: Game -> Maybe (Int, Int)
getEmptyCell = foldr (\x xs -> Just x) Nothing . indices

updateGame :: Game -> Int -> Int -> Int -> Game
updateGame = undefined

solutions :: Game -> ND Game 
solutions gm = 
    case getEmptyCell gm of 
        Nothing -> do return gm
        Just (i,j) -> do
                num <- getFreeNumbers (getSquareNumbers gm i j) (getNColumnNoMaybes gm j) (getNRowNoMaybes gm i)
                let gm' = updateGame gm i j num
                return gm

loadGame :: FilePath -> IO Game
loadGame file = do { 
    contents <- readFile file;
    return . read $ contents
}

main :: IO ()
main =  do {
    file <- openFile "Game.txt" ReadMode;
    contents <- hGetContents file ;
    print (read contents :: Game);
    hClose file
}