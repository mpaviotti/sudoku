{-# LANGUAGE InstanceSigs,  FlexibleInstances, TypeSynonymInstances, DeriveFunctor #-}

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

type Game = Array (Int, Int) (Maybe Int)

instance {-# OVERLAPPING #-} Read Game where 
    readsPrec _ value = [(tryParse value, "")]
        where 
            tryParse :: String -> Game
            tryParse input = listToGame input 

            parseRow :: String -> [Maybe Int]
            parseRow = foldr (\ch xs -> (readGameValue ch):xs) []


            listToGame :: String -> Game
            listToGame input = array ((0,0), (9,9)) $ parse input 9


            indexRow :: Int -> [Maybe Int] -> [((Int,Int), Maybe Int)]
            indexRow row xs = aux row xs 0
                where   
                    aux row [] _ = []
                    aux row (x:xs) n = ((row, n), x) : aux row xs (n+1)

            parse :: String -> Int -> [((Int, Int), Maybe Int)]
            parse str 0 = indexRow 0 (parseRow (take 9 str))
            parse str n = indexRow n (parseRow (take 9 str) ++ (parse (drop 10 str) (n-1))

            readGameValue :: Char -> Maybe Int 
            readGameValue ch = 
                case ch of {
                    '.' -> Nothing;
                    _ -> Just (read [ch] :: Int) 
                }


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

removeMaybes :: Num a => [Maybe a] -> [a]
removeMaybes = map (maybe 0 id) . filter (maybe False (const True))

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

loadGame :: FilePath -> IO Game
loadGame file = do {
    contents <- readFile file;
    return . read $ contents
}

solutions :: Game -> ND Game 
solutions gm = 
    case getEmptyCell gm of 
        Nothing -> do return gm
        Just (i,j) -> do
                num <- getFreeNumbers (getSquareNumbers gm i j) (getNColumnNoMaybes gm j) (getNRowNoMaybes gm i)
                let gm' = updateGame gm i j num
                return gm

main :: IO ()
main =  do {
    file <- openFile "Game.txt" ReadMode;
    contents <- hGetContents file ;
    print contents ;
    print (read contents :: Game);
    hClose file
}