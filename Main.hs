{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where




import System.IO
import Data.IORef
import System.Environment
import Data.Typeable
import Data.List

main :: IO ()
main =
  choiceLoop Empty


choiceLoop :: BinaryTree Int -> IO r
choiceLoop tree = do

  putStrLn "[][][][][][][][][][][][][][][][][]"
  putStrLn "type 1, for insert"
  putStrLn "type 2 for search"
  putStrLn "type 3 to print out the tree, ordered."
  putStrLn "type 4 to insert a file into the tree"
  putStrLn "type 5 to make a new empty tree"
  putStrLn "[][][][][][][][][][][][][][][][][]"

  x <- getLine
  let choice = read(x) :: Int
  if choice == 1 then do
    insertChoice tree
  else if choice == 2 then do
    searchChoice tree
  else if choice == 3 then do
    putStrLn(show(treePrint tree []))
    choiceLoop tree
  else if choice == 4 then do
    insertFromFileChoice tree
  else if choice == 5 then do
    choiceLoop Empty
  else
    choiceLoop tree


-- choices below
searchChoice :: BinaryTree Int -> IO r
searchChoice tree = do
  putStrLn "type the number you wish to search for"
  x <- getLine
  let number = read(x)
  (treeSearch tree number) >>= \case
    True -> putStrLn ("-------------" ++ show(number) ++ " Is in the tree ---------------------")
    False -> putStrLn ("------------" ++ show(number) ++ " Is not in the tree-------------------")
  choiceLoop tree

insertChoice :: BinaryTree Int -> IO r
insertChoice tree = do
  putStrLn "type the number you wish to insert!"
  x <- getLine
  let number = read(x)
  let newTree = treeInsert tree number
  choiceLoop newTree

--insertFromFileChoice :: Ord d => BinaryTree d -> BinaryTree d
insertFromFileChoice :: BinaryTree Int -> IO r
insertFromFileChoice tree = do
  putStrLn "type the name of the file that you wish to insert to the tree"
  filename <- getLine
  withFile filename  ReadMode (\handle -> do
    contents <- hGetContents handle
    let numberLines = lines contents
    let intList = map read $ words contents :: [Int]
    putStrLn(show( intList) ++ " has been inserted into tree !!!!!!!!!!")
    choiceLoop (treeBuilder tree intList)
    )





  --treeSearch treebuild, show orderedTree and treeinsert
class TreeFunc d where
  search :: d -> BinaryTree d
  insert :: d -> BinaryTree d

data BinaryTree d = Empty | Node d (BinaryTree d) (BinaryTree d) deriving (Show)

treeBuilder :: Ord d => BinaryTree d -> [d] -> BinaryTree d
treeBuilder tree lst = foldl treeInsert tree lst

treeSearch :: Ord d => BinaryTree d -> d -> IO Bool
treeSearch Empty val = return False
treeSearch (Node nodeVal left right) val =
  if(nodeVal == val) then
    return True
  else if(val < nodeVal) then
    treeSearch left val
  else
    treeSearch right val

treeInsert :: (Ord d) => BinaryTree d -> d -> BinaryTree d
treeInsert Empty val = Node val Empty Empty
treeInsert (Node nodeVal left right) val =
  if (val < nodeVal) then
      Node nodeVal (treeInsert left val) right
  else
      Node nodeVal left (treeInsert right val)


treePrint :: Ord d => BinaryTree d -> [d] -> [d]
treePrint Empty lst = lst
treePrint (Node nodeVal left right) lst = do
  let lst1 = treePrint left lst
  let lst2 = lst1 ++ [nodeVal]
  treePrint right lst2



