{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

-}

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import Control.Monad.Trans.State.Lazy
import System.Environment
import System.IO.Unsafe
import Data.List
import ApocTools
import ApocStrategyHuman
import Data.Char
import ApocBreakUp



---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = do 
  main' (unsafePerformIO getArgs)

main':: [String] -> IO()
main' [] = do interactive
main' (black:white:comments) = do checkUserInput black white
main' incorrect = do putStrLn "1.Human\n 2.Greedy\n 3.Safe\n" 

interactive :: IO()
interactive = do
  putStrLn ("Please choose Human or computer\n" ++ " 1.Human\n 2.Greedy\n 3.Safe\n")
  putStrLn ("Enter Black Choice")
  black <- getLine
  putStrLn ("Enter White Choice")
  white <- getLine
  checkUserInput black white


checkUserInput :: String -> String -> IO()
checkUserInput black white = do
  let blackCheck = check black
  let whiteCheck = check white
  if (isNothing blackCheck) || (isNothing whiteCheck)
    then putStrLn ("1.Human\n 2.Greedy\n 3.Safe\n" )
    -- else putStrLn whiteCheck
    else startGame (fst (fromJust blackCheck)) (snd (fromJust whiteCheck)) (fst (fromJust whiteCheck)) (snd(fromJust whiteCheck))


startGame :: Chooser -> String -> Chooser -> String -> IO()
startGame black blackString white whiteString = do
  putStrLn $ "\nThe initial board:"
  print initBoard
  letsPlay initBoard black blackString white whiteString

    
check :: String -> Maybe (Chooser, String)
check stringName | (stringName == "human") = Just (human, stringName)
                 | otherwise = Nothing


validate :: Maybe[(Int,Int)] -> Player -> GameState -> Bool
validate Nothing player board = True --check validity this check is for pass
validate move player board = if boundCheck move then
                               if currentSpot == E || (player == White && (currentSpot == BK || currentSpot ==BP ))|| (player == Black && (currentSpot==WK || currentSpot==WP))
                                  then False
                                    else if (currentSpot == WP || currentSpot ==BP) then pawnMove player move board else knightMove move player board
                             else False
                             where currentSpot = (getFromBoard(theBoard board) ((fromJust move)!!0))


boundCheck :: Maybe[(Int,Int)] -> Bool
boundCheck moveToCheck
 | x1 >4 = False
 | y1 >4 = False
 | x2 >4 = False
 | y2 >4 = False
 | otherwise = True
 where x1 = fst ((fromJust moveToCheck) !! 0)
       y1 = snd ((fromJust moveToCheck) !! 0)
       x2 = fst ((fromJust moveToCheck) !! 1)
       y2 = snd ((fromJust moveToCheck) !! 1)


pawnMove :: Player -> Maybe[(Int,Int)] -> GameState -> Bool
pawnMove player pMove currentBoard    | ( x1Minusx2 ==  0 && y1Minusy2 == 1 &&  desPos == E) = True
                                      | ( x1Minusx2 ==  1 && y1Minusy2 == 1 && desPos /= E && playerOf (pieceOf desPos) /= player) = True 
                                      | otherwise = False
                                      where  x1Minusx2 = abs (fst ((fromJust pMove) !! 0) - fst ((fromJust pMove) !! 1))
                                             y1Minusy2 = abs (snd ((fromJust pMove) !! 0) - snd ((fromJust pMove) !! 1))
                                             desPos = (getFromBoard (theBoard currentBoard) ((fromJust pMove) !! 1))

knightMove :: Maybe[(Int,Int)] -> Player -> GameState -> Bool
knightMove kMove player currentBoard | (x1Minusx2 == 1 && y1Minusy2 == 2 && desPos == E) = True 
                                          | (x1Minusx2 == 2 && y1Minusy2 == 1 && desPos == E) = True
                                          | (x1Minusx2 == 1 && y1Minusy2 == 2 && playerOf (pieceOf desPos) /= player) = True 
                                          | (x1Minusx2 == 2 && y1Minusy2 == 1 && playerOf (pieceOf desPos) /= player) = True 
                                          | otherwise = False
                                          where  x1Minusx2 = abs (fst ((fromJust kMove) !! 0) - fst ((fromJust kMove) !! 1))
                                                 y1Minusy2 = abs (snd ((fromJust kMove) !! 0) - snd ((fromJust kMove) !! 1))
                                                 desPos = (getFromBoard (theBoard currentBoard) ((fromJust kMove) !! 1)) 

pawnPlaced :: Maybe[(Int,Int)] -> GameState -> Bool
pawnPlaced placement currentBoard | desPos == E = True 
                                  | otherwise = False
                                    where  desPos = (getFromBoard (theBoard currentBoard) ((fromJust placement) !! 0)) 







-- startGame :: Chooser -> String -> Chooser ->

---2D list utility functions-------------------------------------------------------
-- | Replaces the nth element in a row with a new element.
replace :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)



-- placePawn :: GameState -> Chooser -> String -> Chooser -> String -> Bool -> Bool -> IO()
-- placePawn currentState blackAI blackName whiteAI whiteName False False = 
checkPawnMovesAreSame :: Maybe [(Int,Int)] -> Maybe [(Int,Int)] -> Bool
checkPawnMovesAreSame pmove1 pmove2 = ((fromJust(pmove1)) !! 0) == ( (fromJust(pmove2)) !! 0)


checkPawnMovesHaveSameDest :: [(Int,Int)] -> [(Int,Int)] -> Bool
checkPawnMovesHaveSameDest pdes1 pdes2 = (pdes1 !! 1) == (pdes2 !! 1)


checkForPawnUpgrade :: [Cell] -> Piece -> IO Bool
checkForPawnUpgrade [] p = return False
checkForPawnUpgrade (x:xs)  BlackPawn = if (x/=E && (pieceOf x) == BlackPawn) 
    then return True
    else checkForPawnUpgrade xs BlackPawn
checkForPawnUpgrade (x:xs)  WhitePawn = if (x/=E && (pieceOf x) == WhitePawn) 
    then return True
    else checkForPawnUpgrade xs WhitePawn

-- gets the top row of the board
getTopRow :: Board -> [Cell]
getTopRow board = head (board)
-- -- gets the bottom row of the board
getBottomRow :: Board -> [Cell]
getBottomRow board = board !! ((length board)- 1)

duelWinner :: Board -> [(Int, Int)] -> [(Int, Int)] -> Cell
duelWinner board move1 move2 | (type1 == type2)  = E
                      | (type1 == Knight) = cell1
                      | otherwise         = cell2
                      where cell1 = getFromBoard board (move1 !! 0)
                            cell2 = getFromBoard board (move2 !! 0)
                            type1 = typeOf $ pieceOf cell1
                            type2 = typeOf $ pieceOf cell2


letsPlay :: GameState -> Chooser -> String -> Chooser -> String -> IO()
letsPlay gss black blackName white whiteName = do
    blackMove <- black (gss) Normal Black 
    whiteMove <- white (gss) Normal White
    let gs = GameState (if blackMove==Nothing
                                then Passed
                                else if validate blackMove Black gss 
                                  then Played (head (fromJust blackMove), head (tail (fromJust blackMove)))--first player move
                                  else Goofed (head (fromJust blackMove), head (tail (fromJust blackMove))))
                               (blackPen gss + if ((validate blackMove Black gss) == False) then 1 else 0)
                               (if whiteMove==Nothing
                                  then Passed
                                  else if validate whiteMove White gss 
                                    then Played (head (fromJust whiteMove), head (tail (fromJust whiteMove)))
                                    else Goofed (head (fromJust whiteMove), head (tail (fromJust whiteMove)))) --Second player move
                               (whitePen gss + if ((validate whiteMove White gss) == False) then 1 else 0)
                               (if (blackMove==Nothing || validate blackMove Black gss == False)    :: [[a]] -> (Int,Int) -> a -> [[a]]
                                  then (if whiteMove==Nothing || validate whiteMove White gss == False then (theBoard gss) else (replace2 (replace2 (theBoard gss) ((fromJust whiteMove) !! 1) (getFromBoard (theBoard gss) ((fromJust whiteMove) !! 0))) ((fromJust whiteMove) !! 0) E))
                                  else (if whiteMove==Nothing || validate whiteMove White gss == False then (replace2 (replace2 (theBoard gss) ((fromJust blackMove) !! 1) (getFromBoard (theBoard gss) ((fromJust blackMove) !! 0))) ((fromJust blackMove) !! 0) E)
                                          else if checkPawnMovesHaveSameDest blackMove whiteMove 
                                            then (replace2 (replace2 (replace2 (theBoard gss) ((fromJust whiteMove) !! 0) E) ((fromJust blackMove) !! 0) E)
                                              ((fromJust blackMove) !! 1) (duelWinner (theBoard gss) (fromJust blackMove) (fromJust whiteMove)))
                                            else (replace2 (replace2 (replace2 (replace2 (theBoard gss) ((fromJust whiteMove) !! 0) E) ((fromJust blackMove) !! 0) E)
                                                      ((fromJust whiteMove) !! 1) (getFromBoard (theBoard gss) ((fromJust whiteMove) !! 0)))
                                                    ((fromJust blackMove) !! 1) (getFromBoard (theBoard gss) ((fromJust blackMove) !! 0)))
                                            ))
                                            
    putStrLn (show gs)
    if (blackMove == Nothing) && (whiteMove == Nothing) || gameCheck gs
    then putStrLn("whoWins gs blackName whiteName") -- win condition 
    else checkEndCondition gs black blackName white whiteName

-- this function is ran at the end of every turn line 232 gets the top row of Cells while line 233 gets the Bottom row of Cells
checkEndCondition :: GameState -> Chooser -> String -> Chooser -> String -> IO()
checkEndCondition currentBoardState blackMove blackStr whiteMove whiteStr = do
    blackPawnPlacement <- checkForPawnUpgrade (getTopRow (theBoard currentBoardState)) BlackPawn
    whitePawnPlacement <- checkForPawnUpgrade (getBottomRow (theBoard currentBoardState)) WhitePawn
    if (blackPawnPlacement || whitePawnPlacement) 
        then letsPlay currentBoardState blackMove blackStr whiteMove whiteStr -- do pawn upgrade
        else letsPlay currentBoardState blackMove blackStr whiteMove whiteStr


gameCheck :: GameState -> Bool
gameCheck currentBoardState | (blackPen currentBoardState) >= 2 ||(whitePen currentBoardState) >= 2 = True
                            | otherwise = False

