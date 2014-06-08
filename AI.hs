module AI (getNextSheepMove) where 

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.Random
import System.IO.Unsafe

flatten board = concat board

getPosition element fb = let 
								pos = elemIndex element fb 
								posInt = fromJust pos
								x = posInt `mod` 4
								y = posInt `div` 4
								in (x,y)
getBoardMap board = let
				fb = flatten board
				o1Position = getPosition "o1" fb
				o2Position = getPosition "o2" fb
				o3Position = getPosition "o3" fb
				o4Position = getPosition "o4" fb
				wPosition = getPosition "w" fb
				in Map.fromList [("o1",o1Position),("o2",o2Position),("o3",o3Position),("o4",o4Position),("w",wPosition)] 

getPositionMap boardMap = let
								entries = Map.assocs boardMap
								in Map.fromList [(y,x) | (x,y) <- entries]
								
getElement element boardMap = fromJust (Map.lookup element boardMap)


getBoardFromBoardMap boardMap = let
								positionMap = getPositionMap boardMap
								in [[ (getBoardChar x y positionMap) | x <- [0..3]] | y <- [0..7]]

								

getBoardChar x y positionMap = let 
								figure = Map.lookup (x,y) positionMap
								in if figure == Nothing then "e" else fromJust figure
								
getPossibleMoves element boardMap = let
										(x,y) = getElement element boardMap
										yOdd = y `mod` 2 == 1
										diff = if yOdd then -1 else 1
										initCandidates = if element == "w" then [(x+diff, y+1), (x,y+1), (x,y-1), (x+diff,y-1)] else [(x+diff, y+1), (x, y+1)]
										withoutExisting = initCandidates \\ (Map.elems boardMap)
										in [(element,(x,y)) | (x,y) <- withoutExisting, x <= 3, x >= 0, y >= 0, y <= 7]

getPossibleWolfMoves boardMap = getPossibleMoves "w" boardMap

shuffle :: [a] -> IO [a]
shuffle l = shuffle' l []
  where
    shuffle' [] acc = return acc
    shuffle' l acc =
      do k <- randomRIO (0, length l - 1)
         let (lead, x:xs) = splitAt k l
         shuffle' (lead ++ xs) (x:acc)

shuffleList list = unsafePerformIO (shuffle list)

getPossibleSheepMoves boardMap = concat [getPossibleMoves sheep boardMap | sheep <- (shuffleList ["o1", "o2", "o3", "o4"])]

transform boardMap move = Map.insert (fst move) (snd move) boardMap

evalBoardMap boardMap = let
							wolfposition = getElement "w" boardMap
							wolfY = snd(wolfposition)
							wolfMovesRate = (4 - (length (getPossibleWolfMoves boardMap))) * 4
							sheepClosessRate = getSheepClosenessRate boardMap
							in if wolfY /= 0 then (wolfMovesRate + wolfY + sheepClosessRate) else 0
							
getSheepClosenessRate boardMap = let
									o1P = getElement "o1" boardMap
									o2P = getElement "o2" boardMap
									o3P = getElement "o3" boardMap
									o4P = getElement "o4" boardMap
									xPos = [fst o1P, fst o2P, fst o3P, fst o4P]
									yPos = [snd o1P, snd o2P, snd o3P, snd o4P]
									maxX = maximum xPos
									minY = minimum yPos
									maxY = maximum yPos
									minX = minimum xPos
									xDiff = maxX - minX
									yDiff = maxY - minY
									in (7 + xDiff - yDiff)

data Tree = Empty | Node ([Char], (Int, Int)) Int [Tree]

getSubTrees :: [Char]->Map.Map [Char] (Int, Int)->Int->[Tree]
getSubTrees turn currentBoardMap depth = let	
												possibleMoves = if turn == "w" then (getPossibleWolfMoves currentBoardMap) else (getPossibleSheepMoves currentBoardMap)
												in [ let newBoardMap = transform currentBoardMap move 
														in (Node move (evalBoardMap newBoardMap) (if (depth > 0) then (getSubTrees (if turn == "w" then "o" else "w") newBoardMap (depth - 1)) else [])) | move <- possibleMoves]
							
getRating (Node _ b _) = b

getChildren (Node _ _ c) = c

getMove (Node a _ _) = a

getSubTreesRating subTrees = [(getSubTreeRating subTree, getMove subTree) | subTree <- subTrees]
getSubTreeRating subTree = max (getRating subTree) (if (null (getChildren subTree)) then 0 else maximum [getSubTreeRating s | s <- getChildren subTree])

getBiggestMove subTreesRating = snd (Map.findMax (Map.fromList subTreesRating))

getNextSheepMove board = let
							bm = getBoardMap board
							subTrees = getSubTrees "o" bm 4
							in if (null subTrees) then Nothing else Just (getBestMove subTrees bm)

getBestMove subTrees bm = let
						subTreesRating = getSubTreesRating subTrees
						bestMove = getBiggestMove subTreesRating
						transformed = transform bm bestMove
						in getBoardFromBoardMap transformed