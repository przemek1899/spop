import Data.Char
import System.IO
import Parser

take2Dim :: (Int, Int) -> [[a]] -> [[a]] 
take2Dim (x,y) tab = tab

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt tab i | i < 0 = tab
               | length tab <= i = tab
               | otherwise = take i tab ++ drop (i+1) tab
                             
remove2Dim :: (Int, Int) -> [[a]] -> [[a]]
remove2Dim (_,_) [] = [[]]
remove2Dim (_,_) [[]] = [[]]
remove2Dim (a,b) (x:xs) | a < 0 || b <0 = x:xs
                        | a >= length (x:xs) = x:xs
                        | a == 0 = (removeAt x b) : xs
                        | otherwise = x : remove2Dim (a-1, b) xs


insertAt :: [a] -> a -> Int -> [a]
insertAt [] el _ = [el]
insertAt (x:xs) el i | i == 0 = el : x : xs
		     | otherwise = x : insertAt xs el (i-1)

insert2Dim :: (Int, Int) -> a -> [[a]] -> [[a]]
insert2Dim (_,_) el [] = [[el]]
insert2Dim (_,_) el [[]] = [[el]]
insert2Dim (a,b) el (x:xs) | a == 0 = (insertAt x el b) : xs
                           | otherwise = x : insert2Dim (a-1, b) el xs

replace2Dim :: (Int, Int) -> a -> [[a]] -> [[a]]
replace2Dim (x,y) a tab = insert2Dim (x,y) a (remove2Dim (x,y) tab)

spottedTab :: Int -> Char -> [Char]
spottedTab 0 _ = []
spottedTab l c = c : spottedTab (l - 1) c 
                     
sign = '.'
rows = 5
columns = 10
def_pools = 8

wolfMoves = ['q', 'w', 'a', 'd']

drawCheckboard' :: [[String]] -> IO ()
drawCheckboard' [] = return ()
drawCheckboard' [[]] = return ()
drawCheckboard' (x:xs) | length (x:xs) `mod` 2 == 0 = do drawRow x rows True
                                                         drawCheckboard' xs
                       | otherwise = do drawRow x rows False
                                        drawCheckboard' xs

drawRow :: [String] -> Int -> Bool -> IO ()
drawRow _ 0 _ = return ()
drawRow tab row b = do putStrLn (drawLine tab row b) 
                       drawRow tab (row -1) b


checkboard = [['e','e','e','e'], "eeee", "eeee", "weee"]

drawLine :: [String] -> Int  -> Bool -> [Char]
drawLine [] _ _ = []
drawLine (x:xs) row True = spottedTab columns sign ++ drawFigure x row ++ drawLine xs row True
drawLine (x:xs) row False = drawFigure x row ++ spottedTab columns sign ++ drawLine xs row False

drawFigure :: String -> Int -> [Char]
drawFigure "w" row = drawWolf row
drawFigure ['o', _] row = drawSheep row
drawFigure _ _ = spottedTab columns ' '

drawWolf :: Int -> String
-- dla rows = 5 i columns = 10, ogolnie columns = 2*rows
drawWolf row | row <= (rows `div` 2) = spottedTab (2*(row-1)) ' ' ++ spottedTab 2 sign ++ spottedTab (columns - 4*row) ' '  ++ spottedTab 2 sign ++ spottedTab (2*(row-1)) ' '
             | row == (rows `div` 2 + (rows `mod` 2)) = spottedTab (half - 1) ' ' ++ spottedTab 2 sign ++ spottedTab (half - 1) ' '
             | otherwise = drawWolf (rows - row + 1)
               where half = columns `div` 2

{- dla rows = columns
drawWolf row | row <= (columns `div` 2) = spottedTab (row -1) sign ++ (sign : []) ++ spottedTab (columns - row - 1) sign ++ (sign : []) ++ spottedTab (row - 1) sign
             | drawWolf (columns - rows + 1)
-}

sheepSign :: Char
sheepSign = '*'

drawSheep :: Int -> String
-- dla rows = 5 columns = 10
drawSheep row | row <= (rows `div` 2 + (rows `mod` 2)) = spottedTab ((columns - 2*row) `div` 2) ' ' ++ spottedTab (2*row) sheepSign ++ spottedTab ((columns -2*row)`div` 2) ' '
              | otherwise = drawSheep (rows - row + 1)

wolf :: String
wolf = "w"
emptyPool = "e"

--ruchy gracza
topRight :: Char
topRight = 'w'

topLeft :: Char
topLeft = 'q'

isWolfMovePermitted :: [[String]] -> (Int, Int) -> Char -> Bool
isWolfMovePermitted tab (x,y) 'q' | isWolfOnLeftEdge tab == True = False
                                  | otherwise = isPoolFree tab (x-1, y - (x `mod` 2))
isWolfMovePermitted tab (x,y) 'w' | isWolfOnRightEdge tab == True = False
                                  | otherwise = isPoolFree tab (x-1, y + 1 - (x `mod` 2))
isWolfMovePermitted tab (x,y) 'a' | isWolfOnLeftEdge tab == True = False
                                  | isWolfOnBottomRow tab == True = False
                                  | otherwise = isPoolFree tab (x+1, y - (x `mod` 2))
isWolfMovePermitted tab (x,y) 's' | isWolfOnRightEdge tab == True = False
                                  | isWolfOnBottomRow tab == True = False
                                  | otherwise = isPoolFree tab (x+1, y + 1 - (x `mod` 2))
isWolfMovePermitted _ _ _ = False
                            

isPoolFree :: [[String]] -> (Int, Int) -> Bool
isPoolFree tab (x,y) | (tab !! x) !! y == emptyPool = True
                     | otherwise = False

isWolfOnLeftEdge :: [[String]] -> Bool                                 
isWolfOnLeftEdge [] = False
isWolfOnLeftEdge [[]] = False
-- na lewej krawedzi wilk moze byc tylko co drugi rzad
isWolfOnLeftEdge (x:y:ys) | head y == wolf = True
                          | otherwise = isWolfOnLeftEdge ys
                                      
isWolfOnRightEdge :: [[String]] -> Bool
isWolfOnRightEdge [] = False
isWolfOnRightEdge [[]] = False
isWolfOnRightEdge (x:xs) | last x == wolf = True 
                         | otherwise = isWolfOnRightEdge (tail xs)
                           
isWolfOnBottomRow :: [[String]] -> Bool
isWolfOnBottomRow tab | elem wolf (last tab) == True = True
                      | otherwise = False

                                 
moveWolf :: [[String]] -> (Int, Int) -> (Int, Int) -> [[String]]
-- x1, y1 - stara pozycja wilka
-- x2, y2 - nowa pozycja wilka
moveWolf tab (x1, y1) (x2, y2) = replace2Dim (x2, y2) wolf (replace2Dim (x1,y1) emptyPool tab)

getStartPosition :: IO Int
getStartPosition = do x <- getChar
                      if isDigit x && (digitToInt x) >= 0 && (digitToInt x) <= 3 then return (digitToInt x)
                                        else do putStrLn "\npodaj liczbe od 0 do 3"
                                                getStartPosition

initCheckboard :: Int -> [[String]]
initCheckboard 0 = [["o1", "o2", "o3", "o4"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["w", "e", "e", "e"]]
initCheckboard 1 = [["o1", "o2", "o3", "o4"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "w", "e", "e"]]
initCheckboard 2 = [["o1", "o2", "o3", "o4"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "w", "e"]]
initCheckboard 3 = [["o1", "o2", "o3", "o4"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "w"]]
initCheckboard _ = error "zle dane"

getNewPosition :: (Int, Int) -> Char -> (Int, Int)
getNewPosition (x,y) 'q' | x `mod` 2 == 0 = (x-1, y)
                         | otherwise = (x-1,y-1)
getNewPosition (x,y) 'w' | x `mod` 2 == 0 = (x-1, y+1)
                         | otherwise = (x-1, y)
getNewPosition (x,y) 'a' | x `mod` 2 == 0 = (x+1, y)
                         | otherwise = (x+1, y-1)
getNewPosition (x,y) 's' | x `mod` 2 == 0 = (x+1, y+1)
                         | otherwise = (x+1, y)
getNewPosition _ _ = error "Błędny ruch"

doAction :: Char -> [[String]] -> (Int, Int) -> IO ()
doAction 'k' _ _ = do putStrLn ""
                      putStrLn "Koniec gry" 
                      return ()
doAction 'n' _ _ = do putStrLn ""
                      main
doAction 'z' board (x,y) = do putStrLn "Podaj nazwę pliku, do której ma zostać zapisany stan gry"
                              filePath <- getLine
                              do handle <- openFile filePath WriteMode
                                 writeBoardToFile handle board
                                 hClose handle
                              
doAction 'r' _ _ = do putStrLn "Podaj nazwę pliku, z którego ma zostać wczytany stan gry"
                      filePath <- getLine
                      do handle <- openFile filePath ReadMode
                         board <- readBoardFromFile handle
                         hClose handle
                      
doAction  move board position = doMove move board position

figureParser :: Parser Char
figureParser = do x <- item
                  if x /= ',' then return x else failure

readBoardFromFile :: Handle -> IO [[String]]
readBoardFromFile handle = do content <- hGetContents handle
                              return [[]]
                              

writeBoardToFile :: Handle -> [[String]] -> IO ()
writeBoardToFile _ [[]] = return ()
writeBoardToFile _ [] = return ()
writeBoardToFile file (x:xs) = do hPutStrLn file (getStringFromList x)
                                  writeBoardToFile file xs

getStringFromList :: [String] -> String
getStringFromList [] = ""
getStringFromList (x:xs) = x ++ getStringFromList xs

doMove :: Char -> [[String]] -> (Int, Int) -> IO ()
doMove move board (x,y) = if (isWolfMovePermitted (board) (x, y) move) then game (moveWolf board (x,y) (getNewPosition (x,y) move)) (getNewPosition (x,y) move)  
                          else do putStrLn "ruch niedozwolony"
                                  game board (x,y)

game :: [[String]] -> (Int, Int) -> IO ()
game board (x,y) = do putStrLn ""
                      drawCheckboard' board
                      putStrLn "wykonaj ruch"
--                      move <- getChar
                      action <- getChar
                      doAction action board (x,y)
--                      if (isWolfMovePermitted (board) (x, y) move) then game (moveWolf board (x,y) (getNewPosition (x,y) move)) (getNewPosition (x,y) move)  
--                                                                       else do putStrLn "ruch niedozwolony"
--                                                                               game board (x,y)

--grę rozpoczyna grający wilkiem
main = do putStrLn "wybierz pozycję początkową wilka w dolnym rzędzie podając liczbę od 0 do 3"
          y <- getStartPosition
          game (initCheckboard y) (7,y)
