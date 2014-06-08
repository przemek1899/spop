module Board where
                     
sign = '.'
--rows - liczba linii, z których składa się jedno pole na planszy
rows = 5
--columns liczba kolumna, z których składa się jedno pole na planszy
columns = 10
def_pools = 8

wolf :: String
wolf = "w"

emptyPool :: String
emptyPool = "e"

wolfSign :: Char
wolfSign = '*'

sheepSign :: Char
sheepSign = '*'

boardWidth = 4
boardLength = 8

wolfMoves = ['q', 'w', 'a', 's']
readOption = 'r'
writeOption = 'z'
newGameOption = 'n'
finishGameOption = 'k'


initCheckboard :: Int -> [[String]]
initCheckboard 0 = [["o1", "o2", "o3", "o4"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["w", "e", "e", "e"]]
initCheckboard 1 = [["o1", "o2", "o3", "o4"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "w", "e", "e"]]
initCheckboard 2 = [["o1", "o2", "o3", "o4"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "w", "e"]]
initCheckboard 3 = [["o1", "o2", "o3", "o4"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "e"], ["e", "e", "e", "w"]]
initCheckboard _ = error "zle dane"

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

drawCheckboard :: [[String]] -> IO ()
drawCheckboard board = do putStrLn "" --czyszczenie pierwszej linii
			  drawCheckboard' board

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
drawWolf row | row <= (rows `div` 2) = spottedTab (2*(row-1)) ' ' ++ spottedTab 2 wolfSign ++ spottedTab (columns - 4*row) ' '  ++ spottedTab 2 wolfSign ++ spottedTab (2*(row-1)) ' '
             | row == (rows `div` 2 + (rows `mod` 2)) = spottedTab (half - 1) ' ' ++ spottedTab 2 wolfSign ++ spottedTab (half - 1) ' '
             | otherwise = drawWolf (rows - row + 1)
               where half = columns `div` 2

drawSheep :: Int -> String
-- dla rows = 5 columns = 10
drawSheep row | row <= (rows `div` 2 + (rows `mod` 2)) = spottedTab ((columns - 2*row) `div` 2) ' ' ++ spottedTab (2*row) sheepSign ++ spottedTab ((columns -2*row)`div` 2) ' '
              | otherwise = drawSheep (rows - row + 1)

concatenate :: [[a]] -> [a]
concatenate [] = []
concatenate (x:xs) = x ++ concatenate xs

getWolfYCoordinate :: [String] -> Int
getWolfYCoordinate (x:xs) | x == wolf = boardWidth - length (x:xs)
		          | otherwise = getWolfYCoordinate xs

findWolfCoordinates :: [[String]] -> (Int, Int)
findWolfCoordinates (x:xs) | elem wolf x == True = (boardLength - length (x:xs), getWolfYCoordinate x)
		           | otherwise = findWolfCoordinates xs

--poniżej funkcje do walidacji wczytanej planszy
--zlicz wolne pole "e" w tablicy stringów
countFreePools :: [String] -> Int
countFreePools [] = 0
countFreePools (x:xs) | x == emptyPool = 1 + countFreePools xs
		      | otherwise = countFreePools xs

containsFigures :: [[String]] -> Bool
containsFigures board = (elem "o1" tab) && (elem "o2" tab) && (elem "o3" tab) && (elem "o4" tab) && (elem wolf tab)
			where tab = concatenate board

--czy każda podtablica jest równa 4
subArraysLength :: [[String]] -> Int -> Bool
subArraysLength [a] len = if length a == len then True else False
subArraysLength (x:xs) len | (length x) == len = subArraysLength xs len
		           | otherwise = False

--główna funkcja walidująca wczytaną planszę
validateBoardReadFromFile :: [[String]] -> Bool
validateBoardReadFromFile board | (length board) == boardLength && subArraysLength board boardWidth && containsFigures board && (countFreePools (concatenate board) == (boardLength*boardWidth-5)) = True
				| otherwise = False

-- (Int, Int) - aktulna pozycja wilka na planszy, Char - ruch
isWolfMovePermitted :: [[String]] -> (Int, Int) -> Char -> Bool
isWolfMovePermitted tab (x,y) 'q' | isWolfOnLeftEdge tab == True = False
                                  | otherwise = isSquareFree tab (x-1, y - (x `mod` 2))
isWolfMovePermitted tab (x,y) 'w' | isWolfOnRightEdge tab == True = False
                                  | otherwise = isSquareFree tab (x-1, y + 1 - (x `mod` 2))
isWolfMovePermitted tab (x,y) 'a' | isWolfOnLeftEdge tab == True = False
                                  | x == 7 = False
                                  | otherwise = isSquareFree tab (x+1, y - (x `mod` 2))
isWolfMovePermitted tab (x,y) 's' | isWolfOnRightEdge tab == True = False
                                  | x == 7 = False
                                  | otherwise = isSquareFree tab (x+1, y + 1 - (x `mod` 2))
isWolfMovePermitted _ _ _ = False
                            
-- czy dane pole jest wolne na planszy
isSquareFree :: [[String]] -> (Int, Int) -> Bool
isSquareFree tab (x,y) | (y < 0 || y > 3 || x < 0 || x > 7) == True = False
                       | (tab !! x) !! y == emptyPool = True
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

                                 
moveWolf :: [[String]] -> (Int, Int) -> (Int, Int) -> [[String]]
-- x1, y1 - stara pozycja wilka
-- x2, y2 - nowa pozycja wilka
moveWolf tab (x1, y1) (x2, y2) = replace2Dim (x2, y2) wolf (replace2Dim (x1,y1) emptyPool tab)

getStringFromList :: [String] -> String
getStringFromList [] = ""
getStringFromList [a] = a
getStringFromList (x:xs) = x ++ "," ++ getStringFromList xs


