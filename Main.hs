import Data.Char
import System.IO
import System.IO.Error
import Parser
import Board
import AI
import Data.Maybe

allOptions :: [Char]
allOptions = [readOption, writeOption, newGameOption, finishGameOption] ++ wolfMoves

printOptions :: IO ()
printOptions = do putStrLn "ruchy wilka: q - góra-lewo, w - góra-prawo, a - dół-lewo, w - dół prawo"
		  putStrLn "n - nowa gra"
		  putStrLn "k - koniec gry"
		  putStrLn "z - zapis stanu gry do pliku"
		  putStrLn "r - wczytanie stanu gry z pliku"
		  putStrLn "Po wciśnięciu klawisza wybranej opcji, naciśnij Enter"

--pobiera od użytkownika początkową pozycję y wilka na planszy (x=0)
getStartPosition :: IO Int
getStartPosition = do x <- getChar
                      if isDigit x && (digitToInt x) >= 0 && (digitToInt x) <= 3 then return (digitToInt x)
                                        else do putStrLn "\nPodaj liczbe od 0 do 3"
                                                getStartPosition

--zwraca nową pozycję wilka na planszy po wykonaniu ruchu
getNewWolfPosition :: (Int, Int) -> Char -> (Int, Int)
getNewWolfPosition (x,y) 'q' | x `mod` 2 == 0 = (x-1, y)
                             | otherwise = (x-1,y-1)
getNewWolfPosition (x,y) 'w' | x `mod` 2 == 0 = (x-1, y+1)
                             | otherwise = (x-1, y)
getNewWolfPosition (x,y) 'a' | x `mod` 2 == 0 = (x+1, y)
                             | otherwise = (x+1, y-1)
getNewWolfPosition (x,y) 's' | x `mod` 2 == 0 = (x+1, y+1)
                             | otherwise = (x+1, y)
getNewWolfPosition _ _ = error "Błędny ruch"

writeBoardToFile :: Handle -> [[String]] -> IO ()
writeBoardToFile _ [[]] = return ()
writeBoardToFile _ [] = return ()
writeBoardToFile file [a] = hPutStr file (getStringFromList a)
writeBoardToFile file (x:xs) = do hPutStrLn file (getStringFromList x)
                                  writeBoardToFile file xs

--wykonanie akcji wskazanej przez użytkownika
doAction :: Char -> [[String]] -> (Int, Int) -> IO ()
doAction 'k' _ _ = do putStrLn "\nKoniec gry"
                      return ()
doAction 'n' _ _ = do putStrLn "" --nowa gra
                      main
--zapis stanu gry do pliku
doAction 'z' board (x,y) = catch (
			   do putStrLn "\nPodaj nazwę pliku, do której ma zostać zapisany stan gry"
                              filePath <- getLine
                              do handle <- openFile filePath WriteMode
                                 writeBoardToFile handle board
                                 hClose handle
				 game board (x,y)
			   ) errorHandler
                      	   where
                        	errorHandler e = do putStrLn "Nie udała się operacja zapisu. Nastąpi powrót do gry"
                                                    getLine
                                                    game board (x,y)

--wczytaj stan gry z pliku                              
doAction 'r' board (x,y) = catch 
                      (do putStrLn "\nPodaj nazwę pliku, z którego ma zostać wczytany stan gry"
                          filePath <- getLine
                          do handle <- openFile filePath ReadMode 
                             content <- hGetContents handle
			     let parseBoard = parseBoardFromRows (parseBoardRows content)
				 (a,b) = findWolfCoordinates parseBoard
			     if (not (validateBoardReadFromFile parseBoard))
				then do putStrLn "Podany plik zawiera niepoprawny format danych"
					hClose handle
					printOptions
		      			action <- getActionFromUser
		      			doAction action board (x,y)
				else 
					do game parseBoard (a,b)
                             		   hClose handle
                      ) errorHandler
                      where
                        errorHandler e = do putStrLn "Nie można wczytać pliku. Nastąpi powrót do bierzącej gry"
                                            getLine
                                            game board (x,y)
--wykonanie akcji ruchu wilka na planszy                      
doAction move board position = doMove move board position

areSheepsWinners :: [[String]] -> (Int, Int) -> Bool
areSheepsWinners _ (0,_) = False
areSheepsWinners tab (7,y) = (not (isSquareFree tab (6, y))) && (not (isSquareFree tab (6,y-1)))
areSheepsWinners tab (x,y) | x `mod` 2 == 0 = (not (isSquareFree tab (x-1,y))) && (not (isSquareFree tab (x-1,y+1))) && (not (isSquareFree tab (x+1,y))) && (not (isSquareFree tab (x+1,y+1)))
                      | otherwise = (not (isSquareFree tab (x-1,y))) && (not (isSquareFree tab (x-1,y-1))) && (not (isSquareFree tab (x+1,y))) && (not (isSquareFree tab (x+1,y-1)))

--wykonaj ruch wilka na planszy
doMove :: Char -> [[String]] -> (Int, Int) -> IO ()
doMove move board (x,y) = if (isWolfMovePermitted (board) (x, y) move) then 
			     actionsAfterWolfMove (moveWolf board (x,y) (getNewWolfPosition (x,y) move)) (getNewWolfPosition (x,y) move)  
                          else do putStrLn "\nRuch niedozwolony!"
                                  game board (x,y)

sheepMove :: [[String]] -> [[String]]
sheepMove board = if newBoard == Nothing then board
			else fromJust newBoard
				where newBoard = getNextSheepMove board

--sprawdzenie czy wilk wygrał, ruch owiec, sprawdzenie czy owce wygrały
actionsAfterWolfMove :: [[String]] -> (Int, Int) -> IO ()
actionsAfterWolfMove board (0,_) = do drawCheckboard board
				      putStrLn "Wygrana wilka. Koniec gry"
actionsAfterWolfMove board (x,y) = if areSheepsWinners newBoard (x,y) 
					 then do drawCheckboard newBoard 
						 putStrLn "Owce wygrały. Koniec gry"
					 else do game newBoard (x,y)
						where newBoard = sheepMove board

getActionFromUser :: IO Char
getActionFromUser = do action <- getLine
		       if (length action) /= 1 || not (elem (action !! 0) allOptions) then
			  do putStrLn "\nNieprawidłowe dane. Wybierz jedną z poniższych opcji:"
			     printOptions
		             getActionFromUser
			else return (action !! 0)

game :: [[String]] -> (Int, Int) -> IO ()
game board (x,y) = do drawCheckboard board
		      printOptions
		      action <- getActionFromUser
		      doAction action board (x,y)
 
main = do putStrLn "\nwybierz pozycję początkową wilka w dolnym rzędzie podając liczbę od 0 do 3"
          y <- getStartPosition
          game (initCheckboard y) (7,y)
