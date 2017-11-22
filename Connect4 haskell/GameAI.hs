module GameAI where
import Data.Maybe
data Piece  = Yellow | Red deriving (Eq, Show)
data RoseTree a = Leaf a | RS a [RoseTree a ] deriving (Show)

-- The Data for the column and the board
type Column = [Piece] 
type Board = [Column]

-- The Data for the Board
data BoardState = BS
	{theBoard :: Board,
	lastMove :: Piece,
	numColumns :: Int,
	numRows :: Int,
	numToConnect :: Int} deriving (Show)

-- makeMove takes in a BoardState and a desired column number into which to drop a piece.  
-- The color of the piece being dropped depends on the color of the  lastMove  recorded  
-- in  the  current  board  state.   Ultimately,  we  would  like to return a new board state 
-- { the result of dropping a Piece into a column, as well  as  updated  lastMove. Returns BoardState
-- if valid, Nothing if it's not. It calls the checkLegal function and sees if the move is valid. 
-- If it is then it just puts the piece on the end of the column.

makeMove :: BoardState -> Int -> Maybe BoardState
makeMove boardState columnPicked = if (checkLegal boardState columnPicked) == True
								   then Just(BS(setBoard (theBoard boardState) columnPicked (switchPiece(lastMove boardState)))(switchPiece(lastMove boardState)) (numColumns boardState) (numRows boardState) (numToConnect boardState))
								   else Nothing


-- Pads Nothing to end of a list of Maybe Piece. Returns [Maybe Piece] with nothing padded till the end of the list
padN :: [Maybe Piece] -> Int -> [Maybe Piece]
padN xs n =   xs ++  replicate (n - (length xs )) Nothing					  
					  
-- Returns [[Maybe Piece]] top to bottom, left to right. calls the PadN function		  
rows :: BoardState -> [[Maybe Piece]]
rows boardState = reverse(transpose (columns boardState))

-- Returns [[Maybe Piece]] left to right, bottom to top, calls the PadN function
columns :: BoardState -> [[Maybe Piece]]
columns boardState = map (\col -> padN col (numRows boardState)) (map (map Just)(theBoard boardState))

-- Calls the columns function and the diagonal to return the Forward Diagonal of the board in [[Maybe Piece]] form
diagonalsForward :: BoardState -> [[Maybe Piece]]
diagonalsForward boardState =  diagonal boardState (columns boardState) [] 0 ((numRows boardState)-1)


-- Calls the columns function and reverses the order of the Columns and then call the diagonal function to return the Backward Diagonal of the board in [[Maybe Piece]] form
diagonalsBackward :: BoardState -> [[Maybe Piece]]
diagonalsBackward boardState = diagonal boardState (map reverse (columns boardState)) [] 0 ((numRows boardState)-1)


-- This computes the diagonal starting from the left top corner. It calls diagonal2 function and appends [Maybe Piece] to the diagonals found so far and returns the matrix
-- It works by checking 4 cases
-- Case 1 - Checks if y > 0 if yes then append everything diagonal to y to the current list of diagonal. Then subtract 1 from y and recurse with x = 0
-- Case 2 - if y == 0 && x <= ((numColumns boardState)-1) - then do the same procedure as case 1 but recurse again with y = -1 and x = 1
-- Case 3 - (y < 0 && x <= (numColumns boardState)-1) - then append everything diagonal to x and recurse with y = -1 and x +1
-- Case 4 - Return the list we have been appending to
diagonal :: BoardState -> [[Maybe Piece]] -> [[Maybe Piece]] -> Int -> Int -> [[Maybe Piece]]
diagonal boardState currentBoard diag x y 
											| y > 0 = diagonal boardState currentBoard (diag ++[(diagonal2 currentBoard [] 0 y ((numColumns boardState)-1) ((numRows boardState)-1))])0 (y-1)
											
											|y == 0 && x <= ((numColumns boardState)-1) = diagonal boardState currentBoard (diag ++ [(diagonal2 currentBoard [] 0 y ((numColumns boardState)-1) ((numRows boardState)-1))])1 (y-1)
											
											|(y < 0 && x <= (numColumns boardState)-1) = diagonal boardState currentBoard (diag ++ [(diagonal2 currentBoard [] x 0 ((numColumns boardState)-1) ((numRows boardState)-1))]) (x+1) y
											
											| otherwise =  diag

-- This function takes in the current board, a list of diagonals and x, y, num of col and row. If checks for diagonal by recursing with x+1 and y+1 and appending the value of the xy in the current board to the current list of diagonals. Return the list of diagonal if y > row and x > col. That is all the possible diagonals for the element of currentBoard in the initial x,y
diagonal2 :: [[Maybe Piece]] -> [Maybe Piece] -> Int -> Int -> Int -> Int -> [Maybe Piece]
diagonal2 currentBoard pieceList x y col row = if(y > row || x > col)
							then pieceList
							else diagonal2 currentBoard (pieceList ++[((currentBoard !! x) !! y)]) (x+1) (y+1) col row

-- This function calls isConnected function for every [Maybe Piece] in [[Piece]]. Returns true if there are n pieces repeating consecutively. No otherwise		
checkConnected :: [[Maybe Piece]] -> Int -> Bool
checkConnected [] numToConnect = False
checkConnected (x:xs) numToConnect =  if(isConnected x 1 numToConnect) == True
						then True
						else checkConnected xs numToConnect
-- For the [Maybe Piece] you go through the list and check if you can find Pieces connected n times consecutively
isConnected :: [Maybe Piece] -> Int -> Int -> Bool
isConnected (x:[]) connected numToConnect = if (connected >= numToConnect) == True
											then True
											else False
										
isConnected (x:xs) connected numToConnect |connected >= numToConnect = True 
										| (isSame x (head xs)) == True = isConnected xs (connected + 1) numToConnect
										| otherwise = isConnected xs 1 numToConnect
			
-- Checks if the two pieces type are the same. Return true if it is. False otherwise
isSame :: Maybe Piece -> Maybe Piece -> Bool
isSame (Just Yellow) (Just Yellow) = True
isSame (Just Red) (Just Red) = True
isSame Nothing _ = False
isSame (Just Red) (Just Yellow) = False
isSame x y = isSame y x


-- Place the Piece that moved last in the column desired. All the error checks has been done for it in make move function
setBoard :: Board -> Int -> Piece -> Board
setBoard board columnPicked piece= (take (columnPicked - 1) board) ++ [((board !! (columnPicked -1)) ++ [piece])] 
									++ (drop columnPicked board)
-- Transposes a matrix and return it
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- If yellow then return Red. Else Yellow							
switchPiece :: Piece -> Piece
switchPiece Yellow = Red
switchPiece Red = Yellow



checkBoardFull :: BoardState -> Bool
checkBoardFull bs = 
    let 
        cols = theBoard bs
    in
        all (\col -> length col == numRows bs) cols 






--foldRose (\(k, bd) -> (k, heuristicbd))
--		 (\(k, bd) -> "take the (path, bdScore) pair for which bsScore is maximal" and put k at the top of path and continue passing up bdScore)

foldRose :: (a -> b) -> (a -> [b] -> b) -> RoseTree a -> b 
foldRose l r (Leaf x) = l x
foldRose l r (RS x rss) = r x (map (foldRose l r ) rss)


makePossibleMoves :: BoardState -> [(Int, BoardState)]
-- This what you want makePossible bs = [(1, makeMove bs 1), (2, makeMove 2)..] 
makePossibleMoves bs = makePossibleMoves' 1 (numColumns bs) bs



makePossibleMoves' :: Int -> Int -> BoardState -> [(Int, BoardState)]
makePossibleMoves' counter limit bs = 
								if counter == limit 
									then case (makeMove bs) counter of 
										Nothing -> []
										Just newBs -> [(counter, newBs)]
									else case (makeMove bs) counter of 
										Nothing -> makePossibleMoves' (counter +1) limit bs
										Just newBs -> (counter, newBs):makePossibleMoves' (counter+1) limit bs




joinMaybeList :: [(b,Maybe a)] -> [(b,a)]
joinMaybeList [] = []
joinMaybeList ((_,Nothing):xs) = joinMaybeList xs
joinMaybeList ((y,Just x):xs) = (y,x) : joinMaybeList xs

{-
makeChoices rs
  returns a list of pairs, each of which is of the form
  (k,rsk)
where
  rsk is the result of applying makeMove in the kth columns.
  we leave out the invalid moves, i.e. ones for which makeMove returned Nothing.
-}
makeChoices :: BoardState -> [(Int,BoardState)]
makeChoices bs = joinMaybeList $ map (\i -> (i,makeMove bs i)) [1 ..(numColumns bs)]

makeChoicesLeaf :: BoardState -> [RoseTree (Int,BoardState)]
makeChoicesLeaf = map Leaf . makeChoices

mapRose :: (a -> b) -> RoseTree a -> RoseTree b
mapRose f = foldRose (Leaf . f) (\x rss -> RS (f x) rss)

{-
deepen one level
we assume that every leaf is actually of the form Leaf x, and not RS x []
-}
deepenOneLevel :: RoseTree (Int,BoardState) -> RoseTree (Int,BoardState)
deepenOneLevel =
  foldRose
    (\(i,bs) -> if isJust (checkWin bs) then Leaf (i, bs)  
				else RS (i,bs) (makeChoicesLeaf bs)) RS

deepenNLevels :: Int -> RoseTree (Int,BoardState) -> RoseTree (Int,BoardState)
deepenNLevels 0 bds = bds
deepenNLevels n bds = deepenNLevels (n-1) (deepenOneLevel bds)

makeChoicesToLevel :: Int -> BoardState -> RoseTree (Int,BoardState)
makeChoicesToLevel n bs = deepenNLevels n (Leaf (0,bs))

root :: RoseTree a -> a
root (Leaf a) = a
root (RS a as) = a

checkConsecutiveC :: BoardState -> Piece ->  [[Int]]
checkConsecutiveC boardState piece = map(\pieces -> (map(\n -> isConnected' pieces 0 n 0 piece)
							   [2 .. (numToConnect boardState)]))(columns(boardState))

checkConsecutiveR :: BoardState -> Piece ->  [[Int]]
checkConsecutiveR boardState piece = map(\pieces -> (map(\n -> isConnected' pieces 0 n 0 piece)
							   [2 .. (numToConnect boardState)]))(rows(boardState))

checkConsecutiveF :: BoardState -> Piece ->  [[Int]]
checkConsecutiveF boardState piece = map(\pieces -> (map(\n -> isConnected' pieces 0 n 0 piece)
							   [2 .. (numToConnect boardState)]))(diagonalsForward(boardState))
							   
checkConsecutiveB :: BoardState -> Piece ->  [[Int]]
checkConsecutiveB boardState piece = map(\pieces -> (map(\n -> isConnected' pieces 0 n 0 piece)
							   [2 .. (numToConnect boardState)]))(diagonalsBackward(boardState))

findValue :: BoardState -> Piece -> Int
findValue boardState piece = (sum(map(\intMatrix -> sum(map(\count -> sum(map(\n -> 10^n * (count !! (n - 1)))[1 .. ((numToConnect boardState) -1)])) intMatrix))
						[checkConsecutiveC boardState piece, checkConsecutiveR boardState piece, checkConsecutiveF boardState piece, checkConsecutiveB boardState piece])) + (scoreWin piece boardState)

heuristicScore :: BoardState -> Piece -> Int
heuristicScore bs piece = (findValue bs piece) - (findValue bs (switchPiece piece))

extractScore :: RoseTree (Int, Int) -> Int
extractScore roseTree = (snd (root roseTree))


extractScore2 :: RoseTree (Int, Int) -> Int
extractScore2 roseTree = (snd (root roseTree))


extractCol:: RoseTree (Int, Int) -> Int
extractCol roseTree  = (fst (root roseTree))



scoreWin :: Piece -> BoardState -> Int
scoreWin piece bs 	| checkWin bs == Just piece = 10000000000
					| checkWin bs == Just (switchPiece piece) = -10000000000
					| otherwise = 0

-- It checks if the Column picked is valid returns True if it is and false otherwise
checkLegal:: BoardState -> Int -> Bool
checkLegal boardState columnPicked = if (((numColumns boardState) >= columnPicked  &&  columnPicked > 0)
									 && length((theBoard boardState) !! (columnPicked -1)) < (numRows boardState))
									 then True
									 else False								 

-- Checks if the row, column or diagonal has any pieces that occurs numToConnect times. Returns the Piece if yes. Nothing Otherwise
checkWin :: BoardState -> Maybe Piece
checkWin boardState = if or[(checkConnected (columns(boardState)) (numToConnect boardState)),
						(checkConnected (rows(boardState)) (numToConnect boardState)),
						(checkConnected (diagonalsBackward(boardState))(numToConnect boardState)),
						(checkConnected (diagonalsForward(boardState))(numToConnect boardState))] == True
					  then Just(lastMove boardState)
					  else Nothing


-- Talked about this algorithm among students, it was difficult to grasp 
-- and code but made doing this function way easier
minMax :: Piece -> RoseTree (Int,  BoardState) -> RoseTree (Int,Int)
minMax piece roseTree = foldRose  (\(col, bs) -> Leaf (col, heuristicScore bs piece)) 
                            (\(col, bs) children -> if piece == (lastMove bs) 
                            then RS (col , minimum (map extractScore children)) children                                                                                                                                                                    
                            else RS (col , maximum (map extractScore children)) children) roseTree


							   
isConnected' :: [Maybe Piece] -> Int -> Int -> Int -> Piece -> Int
isConnected' [] consecutive numToConnect connected (piece) | consecutive == numToConnect =  (connected + 1)
														   | otherwise = connected
isConnected' (x: xs) consecutive numToConnect connected piece 
										| consecutive == numToConnect && (xs == [] || head xs /= Just piece) =  isConnected' xs 0 numToConnect (connected + 1) piece
										| consecutive == numToConnect && (xs /=[] && head xs == Just piece) =  isConnected' xs 0 numToConnect (connected) piece
										| x == Just piece = isConnected' xs (consecutive + 1) numToConnect connected piece
										| otherwise = isConnected' xs 0 numToConnect connected piece 



suggestMove' :: RoseTree (Int, Int) -> Int -> Int -> Int
suggestMove' (Leaf x) c n  = 1
suggestMove' (RS a children) col max = checkChildren children col max

checkChildren :: [RoseTree (Int, Int)] -> Int -> Int -> Int
checkChildren [] col max = col
checkChildren (x:xs) col max | extractScore2 x >= max = checkChildren xs (extractCol x) (extractScore x)
							 | otherwise = checkChildren xs col max
							 
							 
							 
							 
							 
							 
suggestMove :: BoardState -> Maybe Int
suggestMove bs | (checkBoardFull bs) == True = Nothing 
               | otherwise = Just (suggestMove' (minMax (switchPiece (lastMove bs)) (makeChoicesToLevel 5 bs)) 1 (-999999999))

