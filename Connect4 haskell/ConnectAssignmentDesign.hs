-- The data for the piece
data Piece  = Yellow | Red
 
-- The Data for the column and the board
type Column = [Piece]
type Board = [Column]

-- The Data for the Board
data BoardState = BS
	{theBoard :: Board,
	lastMove :: Piece,
	numColumns :: Int,
	numRows :: Int,
	numToConnect :: Int} 

-- makeMove takes in a BoardState and a desired column number into which to drop a piece.  The color of the piece being dropped depends on the color of the  lastMove  recorded  in  the  current  board  state.   Ultimately,  we  would  like to return a new board state { the result of dropping a Piece into a column, as well  as  updated  lastMove. Returns BoardState if valid, Nothing if it's not. 
-- It is part of design becuase if is a function that requires a function that checks if a move is vaild, a function to make the move and a funvtion to switch the pieces.
makeMove :: BoardState -> Int -> Maybe BoardState
makeMove boardState columnPicked = if (checkLegal boardState columnPicked) == True
								   then undefined
								   else undefined

-- It checks if the Column picked is valid returns True if it is and false otherwise
-- This is a really simple function that just checks if the column picked is full and if the column picked is within the boundary
checkLegal:: BoardState -> Int -> Bool
checkLegal boardState columnPicked = undefined						 

-- Checks if the row, column or diagonal has any pieces that occurs numToConnect times consecutively. Returns the Piece if yes. Nothing Otherwise
-- This is a function that will call a lot of other functions. A function that checks if there are pieces connected n times consecutively for the row, the column and the diagonal. There are function calls to get the rows, columns and the diagonals too.
checkWin :: BoardState -> Maybe Piece
checkWin boardState = if or[(checkConnected (columns(boardState)) (numToConnect boardState)),
						(checkConnected (rows(boardState)) (numToConnect boardState)),
						(checkConnected (diagonalsBackward(boardState))(numToConnect boardState)),
						(checkConnected (diagonalsForward(boardState))(numToConnect boardState))] == True
					  then undefined
					  else undefined

-- This is in my design because it Needs to add Just infront of Pieces and pad nothing to the end and get the list of rows
rows :: BoardState -> [[Maybe Piece]]
rows boardState = undefined

-- This is in my design because it Needs to add Just infront of Pieces and pad nothing to the end anf get the list of cols
columns :: BoardState -> [[Maybe Piece]]
columns boardState = undefined


-- This needs to get the column  and pass it to the diagonal function to get the diagonal matrix
diagonalsForward :: BoardState -> [[Maybe Piece]]
diagonalsForward boardState =  diagonal boardState (columns boardState) [] 0 ((numRows boardState)-1)


-- This needs to get the column and pass it to the diagonal function to get the diagonal matrix
diagonalsBackward :: BoardState -> [[Maybe Piece]]
diagonalsBackward boardState = undefined


-- This computes the diagonal starting from the left top corner. It calls diagonal2 function and appends [Maybe Piece] to the diagonals found so far and returns the matrix
-- It works by checking 4 cases
-- Case 1 - Checks if y > 0 if yes then append everything diagonal to y to the current list of diagonal. Then subtract 1 from y and recurse with x = 0
-- Case 2 - if y == 0 && x <= ((numColumns boardState)-1) - then do the same procedure as case 1 but recurse again with y = -1 and x = 1
-- Case 3 - (y < 0 && x <= (numColumns boardState)-1) - then append everything diagonal to x and recurse with y = -1 and x +1
-- Case 4 - Return the list we have been appending to

-- This is in my design because this needs other functions such that it can go through and get the diagonals from the board
diagonal :: BoardState -> [[Maybe Piece]] -> [[Maybe Piece]] -> Int -> Int -> [[Maybe Piece]]
diagonal boardState currentBoard diag x y = undefined

-- This checks if pieces are connected n times consecutively.
-- This is in my design because this will need more functions. So it can check for all the [Maybe Piece] in [[Maybe Pieces]]. This one just passes in the column to a function which tries to check for pieces that are connected n times consecutively
checkConnected :: [[Maybe Piece]] -> Int -> Bool
checkConnected [] numToConnect = undefined
-- Place the Piece that moved last in the column desired. All the error checks has been done for it in make move function
-- I will have to implement this to have the movement working and this is a really simple funvtion
setBoard :: Board -> Int -> Piece -> Board
setBoard board columnPicked piece= undefined


-- If yellow then return Red. Else Yellow. It is a really simple function to implement							
switchPiece :: Piece -> Piece
switchPiece x = undefined
