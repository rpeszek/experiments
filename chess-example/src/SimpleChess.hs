{-# LANGUAGE ScopedTypeVariables #-}

{-|
 Incomplete, but easy to extend, two player chess game (no UI just game rules), 
 has currently no checkmate detection, game over semantics,
 no ability to promote pawns and has very few piece types implemented.
 (I hope correctly since I have not played for many years)

 It is an exercises in use of Haskell where I think the benefits
 of this language are not that strong.  
 Chess game rules create very strongly coupled global state.  
 For example, valid moves dependent on other pieces on the board and that 
 global state needs to be considered all the time. 

 This code could be implemented in a simple language like Elm. 
 It does not use much of Haskell, no polymorphism! 
 It is basically lots of code using list monad.  I have just used standard 'do' notation, no
 list comprehension sugar. (Which should make this code look arguably nicer.) 

 Similar code can be implemented in a mainstream language like Java in a similar fashion 
 but with a loss of terseness and working around the lack of list comprehensions.  

 Good open question is how to make this more Haskell.

    * type safety over piece moves would be possible (e.g. complier checks that King does not travel more than one steps in either direction) 
      but that seems not important since the biggest issue is considering the global state of all the pieces and type level version of this 
      is a no-go.

    * recursion schemes - no clear recursive types here

    * more use of type variables?
      e.g. HKD pattern 'data Board f = Board [(Piece, f Position)]' - but that would require some generic 
      library to be beneficial and with Chess seems not that relevant.
      Or making Position or Direction into a type variable like 'data Board a = Board [(Piece, a)]' 
      Functor here does not seem useful for game implementation.

    * Free DSL approach also could be considered, but the challenge I am interested in solving is a cleaner
      decoupling of global board state and that needs to be handled anyway somewhere, not clear how a DSL would
      help with that.      
-}
module SimpleChess where

import           Control.Arrow 
import           Control.Monad
import           Data.Maybe
import           Data.Semigroup ((<>))  
import           Control.Applicative ((<|>))
import           Data.Maybe (catMaybes)
import qualified Data.List as L

------------------------------ 
----     Game API         ----
------------------------------ 

data ChessErr = InvalidNewPosition Position | NotYourTurn | InvalidOldPosition Position deriving Show

game :: Piece -> Position -> Position -> Game -> Either ChessErr Game    
game piece@(Piece _ dir) old new (Game gdir board) = 
    let 
        onboard = onBoard board
        available = availablePiecePositions piece old onboard 
        dirM      = whenB (dir == gdir) NotYourTurn
        pieceM    = whenB (not $ (piece, Just old) `elem` board) $ InvalidOldPosition old
        moveM     = whenB (not $ new `elem` available) $ InvalidNewPosition new
        newBoard = map (\(piece,ps) -> if ps == Just old then (piece, Just new) else (piece, ps)) .
                   map (\(piece,ps) -> if ps == Just new then (piece, Nothing) else (piece, ps)) $ 
                   board     
    in case dirM <|> pieceM <|> moveM of 
        Just err -> Left err
        Nothing -> Right $ Game (otherDir gdir) newBoard

-- | possibly, easier to use version of game       
game' :: Position -> Position -> Game -> Either ChessErr Game  
game' old new g@(Game _ board) = case L.find ((== Just old) . snd) board of 
    Nothing -> Left $ InvalidOldPosition old 
    Just (piece, _) -> game piece old new g 

availablePiecePositions :: Piece -> Position -> OnBoard -> [Position]
availablePiecePositions piece@(Piece tpe dir) pos board = 
    let enemyPositions :: [Position] = map (snd) . filter ((== otherDir dir) . pDirection . fst) $ board
        friendlyPositions :: [Position] =  map (snd) . filter ((== dir) . pDirection . fst) $ board
        possible = map ( map (++++ pos)) $ possibleMoves piece 
        pawnPossibleStriking = map ( map (++++ pos)) $ pawnStrikingMoves dir
    in L.nub . join $ do 
        positions :: [Position] <- possible
        pawnStriking :: [Position] <- pawnPossibleStriking
        case tpe of 
            Pawn -> 
                pure ( (takeWhile (not . (`elem` enemyPositions)) . takeWhile (not . (`elem` friendlyPositions)) . takeWhile onBoard8 $ positions)
                    <> (takeWhile (`elem` enemyPositions) . takeWhile onBoard8 $ pawnStriking))
            _ -> 
                pure . takeWhileInclusive (not . (`elem` enemyPositions)) . takeWhile (not . (`elem` friendlyPositions)) . takeWhile onBoard8 $ positions
 

-- | assumes specified direction turn returns all possible positions for next user
-- can be used to validate / disable positions in a UI
availablePlayerPositions :: Direction -> OnBoard -> [(Piece, [Position])]
availablePlayerPositions dir board = do 
    (piece, position) <- board
    guard $ pDirection piece == dir
    pure (piece, availablePiecePositions piece position board)

------------------------------    
-- Types and implementation --
------------------------------    

data PieceType = 
    Pawn 
    | Bishop    
     deriving (Eq, Show)

-- | direction of attack (or pawn direction)
data Direction = Up | Down deriving (Eq, Show)

otherDir :: Direction -> Direction
otherDir Up = Down
otherDir Down = Up

oneStep :: Direction -> Int 
oneStep Up = 1
oneStep Down = -1

-- | allows possibly 'infinite' positions to be limited by board logic
-- defined (x,y) coordinates
type Position = (Int, Int) 

-- | Postion vector addition e.g. '(1,2) ++++ (-1,2) == (0,4)'
(++++) :: Position -> Position -> Position
(++++) (a,b) = ((+ a) *** (+ b))

onBoard8 :: Position -> Bool
onBoard8 (x,y) = -1 < x && x < 8 && -1 < y && y < 8

-- | list of '(0,0)' relative trajectories a piece can move on infinite board
-- e.g. King would move only '[[(-1,0)], [(1,0)], [(0,1)], [(0,-1)]]' 
-- but Rook would move '[[(0,1), (0,2), ..], [(0,-1), (0,-2), ..], [(1,0), (2, 0), ..], [(-1,0), (-2,0)..]]'
type Moves = [Position]

data Piece = Piece {
     ptype :: PieceType
     , pDirection :: Direction
  } deriving (Eq, Show) 

-- | Contains all player pieces, if offboard Position is Nothing  
-- case for unifying Board and OnBoard under something like 'Board a = Board [(Piece, a)]'
type Board = [(Piece, Maybe Position)] 
type OnBoard = [(Piece, Position)] 

onBoard :: Board -> OnBoard
onBoard = catMaybes . map promoteSnd
  where 
    promoteSnd (a, Just b) = Just (a,b)
    promoteSnd (a, Nothing) = Nothing
    
-- | currently no Checkmate or GameOver construtor    
data Game = Game {
    lastTurn :: Direction
    , board :: Board  
  } deriving Show

-- | unresticted moves, assume infinite chess board and no obstacles
possibleMoves :: Piece -> [Moves]
possibleMoves (Piece Pawn dir) = [[(0,oneStep dir)]]
possibleMoves (Piece Bishop _) = do 
       i <- [-1,1]
       j <- [-1,1]
       pure $ map (((* i) *** (* j)) . diag) [1..] 
    where
        diag i = (i,i)

-- | pawn is unique in being able to move diagonally only when stiking        
pawnStrikingMoves :: Direction -> [Moves]
pawnStrikingMoves dir = [[(1,oneStep dir)], [(-1,oneStep dir)]] 

------------
-- utils ---
------------

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
whenB :: Bool -> a -> Maybe a                                         
whenB False _ = Nothing
whenB True a = Just a
 
-- ghci tests
{-
   p b
 _ P P 
 _ B
-}
tstBoard = [
    (Piece Pawn Up, Just (1,1))
    , (Piece Pawn Up, Just (2,1))
    , (Piece Bishop Up, Just (1,0))
    , (Piece Pawn Down, Just (1,2))
    , (Piece Bishop Down, Just (2,2))
    , (Piece Bishop Down, Nothing)
  ] :: Board

-- ghci>  
-- availablePlayerPositions Up (onBoard tstBoard)   
-- availablePlayerPositions Down (onBoard tstBoard) 
-- game' (1,1) (2,2) (Game Down tstBoard)
-- onBard tstBoard  