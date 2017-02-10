module ApocBreakUp where

import ApocTools
import System.IO.Unsafe

getOpposite :: Player -> Player
getOpposite White = Black
getOpposite Black = White


data PieceType = Knight | Pawn deriving (Eq, Show, Read)

typeOf :: Piece -> PieceType
typeOf BlackKnight = Knight
typeOf BlackPawn   = Pawn
typeOf WhiteKnight = Knight
typeOf WhitePawn   = Pawn
