module Expression where

data Expr = Num Int
          | Str String
          | Op BinOp Expr Expr
            deriving (Show)

data BinOp = Add | Concat
             deriving (SHow)

interp x@(Num _) = x
interp x@(Str _) = x
interp (Op Add (Num a) (Num b)) = Num (a + b)
interp (Op Concat (Str a) (Str b)) = Str (a ++ b)
