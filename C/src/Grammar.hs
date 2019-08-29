module Grammar where

data Expression = Impl { getFrom :: Expression, getTo:: Expression }
                | Or   { getFrom :: Expression, getTo:: Expression }
                | And  { getFrom :: Expression, getTo:: Expression }
                | Not  Expression
                | Var  String
                deriving (Eq, Ord)

data Problem = Problem { getContext :: [Expression], getResult :: Expression }

instance Show Expression where
    show (Impl fi se) = "(" ++ show fi ++ " -> " ++ show se ++ ")"
    show (Or fi se)   = "(" ++ show fi ++ " | " ++ show se ++ ")"
    show (And fi se)  = "(" ++ show fi ++ " & " ++ show se ++ ")"
    show (Not e)      = "!" ++ show e
    show (Var v)      = v

data LineProof = Axiom { getValue :: Int, getExpression :: Expression }
               | Hypothesis { getValue :: Int, getExpression :: Expression }
               | ModusPonens { getMPFrom :: Expression, getMPTo :: Expression }
               | MyNothing { getExpression :: Expression }
               deriving (Eq, Ord)