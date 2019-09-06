module Grammar where

data Expression = Impl { getFrom :: Expression, getTo:: Expression }
                | Or   { getFrom :: Expression, getTo:: Expression }
                | And  { getFrom :: Expression, getTo:: Expression }
                | Not  Expression
                | Var  String
                deriving (Eq, Ord)

data Problem = Problem { getContextProblem :: [Expression], getResultProblem :: Expression }

instance Show Expression where
    show (Impl fi se) = "(" ++ show fi ++ " -> " ++ show se ++ ")"
    show (Or fi se)   = "(" ++ show fi ++ " | " ++ show se ++ ")"
    show (And fi se)  = "(" ++ show fi ++ " & " ++ show se ++ ")"
    show (Not e)      = "!" ++ show e
    show (Var v)      = v

data LineProof = Axiom { getValue :: Int, getExpression :: Expression }
               | Hypothesis { getValue :: Int, getExpression :: Expression }
               | ModusPonens { getImplication :: Expression, getLeft :: Expression, getExpression :: Expression }
               | MyNothing { getExpression :: Expression }
               deriving (Eq, Ord)

data SmartContext = SimpleContext { getContext :: [Expression] } 
                  | ComplexContext { getContext :: [Expression], getFirst :: SmartContext, getSecond :: SmartContext } 
                  deriving (Eq, Ord)

instance Show SmartContext where
    show (SimpleContext a) = show a 
    show (ComplexContext a c d) = (show a) ++ " " ++ " FROM " ++ (show c) ++ " AND " ++ (show d)

instance Show LineProof where
    show (Axiom val expr) = "Ax. sch. " ++ (show val) ++ "] " ++ (show expr)
    show (Hypothesis val expr) = "Hypothesis " ++ (show val) ++ "] " ++ (show expr)
    show (ModusPonens fi se expr) = "M.P. " ++ (show fi) ++ ", " ++ (show se) ++ "] " ++ (show expr)
    show (MyNothing expr) = "MyNothing] " ++ (show expr)
