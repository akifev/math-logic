module Grammar where

data Term = Plus Term Term
          | Mul  Term Term
          | Inc Term
          | Var String
          | Func String [Term] 
          | Zero
          deriving (Eq, Ord, Show)
          
data Expression = Impl { getFrom :: Expression, getTo :: Expression }
                | Or   Expression Expression
                | And  Expression Expression
                | Not Expression
                | ForAll String Expression
                | Exist String Expression
                | Pred String [Term]
                | PVar String
                | Equal Term Term
                deriving (Eq, Ord, Show)

data Problem = Problem { getContext :: [Expression], getResult :: Expression } deriving (Show)

data LineProof = Axiom { getValue :: Int, getExpression :: Expression }
               | Hypothesis { getValue :: Int, getExpression :: Expression }
               | ModusPonens { getFirstValue :: Int, getSecondValue :: Int, getExpression :: Expression }
               | MyNothing { getExpression :: Expression }
               deriving (Eq, Ord, Show)
