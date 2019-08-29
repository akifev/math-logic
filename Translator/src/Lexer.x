{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white                                  ;
    [\|\&\!\(\)]                            { \s -> TSym (head s)}
    "->"                                    { \s -> TImpl }
    $alpha [$alpha $digit \` \’ \']*        { \s -> TVar s }

{
data Token = TSym Char | TImpl | TVar String deriving (Eq, Show)
}
