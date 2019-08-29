{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

    $white                                  ;
    [\|\&\!\(\)\,]                          { \s -> TSym (head s)}
    "->"                                    { \s -> TImpl }
    "|-"                                    { \s -> TTurn }
    $alpha [$alpha $digit \` \’ \']*        { \s -> TVar s }

{
data Token = TSym Char | TImpl | TTurn | TVar String deriving (Eq, Show)
}
