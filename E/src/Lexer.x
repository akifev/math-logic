{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$higherAlpha = [A-Z]
$lowerAlpha = [a-z]


tokens :-

    $white                                  ;
    [\|\&\!\(\)\,\=\+\*\@\?\.\\0\']         { \s -> TSym (head s)}
    "->"                                    { \s -> TImpl }
    "|-"                                    { \s -> TTurn }
    $lowerAlpha [$digit]*                   { \s -> TVar s }
    $higherAlpha [$digit]*                  { \s -> TPred s }

{
data Token = TSym Char | TImpl | TTurn | TVar String | TPred String deriving (Eq, Show)
}
