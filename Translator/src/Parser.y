{
module Parser where
import Lexer
import Grammar
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    or                                      { TSym '|' }
    and                                     { TSym '&' }
    not                                     { TSym '!' }
    open                                    { TSym '(' }
    close                                   { TSym ')' }
    impl                                    { TImpl }
    var                                     { TVar $$ }

%%

Expression:
    Disjunction                             { Disjunction $1 }
    |   Disjunction impl Expression         { Impl $1 $3 }

Disjunction:
    Conjunction                             { Conjunction $1 }
    |   Disjunction or Conjunction          { Or $1 $3 }

Conjunction:
    Negation                                { Negation $1 }
    |   Conjunction and Negation            { And $1 $3 }

Negation:
    not Negation                            { Not $2 }
    |   var                                 { Variable $1 }
    |   open Expression close               { Brackets $2 }

{
parseError _ = error "Parse error"
}