{
module Parser where
import Lexer
import Grammar
}

%name parseProblem Problem
%name parseExpression Expression
%tokentype { Token }
%error { parseError }

%token
    or                                      { TSym '|' }
    and                                     { TSym '&' }
    not                                     { TSym '!' }
    open                                    { TSym '(' }
    close                                   { TSym ')' }
    comma                                   { TSym ',' }
    impl                                    { TImpl }
    var                                     { TVar $$ }
    turn                                    { TTurn }

%%

Problem:
    Context turn Expression                 { Problem $1 $3}

Context:
    Expression                              { $1 : [] }
    |   Context comma Expression            { $3 : $1 }
    |   {- empty -}                         { [] }

Expression:
    Disjunction                             { $1 }
    |   Disjunction impl Expression         { Impl $1 $3 }

Disjunction:
    Conjunction                             { $1 }
    |   Disjunction or Conjunction          { Or $1 $3 }

Conjunction:
    Negation                                { $1 }
    |   Conjunction and Negation            { And $1 $3 }

Negation:
    not Negation                            { Not $2 }
    |   var                                 { Var $1 }
    |   open Expression close               { $2 }


{
parseError _ = error "Parse error"
}