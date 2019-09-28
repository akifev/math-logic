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
    zero                                    { TSym '0' }
    inc                                     { TSym '\'' }
    
    forall                                  { TSym '@' }
    exist                                   { TSym '?' }
    point                                   { TSym '.' }

    mul                                     { TSym '*' }
    plus                                    { TSym '+' }
    eq                                      { TSym '=' }

    impl                                    { TImpl }
    var                                     { TVar $$ }
    pred                                    { TPred $$ }
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
    Unary                                   { $1 }
    |   Conjunction and Unary               { And $1 $3 }

Unary:
    Predicate                               { $1 }
    |   not Unary                           { Not $2 }
    |   open Expression close               { $2 }
    |   forall var point Expression         { ForAll $2 $4 }
    |   exist var point Expression          { Exist $2 $4 }

Predicate:
    pred                                    { PVar $1 }
    |   pred open Terms close               { Pred $1 $3 }
    |   Term eq Term                        { Equal $1 $3 }

Term:
    Slag                                    { $1 }
    |   Term plus Slag                      { Plus $1 $3 }

Slag:
    Multi                                   { $1 }
    |   Slag mul Multi                      { Mul $1 $3 }

Multi:
    var                                     { Var $1 }
    |   var open Terms close                { Func $1 $3 }
    |   open Term close                     { $2 }
    |   zero                                { Zero }
    |   Multi inc                           { Inc $1 }

Terms:
    Term                                    { $1 : [] }
    |   Terms comma Term                    { $3 : $1 }

{
parseError _ = error "Parse error"
}