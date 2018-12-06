module CParser where

import Ast
import ParserMonad

import Debug.Trace

parser :: Parser Program
parser = do code <- rep functionParser
            return $ P code


functionParser :: Parser Stmt
functionParser = do token $ literal "def"
                    name <- varParser
                    token $ literal "("
                    argsFunc <- args
                    token $ literal ")"
                    block <- blockParser
                    return $ Def name argsFunc block


argSingle :: Parser String
argSingle = do arg <- varParser
               token $ (literal "," <||> literal "")
               return arg


args :: Parser [String]
args = do arg <- rep argSingle
          return arg


orParser :: Parser Expr
orParser = withInfix andParser [("||", Or)]

andParser :: Parser Expr
andParser = withInfix condParser [("&&", And)]


condParser :: Parser Expr
condParser = withInfix addSubParser [(">=", GtEq), (">", Gt),
                                    ("<=", LtEq), ("<", Lt),
                                    ("!=", NEq), ("==", Eq)]

addSubParser :: Parser Expr
addSubParser = withInfix multDivModParser [("+", Plus), ("-", Sub)]

multDivModParser :: Parser Expr
multDivModParser = withInfix notParser [("*",Mult), ("/", Div), ("%",Mod)]

notParser :: Parser Expr
notParser = (do token $ literal "!"
                res <- notParser
                return $ Not res) <||> atoms

atoms :: Parser Expr
atoms = ints <||> vars <||> parens

ints :: Parser Expr 
ints = do res <- intParser
          return $ Val res

vars :: Parser Expr
vars = do var <- varParser
          return $ Var var

parens :: Parser Expr
parens = do token $ literal "("
            res <- orParser
            token $ literal ")"
            return res 


statments :: Parser Stmt
statments = ifElseParser <||> ifParser <||> whileParser <||> assignParser <||> contBreakParser <||> printParser <||> line <||> returnParser
 

ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              token $ literal "("
              expr <- orParser
              token $ literal ")"
              block <- blockParser 
              return $ If expr block


ifElseParser :: Parser Stmt
ifElseParser = do token $ literal "if"
                  token $ literal "("
                  expr <- orParser
                  token $ literal ")"
                  block <- blockParser
                  token $ literal "else"
                  blockF <- blockParser 
                  return $ IfElse expr block blockF


whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 token $ literal "("
                 expr <- orParser 
                 token $ literal ")"
                 block <- blockParser
                 return $ While expr block

assignParser :: Parser Stmt
assignParser = do varName <- varParser
                  token $ literal "="
                  var <- orParser
                  token $ literal ";"
                  return $ Assign varName var 

contBreakParser :: Parser Stmt
contBreakParser = do res <- token $ (literal "continue;" <||> literal "break;")
                     if res == "continue;"
                      then return Continue
                      else return Break

printParser :: Parser Stmt
printParser = do token $ literal "print "
                 res <- orParser
                 return $ Print res

line :: Parser Stmt
line = do res <-  orParser
          token $ literal ";"
          return $ Line res

returnParser :: Parser Stmt
returnParser = do token $ literal "return"
                  res <- orParser
                  token $ literal ";"
                  return $ Ret res


blockParser :: Parser Stmt
blockParser = do token $ literal "{"
                 res <- rep statments 
                 --traceShowM res
                 token $ literal "}"
                 return $ Block res






x = "if(x==2){x = x + 1;}else{x=x+2;}"
y = "while(3==2){2}"

funcTest = "def foo(x){if(x==2){return y;}else{x=3;}return x;}"
funcTest2 = "def foo(x){while(x>=2){x = x + 2;}}"
funcTest3 = "def foo(x){if(x==2||x==3) {return y;} }"
funcTest4 = "def foo(x){if(x==2){return y;} if(x==3){return z;}}"
combTest = funcTest ++ funcTest2 ++ funcTest4 ++ funcTest3