module CParser where

import Ast
import ParserMonad

import Debug.Trace

parser :: Parser Program
parser = undefined


line :: Parser Stmt
line = (do res <- orParser
           token $ literal ";"
           return res) <||> returnParser
-- parser' = (do res <- orParser
--               traceShowM res
--               token $ literal ";"
--               rest <- parser'
--               return $ [res] ++ rest) <||> returnParser 

keywords = ["def","return","when","if","then","else"]


varibleParser :: Parser Stmt
varibleParser = do x <- token $ varParser
                   if x `elem` keywords
                   then failParse
                   else return $ Var x




orParser :: Parser Stmt
orParser = withInfix andParser [("||", Or)]

andParser :: Parser Stmt
andParser = withInfix condParser [("&&", And)]


condParser :: Parser Stmt
condParser = withInfix addSubParser [(">=", GtEq), (">", Gt),
                                    ("<=", LtEq), ("<", Lt),
                                    ("!=", NEq), ("==", Eq)]

addSubParser :: Parser Stmt
addSubParser = withInfix multDivModParser [("+", Plus), ("-", Sub)]

multDivModParser :: Parser Stmt
multDivModParser = withInfix notParser [("*",Mult), ("/", Div), ("%",Mod)]

notParser :: Parser Stmt
notParser = (do token $ literal "!"
                res <- notParser
                return $ Not res) <||> atoms




atoms :: Parser Stmt
atoms = ints <||> assignParser <||> varibleParser

statements :: Parser Stmt
statements = ifParser <||> elseParser <||> whileParser 

ints :: Parser Stmt
ints = do res <- token $ intParser
          return $ Val res


returnParser :: Parser Stmt
returnParser = do token $ literal "return "
                  res <- orParser
                  token $ literal ";"
                  return $ Ret res

ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              token $ literal "("
              expr <- orParser
              --traceShowM expr
              token $ literal ")"
              token $ literal "{"
              block <- blockParser
              return $ If expr block

elseParser :: Parser Stmt
elseParser = do token $ literal "else"
                token $ literal "{"
                block <- blockParser
                return $ Else block

whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 token $ literal "("
                 expr <- orParser
                 --traceShowM expr
                 token $ literal ")"
                 block <- blockParser
                 --traceShowM block
                 return $ While expr block

-- --bleh

assignParser :: Parser Stmt
assignParser = do varName <- token $ varParser
                  token $ literal "="
                  expr <- condParser
                  return $ Assign varName expr

funcParser :: Parser Stmt
funcParser = do token $ literal "def"
                funcName <- varParser
                --traceShowM funcName
                token $ literal "("
                args <- varParser
                --traceShowM args
                token $ literal ")"
                token $ literal "{"
                block <-blockParser
                return $ Def funcName [args] block

parens :: Parser Program
parens = do token $ literal "("
            res <- parser
            token $ literal ")"
            return res


blockParser :: Parser Stmt
blockParser = (do res <- rep line 
                  token $ literal "}"
                  return $ Block res) <||> statements



x = "if(x==2){x = x + 1;}else{x=x+2;}"
y = "while(3==2){2}"

funcTest = "def foo(x){if(x==2){return y;}"
funcTest2 = "def foo(x){while(x>=2){x = x + 2;}"
funcTest3 = "def foo(x){if(x==2||x==3) {return y;} else{return x;}"
