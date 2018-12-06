module CParser where

import Ast
import ParserMonad

import Debug.Trace

parser :: Parser Program
parser = do code <- rep funcParser
            return $ P code

line :: Parser Stmt
line = (do res <- orParser
           token $ literal ";"
           return res) <||> returnParser

contBreakParser :: Parser Stmt
contBreakParser = do res <- token $ (literal "break;") <||> (literal "continue;")
                     if res == "break;"
                      then return Break
                      else return Continue

printParser :: Parser Stmt
printParser = do token $ literal "print"
                 res <- orParser
                 token $ literal ";"
                 return $ Print res


keywords = ["def","return","when","if","then","else","break","continue","print"]


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
statements = ifElseParser <||> ifParser <||> whileParser <||> line <||> contBreakParser <||> printParser

ints :: Parser Stmt
ints = do res <- token $ intParser
          return $ Val res


returnParser :: Parser Stmt
returnParser = do token $ literal "return "
                  res <- condParser
                  token $ literal ";"
                  return $ Ret res

ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              token $ literal "("
              expr <- orParser
              traceShowM expr
              token $ literal ")"
             -- token $ literal "{"
              block <- blockParser
              traceShowM block
              return $ If expr block

ifElseParser :: Parser Stmt
ifElseParser = do token $ literal "if"
                  token $ literal "("
                  expr <- orParser
                  traceShowM expr
                  token $ literal ")"
                  -- token $ literal "{"
                  block <- blockParser
                  traceShowM block
                  token $ literal "else"
                  blockEl <- blockParser
                  return $ IfElse expr block blockEl

whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 token $ literal "("
                 expr <- orParser
                 --traceShowM expr
                 token $ literal ")"
                 -- token $ literal "{"
                 block <- blockParser
                 --traceShowM block
                 return $ While expr block

-- --bleh

assignParser :: Parser Stmt
assignParser = do varName <- token $ varParser
                  token $ literal "="
                  expr <- condParser
                  return $ Assign varName expr


argsParser :: Parser [String]
argsParser = (do var <- varParser
                 token $ literal ","
                 rest <- argsParser
                 return var ++ rest) <||> singleArgsParser

                 
singleArgsParser :: Parser [String]
singleArgsParser = do var <- varParser
                      return [var]

funcParser :: Parser Stmt
funcParser = do token $ literal "def"
                funcName <- varParser
                traceShowM funcName
                token $ literal "("
                args <- varParser 
                traceShowM args
                token $ literal ")"
                -- --token $ literal "{"
                -- block <- blockParser 
                return $ Def funcName [args] Continue

parens :: Parser Program
parens = do token $ literal "("
            res <- parser
            token $ literal ")"
            return res


blockParser :: Parser Stmt
blockParser = (do token $ literal "{"
                  res <- rep statements
                  --traceShowM res
                  token $ literal "}"
                  return $ Block res)



x = "if(x==2){x = x + 1;}else{x=x+2;}"
y = "while(3==2){2}"

funcTest = "def foo(x){if(x==2){return y;}else{x=3;}return x;}"
funcTest2 = "def foo(x){while(x>=2){x = x + 2;}}"
funcTest3 = "def foo(x){if(x==2||x==3) {return y;} }"
funcTest4 = "def foo(x){if(x==2){return y;} if(x==3){return z;}}"