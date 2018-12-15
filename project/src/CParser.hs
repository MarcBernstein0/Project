module CParser where

import Ast
import ParserMonad
import TestsProject

import Debug.Trace


parser :: Parser Program
parser = do code <- rep functionParser
            return $ P code


functionParser :: Parser Stmt
functionParser = do token $ literal "def"
                    name <- varParser
                    --traceShowM name
                    token $ literal "("
                    argsFunc <- params
                    --traceShowM argsFunc
                    token $ literal ")"
                    block <- blockParser
                    return $ Def name argsFunc block


paramSingle :: Parser String
paramSingle = do arg <- varParser
                 token $ (literal "," <||> literal "")
                 return arg


params :: Parser [String]
params = do arg <- rep paramSingle
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
atoms = ints <||> funcCall  <||> unaryMinus <||> vars <||> parens

ints :: Parser Expr 
ints = do res <- intParser
          return $ Val res


-- varsNeg :: Parser Expr
-- varsNeg = (do token $ literal "-"
--               res <- varParser 
--               return $ VarNeg res) 

unaryMinus :: Parser Expr
unaryMinus = (do token $ literal "-"
                 expr <- token $ orParser
                 return $ UnaryMinus expr) <||>
              (do token $ literal "-"
                  expr <- token $ vars
                  return $ UnaryMinus expr)

vars :: Parser Expr
vars = do var <- varParser
          if var `elem` keyWords
            then failParse
            else return $ Var var

parens :: Parser Expr
parens = do token $ literal "("
            res <- orParser
            token $ literal ")"
            return res 

funcCall :: Parser Expr
funcCall = do name <- varParser
              --traceShowM name
              token $ literal "("
              args <- rep argSingle
              --traceShowM args
              token $ literal ")"
              return $ Call name args


argSingle :: Parser Expr
argSingle = do arg <- orParser
               token $ literal "," <||> literal ""
               return arg



statments :: Parser Stmt
statments = ifElseParser <||> ifParser <||> whileParser <||> printParser <||> contBreakParser <||>assignParser <||> line <||> returnParser
 

ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              token $ literal "("
              expr <- orParser
              token $ literal ")"
              block <- blockParser 
              return $ If expr block


ifElseParser :: Parser Stmt
ifElseParser = do token $ literal "if"
                  traceShowM "started parser"
                  token $ literal "("
                  expr <- orParser
                  traceShowM expr
                  token $ literal ")"
                  block <- blockParser
                  traceShowM block
                  token $ literal "else"
                  blockF <- blockParser 
                  return $ IfElse expr block blockF


whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 token $ literal "("
                 expr <- orParser 
                 traceShowM expr
                 token $ literal ")"
                 block <- blockParser
                 return $ While expr block


keyWords = ["continue", "break", "print", "while", "return", "if", "else", "def"]


assignParser :: Parser Stmt
assignParser = do varName <- varParser
                  --traceShowM $ "Variable name " ++ varName
                  if varName `elem` keyWords
                    then failParse
                    else do token $ literal "="
                            var <- orParser 
                            --traceShowM var
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
                 token $ literal ";"
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





