module CParser where

import Ast
import ParserMonad

import Debug.Trace

parser :: Parser Program
parser = undefined

parser' :: Parser Stmt
parser' = undefined







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
                --traceShowM res
                return $ Not res) <||> atoms



atoms :: Parser Stmt
atoms = ints <||> ifParser <||> ifElseParser <||> whileParser

ints :: Parser Stmt
ints = do res <- token $ intParser
          return $ Val res


ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              token $ literal "("
              expr <- orParser
              --traceShowM expr
              token $ literal ")"
              token $ literal "{"
              block <- orParser
              token $ literal "}"
              return $ If expr block

ifElseParser :: Parser Stmt
ifElseParser = do token $ literal "if"
                  token $ literal "("
                  expr <- orParser
                  token $ literal ")"
                  token $ literal "{"
                  blockT <- orParser
                  token $ literal "}"
                  token $ literal "else"
                  token $ literal "{"
                  blockEl <- orParser
                  token $ literal "}"
                  return $ IfElse expr blockT blockEl

whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 token $ literal "("
                 expr <- orParser
                 traceShowM expr
                 token $ literal ")"
                 token $ literal "{"
                 block <- orParser
                 --traceShowM block
                 token $ literal "}"
                 return $ While expr block
                 