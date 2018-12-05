module CParser where

import Ast
import ParserMonad

import Debug.Trace

parser :: Parser Program
parser = undefined

parser' :: Parser Stmt
parser' = undefined









multDivModParser :: Parser Stmt
multDivModParser = withInfix notParser [("*",Mult), ("/", Div), ("%",Mod)]

notParser :: Parser Stmt
notParser = (do token $ literal "!"
                res <- notParser
                --traceShowM res
                return $ Not res) <||> ints



atoms :: Parser Stmt
atoms = ifParser <||> ifElseParser <||> whileParser

ints :: Parser Stmt
ints = do res <- token $ intParser
          return $ Val res


ifParser :: Parser Stmt
ifParser = do token $ literal "if"
              token $ literal "("
              expr <- ints
              token $ literal "("
              token $ literal "{"
              block <- parser'
              token $ literal "}"
              return $ If expr block

ifElseParser :: Parser Stmt
ifElseParser = do token $ literal "if"
                  token $ literal "("
                  expr <- ints
                  token $ literal "("
                  token $ literal "{"
                  block <- parser'
                  token $ literal "}"
                  token $ literal "else"
                  token $ literal "{"
                  blockEl <- parser'
                  token $ literal "}"
                  return $ IfElse expr Continue Continue

whileParser :: Parser Stmt
whileParser = do token $ literal "while"
                 token $ literal "("
                 expr <- ints
                 token $ literal ")"
                 token $ literal "{"
                 block <- parser'
                 token $ literal "}"
                 return $ While expr block