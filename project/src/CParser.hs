module CParser where

import Ast
import ParserMonad

import Debug.Trace



test = "def main(){x=0;while(1){x=x+1; if(x==10){break;}}print x; return x;}"

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
atoms = ints <||> funcCall  <||> vars <||> parens

ints :: Parser Expr 
ints = do res <- intParser
          return $ Val res

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






tst = "def main(){return 1;}"


x = "if(x==2){x = x + 1;}else{x=x+2;}"
y = "while(3==2){2}"

funcTest = "def foo(x){if(x==2){return y;}else{x=3;}return bar(x);}"
funcTest2 = "def foo(x){while(x>=2){x = x + 2;}}"
funcTest3 = "def foo(x){if(x==2||x==3) {return y;} }"
funcTest4 = "def foo(x){if(x==2){return y;} if(x==3){return z;}}"
combTest =  funcTest3 ++ tst



test2 = concat [
   "def main() {                    ",
   "    x = 4;                    ",
   "    y = 2;                    ",
   "    z = -1;                    ",
   "    if(x > 2) {                    ",
   "        print x;                    ",
   "    }                    ",
   "    if(y < 2) {                    ",
   "        print y;                    ",
   "    }                    ",
   "    if(z != 2) {                    ",
   "        print z;                    ",
   "    } else {                    ",
   "        print y;                    ",
   "    }                    ",
   "    print z;                    ",
   "    if(z <= y) {                    ",
   "        print x;                    ",
   "        if(x + y > z) {                    ",
   "            print y;                    ",
   "        }                    ",
   "        else {                    ",
   "            print z;                    ",
   "        }                    ",
   "    } else {                    ",
   "        print z;                     ",
   "    }                    ",
   "    return 0;                     ",
   "}                    "]

test3 = concat [
   "def main() {                    ",
   "   k = 1;                    ",
   "   sum = 0;                    ",
   "                    ",
   "   while ( k <= 10 ) {                    ",
   "      sum = sum + k;                    ",
   "      k = k + 1;                    ",
   "   }                    ",
   "                    ",
   "   print sum;                    ",
   "   return 0;                     ",
   "}                    "]

test4 = concat [
   "def main() {                    ",
   "   n = 1;                    ",
   "                    ",
   "   count = 0;                    ",
   "                    ",
   "   while ( n <= 3 ) {                    ",
   "      m = 1;                    ",
   "      while ( m <= 4 ) {                    ",
   "         if ( m % n == 0 ) {                    ",
   "            count = count + 1;                    ",
   "         }                    ",
   "         m = m + 1;                    ",
   "      }                    ",
   "      n = n + 1;                    ",
   "   }                    ",
   "                    ",
   "   print count;                    ",
   "   return 0;                     ",
   "                    ",
   "}                    "]

test5 = concat [
   "def main() {                    ",
   "   count = 0;                    ",
   "   limit = 10;                    ",
   "   n = 2;                       ",
   "                    ",
   "   while (count <= limit) {                     ",
   "      isPrime = 1;                    ",
   "      k = 2;                    ",
   "      while ( k < n ) {                    ",
   "         if (n % k == 0) {                    ",
   "            isPrime = 0;                    ",
   "         }                    ",
   "         k = k + 1;                    ",
   "      }                    ",
   "      if (isPrime==1) {                    ",
   "         print n;                    ",
   "         count = count + 1;                    ",
   "      }                    ",
   "      n = n + 1;                    ",
   "   }                    ",
   "   return 0;                    ",
   "}                    "]