module Ast where


data Program = P [Stmt]
               deriving Show


data Stmt = Def String [String] Stmt
          | While Expr Stmt
          | Block [Stmt]
          | If Expr Stmt
          | IfElse Expr Stmt Stmt
          | Assign String Expr
          | Line Expr 
          | Ret Expr
          | Print Expr
          | Break
          | Continue 
          deriving Show



data Expr = Val Integer 
          | Plus Expr Expr
          | Sub Expr Expr 
          | Mult Expr Expr 
          | Div Expr Expr
          | Mod Expr Expr
          | Eq Expr Expr
          | NEq Expr Expr
          | Lt Expr Expr
          | LtEq Expr Expr
          | Gt Expr Expr
          | GtEq Expr Expr
          | And Expr Expr 
          | Or Expr Expr
          | Not Expr 

          | Var String
          | Call String [Expr]
          deriving Show

 

-- data Stmt = Def String [String] Stmt
--           | While Stmt Stmt
--           | Block [Stmt]
--           | If Stmt Stmt
--           | IfElse Stmt Stmt Stmt
--           | Assign String Stmt
--           | Id Stmt 
--           | Ret Stmt
--           | Print Stmt
--           | Break
--           | Continue

--           | Val Integer 
--           | Plus Stmt Stmt
--           | Sub Stmt Stmt 
--           | Mult Stmt Stmt
--           | Div Stmt Stmt
--           | Mod Stmt Stmt
--           | Eq Stmt Stmt
--           | NEq Stmt Stmt
--           | Lt Stmt Stmt
--           | LtEq Stmt Stmt
--           | Gt Stmt Stmt
--           | GtEq Stmt Stmt
--           | And Stmt Stmt 
--           | Or Stmt Stmt
--           | Not Stmt 

--           | Var String

--           deriving Show

--x = [Def "foo" [] (Block [If (Eq (Val 4) (Val 4) (Block [(Assign (Var "y") (Val 3))]))])]



prettyShow :: Program -> String
prettyShow = undefined