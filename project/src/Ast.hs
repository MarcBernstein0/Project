module Ast where


type Program = [Stmt]


data Stmt = Def String [String] Stmt
          | While Expr Stmt
          | Block [Stmt]
          | If Expr Stmt
          | IfElse Expr Stmt Stmt
          | Assign Expr Expr
          | Id Expr 
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
          | Not Expr Expr

          | Var String
          deriving Show


prettyShow :: Program -> String
prettyShow = undefined