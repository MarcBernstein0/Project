module Ast where


data Program = P [Stmt]
               deriving Show


data Stmt = Def String [String] Stmt --done
          | While Expr Stmt --done 
          | Block [Stmt] --done 
          | If Expr Stmt --done
          | IfElse Expr Stmt Stmt
          | Assign String Expr
          | Line Expr 
          | Ret Expr --done
          | Print Expr
          | Break
          | Continue 
          deriving Show



data Expr = Val Integer --done
          | Plus Expr Expr --done
          | Sub Expr Expr --done
          | Mult Expr Expr --done
          | Div Expr Expr --done
          | Mod Expr Expr --done
          | Eq Expr Expr --done
          | NEq Expr Expr --done
          | Lt Expr Expr --done
          | LtEq Expr Expr --done
          | Gt Expr Expr --done
          | GtEq Expr Expr --done
          | And Expr Expr --done
          | Or Expr Expr --done
          | Not Expr --done

          | Var String --done
          | Call String [Expr] --done
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