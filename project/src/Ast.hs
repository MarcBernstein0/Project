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
          | Call String [Expr] 
          deriving Show

 
prettyShow :: Program -> String
prettyShow = undefined


prettyStmt :: Stmt -> String
prettyStmt (Def name vars body) = undefined
prettyStmt (While cond body) = undefined
prettyStmt (Block body) = undefined
prettyStmt (If cond body) = undefined
prettyStmt (IfElse cond true false) = undefined
prettyStmt (Assign var body) = undefined
prettyStmt (Line body) = undefined
prettyStmt (Ret body) = undefined
prettyStmt (Print body) = undefined
prettyStmt (Break) = undefined
prettyStmt (Continue) = undefined

prettyExpr :: Expr -> String
prettyExpr (Val i) = show i
prettyExpr (Plus x y) = showPar (prettyExpr x) ++ " + " ++ showPar (prettyExpr y)
prettyExpr (Sub x y) = showPar (prettyExpr x) ++ " - " ++ showPar (prettyExpr y)
prettyExpr (Mult x y) = showPar (prettyExpr x) ++ " * " ++ showPar (prettyExpr y)
prettyExpr (Div x y) = showPar (prettyExpr x) ++ " / " ++ showPar (prettyExpr y)
prettyExpr (Mod x y) = showPar (prettyExpr x) ++ " % " ++ showPar (prettyExpr y)
prettyExpr (Eq x y) = showPar (prettyExpr x) ++ " == " ++ showPar (prettyExpr y)
prettyExpr (NEq x y) = showPar (prettyExpr x) ++ " != " ++ showPar (prettyExpr y)
prettyExpr (Lt x y) = showPar (prettyExpr x) ++ " < " ++ showPar (prettyExpr y)
prettyExpr (LtEq x y) = showPar (prettyExpr x) ++ " <= " ++ showPar (prettyExpr y) 
prettyExpr (Gt x y) = showPar (prettyExpr x) ++ " > " ++ showPar (prettyExpr y) 
prettyExpr (GtEq x y) = showPar (prettyExpr x) ++ " >= " ++ showPar (prettyExpr y)
prettyExpr (And x y) = showPar (prettyExpr x) ++ " && " ++ showPar (prettyExpr y)
prettyExpr (Or x y) = showPar (prettyExpr x) ++ " || " ++ showPar (prettyExpr y)
prettyExpr (Not x) = prettyExpr x 
prettyExpr (Var str) = show str
prettyExpr (Call str lst) = undefined --show str ++ prettyExpr lst


showPar :: String -> String
showPar s = "(" ++ s ++ ")"

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



