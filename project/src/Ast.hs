module Ast where


data Program = P [Stmt]
               deriving Show


data Stmt = Def String [String] Stmt --done done
          | While Expr Stmt --done done
          | Block [Stmt] --done done
          | If Expr Stmt --done done
          | IfElse Expr Stmt Stmt --done
          | Assign String Expr --done done
          | Line Expr --done done
          | Ret Expr --done done
          | Print Expr --done done
          | Break --done done
          | Continue --done done
          deriving Show



data Expr = Val Integer --done done
          | Plus Expr Expr --done done
          | Sub Expr Expr --done done
          | Mult Expr Expr --done done
          | Div Expr Expr --done done
          | Mod Expr Expr --done done
          | Eq Expr Expr --done done
          | NEq Expr Expr --done done
          | Lt Expr Expr --done done
          | LtEq Expr Expr --done done
          | Gt Expr Expr --done done
          | GtEq Expr Expr --done done
          | And Expr Expr --done done
          | Or Expr Expr --done done
          | Not Expr --done done

          | Var String --done done
          | UnaryMinus Expr --done done
          | Call String [Expr] --done
          deriving Show

 
prettyShow :: Program -> String
prettyShow (P (head:rest)) = prettyStmt head ++ "\n" ++ (prettyShow (P rest))


prettyStmt :: Stmt -> String
prettyStmt (Def name (x:xs) body) = undefined
prettyStmt (While cond body) = "While " ++ showPar (prettyExpr cond) ++ "\n\t" ++ prettyStmt body
prettyStmt (Block (first:rest)) = undefined
prettyStmt (If cond body) = "If" ++ showPar (prettyExpr cond) ++ "\n\t" ++ prettyStmt body 
prettyStmt (IfElse cond true false) = "If" ++ showPar (prettyExpr cond) ++ "\n\t" ++ prettyStmt true ++ "\nElse\n\t" ++ prettyStmt false 
prettyStmt (Assign var body) = var ++ "=" ++ prettyExpr body ++ ";"
prettyStmt (Line body) = undefined
prettyStmt (Ret body) = "return " ++ prettyExpr body
prettyStmt (Print body) = "print(" ++ prettyExpr body ++")"
prettyStmt (Break) = "Break"
prettyStmt (Continue) = "Continue"

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



