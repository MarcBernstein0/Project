module ASTInterpreter where

import Ast
import StatefulUnsafeMonad
import Data.Map as Map


data State = Map String Stmt -- TODO change to be the type of state, you have freedom for how you implement it

test = [Def "main" [] (Block [Ret (Val 1)])]
test2 = [Def "foo" ["x"] (Block [If (Or (Eq (Var "x") (Val 2)) (Eq (Var "x") (Val 3))) (Block [Ret (Var "y")])]),Def "main" [] (Block [Ret (Val 1)])]

createGlobal :: [Stmt] -> (Map (String, [String]) Stmt)
createGlobal [] = Map.empty
createGlobal ((Def "main" [] ast):rest) = Map.insert ("main",[]) ast Map.empty
createGlobal ((Def name params ast):rest) = Map.insert (name, params) ast (createGlobal rest)



eval :: Program -> Maybe [String]
eval (P program) = eval' (P program) (createGlobal program) Map.empty []
-- eval p = getPrints $ snd $ app (eval' p) undefined
--
-- getPrints :: State -> [String]
-- getPrints = undefined
--
-- eval' :: Program -> StatefulUnsafe State Int
-- eval' = undefined

--takes a program, buts it itno a 
eval' :: Program -> (Map (String, [String]) Stmt) -> (Map String Integer) -> [String] -> Maybe [String]
eval' p x y z = Nothing

evalStmt :: Stmt -> (Map String Stmt) -> (Map String Integer) -> [String] -> Maybe [String]
evalStmt = undefined



evalExpr :: Expr -> (Map String Stmt) -> (Map String Integer) ->  Maybe Int
evalExpr = undefined