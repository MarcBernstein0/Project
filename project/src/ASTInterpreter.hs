module ASTInterpreter where

import Ast
import StatefulUnsafeMonad
import Data.Map as Map


type GlobalScope = Map String Stmt -- TODO change to be the type of state, you have freedom for how you implement it
type LocalScope = Map String Integer


test = [Def "main" [] (Block [Ret (Val 1)])]
test2 = [Def "foo" ["x"] (Block [If (Or (Eq (Var "x") (Val 2)) (Eq (Var "x") (Val 3))) (Block [Ret (Var "y")])]),Def "main" [] (Block [Ret (Val 1)])]

-- createGlobal :: [Stmt] -> globalScope
-- createGlobal [] = G $ Map.empty
-- createGlobal ((Def "main" [] ast):rest) = G $ Map.insert ("main",[]) ast Map.empty
-- createGlobal ((Def name params ast):rest) = G $ Map.insert (name, params) ast (createGlobal rest)
-- -- Tried adding parameters to it

-- -- createGlobal :: [Stmt] -> (Map String Stmt) 
-- -- createGlobal [] = Map.empty
-- -- createGlobal ((Def name params ast):rest) = Map.insert name ast (createGlobal rest)

createGlobal :: [Stmt] -> GlobalScope 
createGlobal [] = Map.empty
createGlobal ((Def funcName params ast): rest) = Map.insert funcName ast (createGlobal rest)  


eval :: Program -> Maybe [String]
eval (P program) = eval'

--eval' :: Stmt -> 

-- eval :: Program -> Maybe [String]
-- eval (P program) = eval' (P program) (createGlobal program) Map.empty []
-- -- eval p = getPrints $ snd $ app (eval' p) undefined
-- --
-- -- getPrints :: State -> [String]
-- -- getPrints = undefined
-- --
-- -- eval' :: Program -> StatefulUnsafe State Int
-- -- eval' = undefined

-- --takes a program, buts it itno a 
-- eval' :: Program -> (Map (String, [String]) Stmt) -> (Map String Integer) -> [String] -> Maybe [String]
-- eval' p globalScope localScope strList = case (Map.lookup ("main", []) globalScope) of
--                                             Just ast -> evalStmt ast globalScope localScope strList
--                                             Nothing -> Nothing

-- evalStmt :: Stmt -> (Map (String, [String]) Stmt) -> (Map String Integer) -> [String] -> Maybe [String]
-- evalStmt (Assign varName expr) createGlobal localScope strList = let res = evalExpr expr globalScope localScope 
--                                                                    in  



-- evalExpr :: Expr -> (Map (String, [String]) Stmt) -> (Map String Integer) ->  Maybe Int
-- evalExpr (Val i) globalScope localScope = Just i
-- evalExpr (Plus l r) globalScope localScope = let x = eval l globalScope localScope
--                                                  y = eval r globalScope localScope
