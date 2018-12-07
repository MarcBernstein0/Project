module ASTInterpreter where

import Ast
import StatefulUnsafeMonad
import Data.Map as Map


data State = Map String Stmt -- TODO change to be the type of state, you have freedom for how you implement it



createGlobal :: Program -> (Map String Stmt)
createGlobal p = undefined



eval :: Program -> [String]
eval p = undefined -- eval' p empty
-- eval p = getPrints $ snd $ app (eval' p) undefined
--
-- getPrints :: State -> [String]
-- getPrints = undefined
--
-- eval' :: Program -> StatefulUnsafe State Int
-- eval' = undefined

--takes a program, buts it itno a 
eval' :: Program -> (Map String Stmt) -> (Map String Integer) -> [String] -> Maybe [String]
eval' = undefined

evalStmt :: Stmt -> (Map String Stmt) -> (Map String Integer) -> [String] -> Maybe [String]
evalStmt = undefined



evalExpr :: Expr -> (Map String Stmt) -> (Map String Integer) ->  Maybe Int
evalExpr = undefined