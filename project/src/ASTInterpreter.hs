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


-- eval :: Program -> Maybe [String]
-- eval (P program) = eval'

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



-- run :: Expr -> GlobalScope -> Unsafe Integer
-- run a g = app (evalExpr a g) 


eval :: Program -> Maybe [String]
eval (P program) = eval' program (createGlobal program) [Map.empty] []


eval' :: [Stmt] -> GlobalScope -> [LocalScope] -> [String] -> (Maybe [String])
eval' program g l strList = Nothing



evalStmt :: Stmt -> GlobalScope -> [LocalScope] -> [String] -> Maybe [String]
evalStmt stmt g l strList = undefined


-- evalExpr :: Expr -> GlobalScope -> StatefulUnsafe LocalScope Integer
-- evalExpr (Val i) _ = return i






evalExpr :: Expr -> GlobalScope -> [LocalScope] -> Unsafe Integer
evalExpr _ _ [] = Error "No local scope"
evalExpr (Val i) _ _ = Ok i
evalExpr (Plus l r) global local = let x = evalExpr l global local
                                       y = evalExpr r global local
                                    in case x of Error str -> Error str
                                                 Ok x' -> case y of Error str -> Error str
                                                                    Ok y' -> Ok (x' + y')
evalExpr (Sub l r) global local = let x = evalExpr l global local
                                      y = evalExpr r global local
                                    in case x of Error str -> Error str
                                                 Ok x' -> case y of Error str -> Error str
                                                                    Ok y' -> Ok (x' - y')
evalExpr (Mult l r) global local = let x = evalExpr l global local
                                       y = evalExpr r global local
                                    in case x of Error str -> Error str
                                                 Ok x' -> case y of Error str -> Error str
                                                                    Ok y' -> Ok (x' * y')
evalExpr (Div l r) global local = let x = evalExpr l global local
                                      y = evalExpr r global local
                                    in case x of Error str -> Error str
                                                 Ok x' -> case y of Error str -> Error str
                                                                    Ok y' -> if y' == 0
                                                                             then Error "Cannot divide by 0"
                                                                             else Ok (x' `div` y')
evalExpr (Mod l r) global local = let x = evalExpr l global local
                                      y = evalExpr r global local
                                    in case x of Error str -> Error str
                                                 Ok x' -> case y of Error str -> Error str
                                                                    Ok y' -> if y' == 0
                                                                             then Error "Cannot mod by 0"
                                                                             else Ok (x' `mod` y')
evalExpr (Eq l r) global local = let x = evalExpr l global local
                                     y = evalExpr r global local 
                                 in case x of Error str -> Error str
                                              x' -> case y of Error str -> Error str
                                                              y' -> if x' == y'
                                                                     then Ok 1
                                                                     else Ok 0
evalExpr (NEq l r) global local = let x = evalExpr l global local
                                      y = evalExpr r global local 
                                  in case x of Error str -> Error str
                                               Ok x' -> case y of Error str -> Error str
                                                                  Ok y' -> if x' == y'
                                                                            then Ok 0
                                                                            else Ok 1
evalExpr (Lt l r) global local = let x = evalExpr l global local
                                     y = evalExpr r global local 
                                 in case x of Error str -> Error str
                                              Ok x' -> case y of Error str -> Error str
                                                                 Ok y' -> if x' < y'
                                                                          then Ok 1
                                                                          else Ok 0
evalExpr (LtEq l r) global local = let x = evalExpr l global local
                                       y = evalExpr r global local 
                                   in case x of Error str -> Error str
                                                Ok x' -> case y of Error str -> Error str
                                                                   Ok y' -> if x' <= y'
                                                                            then Ok 1
                                                                            else Ok 0                                                                    
evalExpr (Gt l r) global local = let x = evalExpr l global local
                                     y = evalExpr r global local 
                                   in case x of Error str -> Error str
                                                Ok x' -> case y of Error str -> Error str
                                                                   Ok y' -> if x' > y'
                                                                             then Ok 1
                                                                             else Ok 0 
evalExpr (GtEq l r) global local = let x = evalExpr l global local
                                       y = evalExpr r global local 
                                   in case x of Error str -> Error str
                                                Ok x' -> case y of Error str -> Error str
                                                                   Ok y' -> if x' > y'
                                                                             then Ok 1
                                                                             else Ok 0 

evalExpr (And l r) global local = let x = evalExpr l global local
                                  in case x of Error str -> Error str
                                               Ok 0 -> Ok 0
                                               Ok _ -> let y = evalExpr r global local
                                                        in case y of Error str -> Error str
                                                                     Ok 0 -> Ok 0
                                                                     Ok _ -> Ok 1
evalExpr (Or l r) global local = let x = evalExpr l global local 
                                  in case x of Error str -> Error str
                                               Ok 0 -> let y = evalExpr r global local
                                                       in case y of Error str -> Error str
                                                                    Ok 0 -> Ok 0
                                                                    Ok _ -> Ok 1
                                               Ok _ -> Ok 1
evalExpr (Not val) global local = let x = evalExpr val global local
                                  in case x of Error str -> Error str
                                               Ok 0 -> Ok 1
                                               Ok _ -> Ok 0                                              

--evaluating for function and variable calls
evalExpr (Var str) global (lsope:rest) = let x = Map.lookup str lsope
                                         in case x of Nothing -> Error "Variable not found"
                                                      Just x' -> Ok x'
evalExpr (Call str args) global local = undefined --let newStack 



testEvalExpr = evalExpr (Plus (Div (Val 2) (Val 2)) (Div (Val 8) (Val 0))) Map.empty [Map.empty]
test' = evalExpr (Not (Not (Not (Not (Eq (Val 2) (Val 2)))))) Map.empty [Map.empty]