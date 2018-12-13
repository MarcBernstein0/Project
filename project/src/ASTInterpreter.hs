module ASTInterpreter where

import Ast
import StatefulUnsafeMonad
import CParser
import Debug.Trace
import Data.Map as Map



type GlobalScope = Map String ([String],Stmt) -- TODO change to be the type of state, you have freedom for how you implement it
type LocalScope = Map String Integer

type State = Map String ([String], Stmt, [LocalScope], [String])



tt = P [Def "main" [] (Block [Assign "x" (Val 3),Ret (Val 3),Print (Plus (Var "x") (Val 1))])]



testNegate = (P [Def "main" [] (Block [Assign "x" (Val 6),Assign "y" (Val 8),Assign "z" (Plus (Plus (Div (Mult (Var "x") (Var "y")) (Val 3)) (VarNeg "x")) (Mult (Val 2) (Var "y"))),Assign "w" (Sub (Var "z") (Mod (Var "x") (Sub (Var "x") (Val 2)))),Print (Var "w"),Ret (Val 0)])])
test = [Def "main" [] (Block [Ret (Val 1)])]
test' = P [Def "f" ["x"] (Block [Ret (Mult (Var "x") (Val 10))]),Def "main" [] (Block [Assign "x" (Val 10),Assign "x" (Call "f" [Var "x"]), Print (Var "x")])]
test'' = P [Def "f" [] (Block [Ret (Val 100)]),Def "main" [] (Block [Assign "x" (Call "f" []),Print (Var "x"),Ret (Var "x")])]
test''' = P [Def "main" [] (Block [Assign "x" (Val 4),Assign "y" (Val 2),Assign "z" (Val (-1)),If (Gt (Var "x") (Val 2)) (Block [Print (Var "x")]),If (Lt (Var "y") (Val 2)) (Block [Print (Var "y")]),IfElse (NEq (Var "z") (Val 2)) (Block [Print (Var "z")]) (Block [Print (Var "y")]),Print (Var "z"),IfElse (LtEq (Var "z") (Var "y")) (Block [Print (Var "x"),IfElse (Gt (Plus (Var "x") (Var "y")) (Var "z")) (Block [Print (Var "y")]) (Block [Print (Var "z")])]) (Block [Print (Var "z")]),Ret (Val 0)])]
test'''' = P [Def "main" [] (Block [Assign "x" (Val 4), If (Gt (Var "x") (Val 2))(Block [Print (Var "x")]),If (NEq (Var "x") (Val 2))(Block [Print (Var "x")])])]
testLoop = P [Def "main" [] (Block [Assign "k" (Val 1),Assign "sum" (Val 0),While (LtEq (Var "k") (Val 10)) (Block [Assign "sum" (Plus (Var "sum") (Var "k")),Assign "k" (Plus (Var "k") (Val 1))]),Print (Var "sum"),Ret (Val 0)])]
testNestLoop = P [Def "main" [] (Block [Assign "n" (Val 1),Assign "count" (Val 0),While (LtEq (Var "n") (Val 3)) (Block [Assign "m" (Val 1),While (LtEq (Var "m") (Val 4)) (Block [If (Eq (Mod (Var "m") (Var "n")) (Val 0)) (Block [Assign "count" (Plus (Var "count") (Val 1))]),Assign "m" (Plus (Var "m") (Val 1))]),Assign "n" (Plus (Var "n") (Val 1))]),Print (Var "count"),Ret (Val 0)])]
test10Prime = P [Def "main" [] (Block [Assign "count" (Val 0),Assign "limit" (Val 10),Assign "n" (Val 2),While (LtEq (Var "count") (Var "limit")) (Block [Assign "isPrime" (Val 1),Assign "k" (Val 2),While (Lt (Var "k") (Var "n")) (Block [If (Eq (Mod (Var "n") (Var "k")) (Val 0)) (Block [Assign "isPrime" (Val 0)]),Assign "k" (Plus (Var "k") (Val 1))]),If (Eq (Var "isPrime") (Val 1)) (Block [Print (Var "n"),Assign "count" (Plus (Var "count") (Val 1))]),Assign "n" (Plus (Var "n") (Val 1))]),Ret (Val 0)])]
test2 = [Def "foo" ["x"] (Block [If (Or (Eq (Var "x") (Val 2)) (Eq (Var "x") (Val 3))) (Block [Print (Var "x")])]),Def "main" [] (Block [Ret (Val 1)])]
testShortCircuit = P [Def "main" [] (Block [Assign "x" (Val 3),Assign "y" (Val 5),Assign "z" (Val 1),If (And (Gt (Var "x") (Val 2)) (Lt (Var "y") (Val 5))) (Block [Print (Var "x")]),If (Or (Gt (Var "x") (Val 2)) (Lt (Var "y") (Val 5))) (Block [Print (Var "x")]),If (Or (And (Gt (Var "x") (Val 2)) (Lt (Var "y") (Val 5))) (Not (Eq (Var "z") (Val 2)))) (Block [Print (Var "x")]),If (Or (NEq (Var "z") (Val 2)) (And (Gt (Var "x") (Val 2)) (Lt (Var "y") (Val 5)))) (Block [Print (Var "x")]),Ret (Val 0)])]
testFuncCall = P [Def "f" ["x"] (Block [Assign "n" (Plus (Var "x") (Val 1)),Assign "n" (Mult (Var "n") (Val 2)),Ret (Var "n")]),Def "main" [] (Block [Assign "y" (Val 2),Assign "n" (Val 3),Assign "z" (Call "f" [Plus (Var "y") (Var "n")]),Print (Var "z"),Ret (Val 0)])]
testMultCall = P [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "a" (Val 1),Assign "z" (Plus (Call "succ" [Var "a"]) (Mult (Call "succ" [Plus (Var "a") (Val 1)]) (Call "succ" [Mult (Var "a") (Val 2)]))),Print (Var "z"),Ret (Val 0)])]
testNestedCall =P [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "a" (Val 5),Assign "z" (Call "succ" [Call "succ" [Call "succ" [Var "a"]]]),Print (Var "z"),Ret (Val 0)])]
testFuncsCallFuncs=P [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "times2" ["x"] (Block [Ret (Mult (Var "x") (Val 2))]),Def "f" ["y"] (Block [Assign "z" (Call "succ" [Var "y"]),Assign "y" (Call "times2" [Var "z"]),Ret (Var "y")]),Def "main" [] (Block [Assign "z" (Call "f" [Val 10]),Print (Var "z"),Ret (Val 0)])]
testGCD = P [Def "gcd" ["b"] (Block [Assign "a" (Val 2854),While (NEq (Var "b") (Val 0)) (Block [Assign "t" (Var "b"),Assign "b" (Mod (Var "a") (Var "b")),Assign "a" (Var "t")]),Ret (Var "a")]),Def "main" [] (Block [Assign "m" (Val 264),Assign "res" (Call "gcd" [Var "m"]),Print (Var "res"),Ret (Val 0)])]


test12 = P [Def "Q" ["n"] (Block [IfElse (LtEq (Var "n") (Val 2)) (Block [Ret (Val 1)]) (Block [Ret (Plus (Call "Q" [Sub (Var "n") (Call "Q" [Sub (Var "n") (Val 1)])]) (Call "Q" [Sub (Var "n") (Call "Q" [Sub (Var "n") (Val 2)])]))])]),Def "main" [] (Block [Assign "k" (Val 1),While (Lt (Var "k") (Val 20)) (Block [Assign "q" (Call "Q" [Var "k"]),Print (Var "q"),Assign "k" (Plus (Var "k") (Val 1))]),Ret (Val 0)])]




testingRecursion =  [Def "fib" ["x"] (Block [IfElse (Or (Eq (Var "x") (Val 1)) (Eq (Var "x") (Val 0))) (Block [Ret (Val 15)]) (Block [Assign "x" (Call "fib" [Sub (Var "x") (Val 1)]),Ret (Var "x")])]),Def "main" [] (Block [Assign "k" (Call "fib" [Val 15]),Print (Var "k"),Ret (Val 0)])]

testingMultiArgs = P [Def "f" ["x","y"] (Block [Ret (Plus (Div (Var "x") (Var "y")) (Val 2))]),Def "main" [] (Block [Assign "x" (Call "f" [Val 3,Val 3]), Print (Var "x"), Ret (Val 0)])]

-- testingRecCallInRet = [Def "fib" ["x"] (Block [IfElse (Or (Eq (Var "x") (Val 1)) (Eq (Var "x") (Val 0))) (Block [Ret (Val 1)]) (Block [Ret (Plus (Call "fib" [Sub (Var "x") (Val 1)]) (Call "fib" [Sub (Var "x") (Val 2)]))])]),Def "main" [] (Block [Assign "k" (Call "fib" [Val 3]),Print (Var "k"),Ret (Val 0)])]

-- testQ = P [Def "Q" ["n"] (Block [IfElse (LtEq (Var "n") (Val 2)) (Block [Ret (Val 1)]) (Block [Ret (Plus (Call "Q" [Sub (Var "n") (Call "Q" [Sub (Var "n") (Val 1)])]) (Call "Q" [Sub (Var "n") (Call "Q" [Sub (Var "n") (Val 2)])]))])]),Def "main" [] (Block [Assign "k" (Val 1),While (LtEq (Var "k") (Val 20)) (Block [Assign "q" (Call "Q" [Var "k"]),Print (Var "q"),Assign "k" (Plus (Var "k") (Val 1))]),Ret (Val 0)])


createState :: [Stmt] -> State
createState [] = Map.empty
createState ((Def funcName params ast):rest) = Map.insert funcName (params, ast, [], []) (createState rest)





createLocal :: [(String,Integer)] -> LocalScope
createLocal [] = Map.empty
createLocal ((var, args):rest) = Map.insert var args (createLocal rest)

filterList :: [Unsafe Integer] -> [Integer]
filterList lst = [x | Ok x <- lst] 


getLocalScope :: String -> State -> Maybe LocalScope
getLocalScope funcName state = let local = Map.lookup funcName state in
                                case local of
                                    Nothing -> Nothing
                                    Just (_,_,[],_) -> Nothing
                                    Just (_,_,(x:xs),_) -> Just x

data StmtRet = Nil | RetVal Integer | RetPass | RetBreak | RetCont  deriving Show


-- runEverything :: IO() -> IO()
-- runEverything = 

run :: Program -> IO()
run program = let res = run' program in
                case res of
                  (Error str,_) -> putStrLn str
                  (Ok output,_) -> mapM_ putStrLn $ output


run' :: Program -> (Unsafe [String], State)
run' program = r (eval program) Map.empty


run'' :: (String, [Expr]) -> (Unsafe [Integer], State)
run'' (name, a) = r (evalArgs (name, a)) Map.empty


run''' :: (String , Expr) -> (Unsafe Integer, State)
run''' (name, a) = r (evalExpr (name, a)) (createState $ [Def "foo" ["x","y"] (Block [Ret (Plus (Var "x") (Var "y"))])])

eval :: Program -> StatefulUnsafe State [String]
eval (P code) = let state = createState code in
                  do put state
                     let start = Map.lookup "main" state in
                      case start of
                        Nothing -> err "No main function"
                        Just (p,a,l,str) -> do res <- evalProgram ("main",[a])
                                               --traceShowM $ "result of program " ++ (show res)
                                               newState <- get
                                               let start = Map.lookup "main" newState in
                                                case start of
                                                  Nothing -> err "No main function"
                                                  Just (p,a,l,str) -> do return str





evalProgram :: (String, [Stmt]) -> StatefulUnsafe State StmtRet
evalProgram (funcName, []) = return Nil
evalProgram (funcName, (head:tail)) = do res1 <- evalStmt (funcName, head)
                                         -- traceShowM (head:tail)
                                         -- traceShowM $ (show res1) ++ " in prgram"
                                         case res1 of
                                          Nil -> return Nil
                                          RetPass -> evalProgram (funcName, tail)
                                          RetVal i -> return $ RetVal i
                                          RetBreak -> do return RetBreak
                                          RetCont -> return RetCont
                                         



evalStmt :: (String, Stmt) -> StatefulUnsafe State StmtRet
evalStmt (funcName, (Block code)) = do res <- evalProgram (funcName, code)
                                       -- traceShowM $ "block evalStmt " ++ (show code)
                                       -- traceShowM $ "block being hit " ++ (show res)
                                       return res
evalStmt (funcName, Ret expr) = do res <- evalExpr (funcName, expr)
                                   -- traceShowM "Starting return call"
                                   -- traceShowM res
                                   state <- get 
                                   let funcState = Map.lookup funcName state in 
                                    case funcState of
                                      Nothing -> err "Function does not exist"
                                      Just (p,a,l,strLst) -> let updateLocal = drop 1 l
                                                                 updateState = Map.insert funcName (p,a,updateLocal,strLst) state in 
                                                              do put updateState
                                                                 newState <- get 
                                                                 -- traceShowM $ "Final local " ++ (show l)
                                                                 -- traceShowM $ "Updated local " ++ (show res)
                                                                 -- traceShowM $ "returning " ++ (show updateLocal) 
                                                                 return $RetVal res
evalStmt (funcName, (While expr code)) = do cond <- evalExpr (funcName, expr)
                                            --traceShowM $ "condition for while " ++ (show cond)
                                            if cond /= 0
                                                then do res <- evalStmt (funcName, code)
                                                        case res of
                                                          RetBreak -> return RetPass
                                                          RetCont -> evalStmt (funcName, While expr code)
                                                          otherwise -> evalStmt (funcName, While expr code)
                                                else return RetPass
evalStmt (funcName, (If expr code)) = do cond <- evalExpr (funcName, expr)
                                         --traceShowM $ "what is code " ++ (show code)
                                         if cond /= 0
                                             then do evalStmt (funcName, code)
                                                     return RetPass
                                             else return RetPass
evalStmt (funcName, (IfElse expr blockT blockF)) = do cond <- evalExpr (funcName, expr)
                                                      if cond /= 0
                                                        then do res <- evalStmt (funcName, blockT)
                                                                -- t       raceShowM $ "Result of if block " ++ (show res)
                                                                case res of 
                                                                  RetVal i -> do return $ RetVal i 
                                                                  Nil -> do return Nil
                                                                  otherwise -> do return RetPass 
                                                        else do res <- evalStmt (funcName, blockF)
                                                                -- traceShowM $ "Result of else block " ++ (show res)
                                                                case res of 
                                                                  RetVal i -> do return $ RetVal i 
                                                                  Nil -> do return Nil
                                                                  otherwise -> do return RetPass 
evalStmt (funcName, (Assign var val)) = do res <- evalExpr (funcName, val)
                                           -- traceShowM $ (show res) ++ " assigning function"
                                           state <- get 
                                           let funcState = Map.lookup funcName state in 
                                            do case funcState of
                                                Nothing -> err "Function does not exist"
                                                Just (p,a,(lc:rest),strLst) -> let newLc = Map.insert var res lc 
                                                                                   newState = Map.insert funcName (p, a, (newLc:rest), strLst) state
                                                                                in do put newState
                                                                                      -- state' <- get 
                                                                                      -- traceShowM state'
                                                                                      return RetPass
                                                Just (p,a,[],strLst) -> let newLc = Map.insert var res Map.empty
                                                                            newState = Map.insert funcName (p,a,[newLc],strLst) state
                                                                         in do put newState
                                                                               -- state' <- get 
                                                                               -- traceShowM state'
                                                                               return RetPass
evalStmt (funcName, (Print expr)) = do res <- evalExpr (funcName, expr)
                                       --traceShowM res
                                       state <- get 
                                       let funcState = Map.lookup "main" state in 
                                        case funcState of
                                          Nothing -> err "Function does not exist"
                                          Just (p,a,lc,strLst) -> let newPrint = strLst ++ [(show res)] 
                                                                      newState = Map.insert "main" (p,a,lc,newPrint) state
                                                                  in do put newState
                                                                        return RetPass
evalStmt (funcName, (Line expr)) = do res <- evalExpr (funcName, expr)
                                      return RetPass
evalStmt (funcName, (Break)) = return RetBreak
evalStmt (funcName, (Continue)) = return RetCont

  
                                            

                                                
                                         


evalExpr :: (String, Expr) -> StatefulUnsafe State Integer
evalExpr (name, (Val i)) = return i
evalExpr (name, (Plus l r)) = do x <- evalExpr (name, l)
                                 y <- evalExpr (name, r)
                                 return $ x + y
evalExpr (name, (Sub l r)) = do x <- evalExpr (name, l)
                                y <- evalExpr (name, r)
                                return $ x - y
evalExpr (name, (Mult l r)) = do x <- evalExpr (name, l)
                                 y <- evalExpr (name, r)
                                 return $ x * y
evalExpr (name, (Div l r)) = do x <- evalExpr (name, l)
                                y <- evalExpr (name, r)
                                if y == 0
                                    then err "Cannot divide by 0"
                                    else return $ x `div` y
evalExpr (name, (Mod l r)) = do x <- evalExpr (name, l)
                                y <- evalExpr (name, r)
                                if y == 0
                                    then err "Cannot divide by 0"
                                    else return $ x `mod` y
evalExpr (name, (Eq l r)) = do x <- evalExpr (name, l)
                               y <- evalExpr (name, r)
                               if x == y
                                then return 1
                                else return 0
evalExpr (name, (NEq l r)) = do x <- evalExpr (name, l)
                                y <- evalExpr (name, r)
                                if x == y
                                 then return 0
                                 else return 1
evalExpr (name, (Lt l r)) = do x <- evalExpr (name, l) 
                               y <- evalExpr (name, r)
                               if x < y
                                then return 1
                                else return 0
evalExpr (name, (LtEq l r)) = do x <- evalExpr (name, l) 
                                 y <- evalExpr (name, r)
                                 if x <= y
                                  then return 1
                                  else return 0   
evalExpr (name, (Gt l r)) = do x <- evalExpr (name, l) 
                               y <- evalExpr (name, r)
                               if x > y
                                then return 1
                                else return 0
evalExpr (name, (GtEq l r)) = do x <- evalExpr (name, l) 
                                 y <- evalExpr (name, r)
                                 if x >= y
                                  then return 1
                                  else return 0
evalExpr (name, (And l r)) = do x <- evalExpr (name, l)
                                case x of
                                    0 -> return 0
                                    _ -> do y <- evalExpr (name, r)
                                            case y of 
                                                0 -> return 0
                                                _ -> return 1
evalExpr (name, (Or l r)) = do x <- evalExpr (name, l) 
                               case x of
                                0 -> do y <- evalExpr (name, r) 
                                        case y of
                                          0 -> return 0
                                          _ -> return 1
                                _ -> return 1
evalExpr (name, (Not expr)) = do x <- evalExpr (name, expr)
                                 case x of
                                    0 -> return 1
                                    _ -> return 0
evalExpr (name, (VarNeg var)) = do cState <- get
                                   let local = getLocalScope name cState in 
                                    case local of 
                                      Nothing -> err "Function does not exist"
                                      Just a -> case Map.lookup var a of
                                                  Just val -> do return $ -val
                                                  Nothing -> err "Variable does not exist"
evalExpr (name, (Var var)) = do cState <- get
                                --traceShowM $ "Looking up variable " ++ var
                                let local = getLocalScope name cState in 
                                    case local of
                                        Nothing -> err "Function does not exist"
                                        Just a -> case Map.lookup var a of 
                                                   Just val -> do return val
                                                   Nothing -> err "Variable does not exist"
evalExpr (name, (Call func [])) = do cState <- get
                                         --traceShowM cState
                                     let getFunc = Map.lookup func cState in
                                       case getFunc of
                                        Nothing -> err $ "Function " ++ func ++ " does not exist"
                                        Just (p,a,l,strLst) -> do res <- evalProgram (func, [a])
                                                                  --traceShowM $ "Result of func call " ++ (show res)
                                                                  case res of
                                                                   RetVal i -> do return i
                                                                   otherwise -> err "Nothing returned"
evalExpr (name, (Call func expr)) = do cState <- get
                                       --traceShowM "Call function"
                                       let getFunc = Map.lookup func cState in
                                         case getFunc of
                                          Nothing -> err $ "Function " ++ func ++ " does not exist"
                                          Just (p,a,l,strLst) -> do res <- evalArgs (name, expr)
                                                                    -- traceShowM l
                                                                    --traceShowM res
                                                                    let newLocal = createLocal (zip p res) 
                                                                        newState = Map.insert func (p,a,(newLocal:l),strLst) cState
                                                                     in do put newState
                                                                           --traceShowM $ "Result of func call " ++ (show newLocal)
                                                                           res <- evalProgram (func, [a])
                                                                           --traceShowM res
                                                                           case res of
                                                                            RetVal i -> do return i
                                                                            otherwise -> err "Nil was hit"
                                                                       

evalArgs :: (String, [Expr]) -> StatefulUnsafe State [Integer]
evalArgs (name, []) = return []
evalArgs (name, (x:xs)) = do res <- evalExpr (name, x)
                             --traceShowM res
                             rest <- evalArgs (name, xs)
                             return $ [res]++rest
                                        
-- setLocal :: (String, [Expr]) -> State -> [Integer]
-- setLocal (name, []) state = []
-- setLocal (name, (x:xs)) state = let res = evalExpr (name, x) in
--                                  case res of
--                                   res -> res

