module ASTInterpreter where

import Ast
import StatefulUnsafeMonad
import Debug.Trace
import Data.Map as Map


type GlobalScope = Map String ([String],Stmt) -- TODO change to be the type of state, you have freedom for how you implement it
type LocalScope = Map String Integer

type State = Map String ([String], Stmt, [LocalScope], [String])

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
testRec = P [Def "fib" ["x"] (Block [IfElse (Or (Eq (Var "n") (Val 1)) (Eq (Var "n") (Val 0))) (Block [Ret (Val 1)]) (Block [Assign "x" (Plus (Call "fib" [Sub (Var "x") (Val 1)]) (Call "fib" [Sub (Var "x") (Val 2)])),Ret (Var "x")])]),Def "main" [] (Block [Assign "k" (Call "fib" [Val 3]),Print (Var "k"),Ret (Val 0)])]

testFib = P [Def "fib" ["x"] (Block [IfElse (Or (Eq (Var "x") (Val 1)) (Eq (Var "x") (Val 0))) (Block [Ret (Val 1)]) (Block [Assign "x" (Plus (Call "fib" [Sub (Var "x") (Val 1)]) (Call "fib" [Sub (Var "x") (Val 2)])),Ret (Var "x")])]),Def "main" [] (Block [Assign "k" (Call "fib" [Val 3]),Print (Var "k"),Ret (Val 0)])]

testClean = P [Def "f" ["x"] (Block [Assign "x" (Plus (Var "x") (Val 1)),Ret (Var "x")]),Def "main" [] (Block [Assign "x" (Val 0),Print (Plus (Call "f" [Var "x"]) (Call "f" [Var "x"]))])]



testing' = P [Def "main" [] (Block [Assign "x" (Val 0),IfElse (NEq (Var "x") (Val 0)) (Block [Ret (Val 1)]) (Block [Print (Var "x"),Ret (Val 100)])])]



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
createGlobal ((Def funcName params ast): rest) = Map.insert funcName (params, ast) (createGlobal rest)  


createState :: [Stmt] -> State
createState [] = Map.empty
createState ((Def funcName params ast):rest) = Map.insert funcName (params, ast, [], []) (createState rest)


-- testGlob = createState test


createLocal :: [(String,Integer)] -> LocalScope
createLocal [] = Map.empty
createLocal ((var, args):rest) = Map.insert var args (createLocal rest)

filterList :: [Unsafe Integer] -> [Integer]
filterList lst = [x | Ok x <- lst] 

-- testing = evalExpr (Call "foo" [(Plus (Val 2) (Val 8))]) (createGlobal test2) [Map.empty]
-- testing' = evalExpr (Call "foo" [(Div (Val 2) (Val 0))]) (createGlobal test2) [Map.empty]

-- testEvalExpr = evalExpr (Plus (Div (Val 2) (Val 2)) (Div (Val 8) (Val 0))) Map.empty [Map.empty]
-- test' = evalExpr (Not (Not (Not (Not (Eq (Val 2) (Val 2)))))) Map.empty [Map.empty]

getLocalScope :: String -> State -> Maybe LocalScope
getLocalScope funcName state = let local = Map.lookup funcName state in
                                case local of
                                    Nothing -> Nothing
                                    Just (_,_,[],_) -> Nothing
                                    Just (_,_,(x:xs),_) -> Just x

data StmtRet = Nil | RetVal Integer | RetPass | RetBreak | RetCont  deriving Show


eval :: Program -> StatefulUnsafe State [String]
eval (P code) = let state = createState code in
                  do put state
                     let start = Map.lookup "main" state in
                      case start of
                        Nothing -> err "No main function"
                        Just (p,a,l,str) -> do evalProgram ("main",[a])
                                               newState <- get
                                               let start = Map.lookup "main" newState in
                                                case start of
                                                  Nothing -> err "No main function"
                                                  Just (p,a,l,str) -> do traceShowM str
                                                                         return str



-- getStrings :: String -> State -> StatefulUnsafe State [String]
-- getStrings name state = return err


evalProgram :: (String, [Stmt]) -> StatefulUnsafe State StmtRet
evalProgram (funcName, []) = return Nil
evalProgram (funcName, (head:tail)) = do res1 <- evalStmt (funcName, head)
                                         -- traceShowM (head:tail)
                                         -- traceShowM $ (show res1) ++ " in prgram"
                                         case res1 of
                                          Nil -> return Nil
                                          RetPass -> evalProgram (funcName, tail)
                                          RetVal i -> return $ RetVal i
                                          RetBreak -> do traceShowM $"Tracing for tail " ++ (show tail)
                                                         return RetBreak
                                          RetCont -> return RetCont
                                         



evalStmt :: (String, Stmt) -> StatefulUnsafe State StmtRet
evalStmt (funcName, (Block code)) = do res <- evalProgram (funcName, code)
                                       --traceShowM $ "block evalStmt " ++ (show code)
                                       return res
evalStmt (funcName, Ret expr) = do res <- evalExpr (funcName, expr)
                                   traceShowM "Starting return call"
                                   traceShowM res
                                   state <- get 
                                   let funcState = Map.lookup funcName state in 
                                    case funcState of
                                      Nothing -> err "Function does not exist"
                                      Just (p,a,l,strLst) -> let updateState = Map.insert funcName (p,a,(drop 1 l),strLst) state in 
                                                              do put updateState
                                                                 test <- get 
                                                                 traceShowM $ "returning " ++ (show res) 
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
                                                                traceShowM $ "Result of if block " ++ (show res)
                                                                state <- get
                                                                traceShowM state
                                                                return RetPass
                                                        else do res <- evalStmt (funcName, blockF)
                                                                traceShowM $ "Result of else block " ++ (show res)
                                                                state <- get
                                                                --traceShowM state
                                                                return RetPass
evalStmt (funcName, (Assign var val)) = do res <- evalExpr (funcName, val)
                                           traceShowM $ (show res) ++ " assigning function"
                                           state <- get 
                                           let funcState = Map.lookup funcName state in 
                                            do case funcState of
                                                Nothing -> err "Function does not exist"
                                                Just (p,a,(lc:rest),strLst) -> let newLc = Map.insert var res lc 
                                                                                   newState = Map.insert funcName (p, a, (newLc:rest), strLst) state
                                                                                in do put newState
                                                                                      state' <- get 
                                                                                      traceShowM state'
                                                                                      return RetPass
                                                Just (p,a,[],strLst) -> let newLc = Map.insert var res Map.empty
                                                                            newState = Map.insert funcName (p,a,[newLc],strLst) state
                                                                         in do put newState
                                                                               state' <- get 
                                                                               traceShowM state'
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

  
                                            

                                                
                                         

run' :: Program -> (Unsafe [String], State)
run' program = r (eval program) Map.empty


run'' :: (String, Stmt) -> (Unsafe StmtRet, State)
run'' (name, a) = r (evalStmt (name, a)) (createState test)


run :: (String, Expr) -> (Unsafe Integer, State)
run (str, a) = r (evalExpr (str, a)) (createState test2)

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
                                traceShowM $ "Looking up variable " ++ var
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
evalExpr (name, (Call func (x:xs))) = do cState <- get
                                         --traceShowM cState
                                         let getFunc = Map.lookup func cState in
                                           case getFunc of
                                            Nothing -> err $ "Function " ++ func ++ " does not exist"
                                            Just (p,a,l,strLst) -> do res <- evalExpr (name, x)
                                                                      --traceShowM res
                                                                      --traceShowM p
                                                                      let newLocal = createLocal (zip p [res]) 
                                                                          newState = Map.insert func (p,a,(newLocal:l),strLst) cState
                                                                       in do put newState
                                                                             traceShowM $ "Result of func call " ++ (show newLocal)
                                                                             res <- evalProgram (func, [a])
                                                                             case res of
                                                                              RetVal i -> do return i
                                                                              otherwise -> do traceShowM res
                                                                                              err "Nothing returned"
                                                                       

-- evalArgs :: (String, [Expr]) -> StatefulUnsafe State [Integer]
-- evalArgs (name, []) = return []
-- evalArgs (name, (x:xs)) = do res <- evalExpr (name, x)
--                              traceShowM res
--                              rest <- evalArgs (name, xs)
--                              return $ [res]++rest
                                        
-- setLocal :: (String, [Expr]) -> State -> [Integer]
-- setLocal (name, []) state = []
-- setLocal (name, (x:xs)) state = let res = evalExpr (name, x) in
--                                  case res of
--                                   res -> res

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


-- eval :: Program -> (Unsafe Integer, Unsafe [String])
-- eval (P program) = let global = createGlobal program
--                     in eval' global [Map.empty] []



--eval' program (createGlobal program) [Map.empty] []


-- eval' :: GlobalScope -> [LocalScope] -> [String] -> (Unsafe Integer, Unsafe [String])
-- eval' global local strLst = let main = Map.lookup "main" global
--                               in case main of 
--                                    Nothing -> (Error "no main function", Error "no main functions")
--                                    Just (_, Block rest) -> evalStmt rest global local strLst

ifTest = (Block [Assign "x" (Val 2), If (NEq (Val 2) (Val 2)) (Block [Ret (Val 4)]),Ret (Sub (Val 2) (Val 1))])
retTest = [Ret (Val 3)]

whileTest = Block [Assign "x" (Val 10), While (Gt (Var "x") (Val 1)) (Block [Assign "x" (Div (Var "x") (Val 2))]),Print (Var "x"), Assign "x" (Val (-12))]

ifelseTest = (Block [Assign "x" (Val 200), IfElse (Eq (Var "x") (Val 2)) (Block [Ret (Plus (Var "x") (Var "x"))]) (Block [Ret (Var "x")])])

printTest = [Block [Assign "x" (Val 3), Print (Plus (Var "x") (Val 2))]]

breakTest = [Block [Assign "x" (Val 10), While (Lt (Var "x") (Val 20)) (Block [Assign "x" (Plus (Var "x") (Val 1)), If (Eq (Var "x") (Val 15))(Block [Print (Val 111), Break]), Print (Var "x")]), Print (Var "x")]]



contTest = [Block [Assign "x" (Val 10), While (Lt (Var "x") (Val 20)) (Block [Assign "x" (Plus (Var "x") (Val 1)), If (Eq (Var "x") (Val 15))(Block [Print (Val 111), Continue]), Print (Var "x")]), Print (Var "x")]]

breakTest1 = [Block [Assign "x" (Val 10),If (Eq (Var "x") (Val 10)) (Block [Break,Print (Val 11111111111111)]), Print (Var "x")]]

testLocal = createLocal [("x", 0)]


funcTest = (P [Def "main" [] (Block [If (Eq (Val 2) (Val 2)) (Block [Ret (Val 4)]),Ret (Sub (Val 2) (Val 1))])])


-- x = 10
-- if(x==10){
--   break;
--   print x;
-- }
-- print x;




-- evalStmt :: [Stmt] -> GlobalScope -> [LocalScope] -> [String] -> (Unsafe Integer, [LocalScope], [String])
-- evalStmt [] _ local print = (Ok 0, local, print)
-- evalStmt ((Block code):rest) global local print = let block = evalStmt code global local print
--                                              in case block of
--                                                  (Error str, _, print') -> (Error str, [], print')
--                                                  (Ok i, [], print') -> (Ok i, [], print') 
--                                                  (Ok _, stack, print') -> evalStmt rest global stack print'
-- evalStmt ((While expr code):rest) global local print = let cond = evalExpr expr global local [] in
--                                                         case cond of
--                                                           Error str -> (Error ("Cond broken because " ++ str), [], print)
--                                                           Ok 0 -> evalStmt rest global local print
--                                                           Ok _ -> let evaledLoop = evalStmt [code] global local print in
--                                                                     case evaledLoop of
--                                                                       (Ok i, [], print') -> (Ok i, [], print')
--                                                                       (Ok _, (x:xs), print') -> evalStmt ((While expr code):rest) global (x:xs) print'
--                                                                       (Error str, local', print') -> if str == "Break"
--                                                                                                       then evalStmt rest global local' print'
--                                                                                                       else (Error str, [], print')


-- evalStmt ((Ret code):rest) global local print = let res = evalExpr code global local []
--                                                  in case res of 
--                                                      Error str -> (Error str, [], print)
--                                                      Ok i -> (Ok i, [], print)
-- evalStmt ((If expr code):rest) global local print = let cond = evalExpr expr global local [] in 
--                                                      case cond of 
--                                                          Error str -> (Error str, [], print)
--                                                          Ok 0 -> evalStmt rest global local print
--                                                          Ok _ -> let evaled = evalStmt [code] global local print in
--                                                                    case evaled of
--                                                                      (Ok i, [], newPrint) -> (Ok i, [], newPrint)
--                                                                      (Error str, local', newPrint) -> if str == "Break" 
--                                                                                                         then evalStmt rest global local' newPrint
--                                                                                                         else (Error ("if block failed "++str), [], newPrint)
--                                                                      (Ok _, stack, newPrint) -> evalStmt rest global local newPrint
-- evalStmt ((IfElse expr blockT blockF):rest) global local print = let cond = evalExpr expr global local [] in
--                                                                   case cond of
--                                                                       Error str -> (Error str, [], print)
--                                                                       Ok 0 -> evalStmt [blockF] global local print
--                                                                       Ok _ -> evalStmt [blockT] global local print

                                                             
-- evalStmt ((Assign name expr):rest) global (cLocal:rLocal) print = let exprEvaled = evalExpr expr global (cLocal:rLocal) [] in
--                                                                    case exprEvaled of
--                                                                     Error str -> (Error ("Assign expr failed, "++str), [], print)
--                                                                     Ok i -> let updateLocal = Map.insert name i cLocal in
--                                                                               (Ok 0, (updateLocal:rLocal), print)
-- evalStmt ((Line expr):rest) global local print = evalStmt rest global local print
-- evalStmt ((Print expr):rest) global local print = let evalingPrint = evalExpr expr global local [] 
--                                                    in case evalingPrint of
--                                                     Error str -> (Error str, [], print)
--                                                     Ok i -> let newString = show i in
--                                                              (Ok 0, local, newString:print)
-- evalStmt ((Break):rest) global local print = (Error "Break", local, print) 



                                                                
                                                                



-- evalStmt :: [Stmt] -> GlobalScope -> [LocalScope] -> [String] -> (Unsafe Integer, Unsafe [String])
-- evalStmt [] global local strLst = (Ok 0, Ok []) 
-- evalStmt ((Block code):rest) global local strLst = evalStmt code global local strLst 
-- -- evalStmt (Block rest) global local strLst = eval' rest global local strLst 
-- evalStmt (Ret expr) global local strLst = let res = evalExpr expr global local strLst
                                            --in (res, Ok strLst)
-- evalStmt (Assign var expr) global (local:rest) strLst = let x = evalExpr expr global (local:rest) strLst
--                                                             newScope = Map.insert var x local












-- evalExpr :: Expr -> GlobalScope -> [LocalScope] -> [String] -> Unsafe Integer
-- evalExpr _ _ [] _ = Error "No local scope"
-- evalExpr (Val i) _ _ _ = Ok i
-- evalExpr (Plus l r) global local strLst = let x = evalExpr l global local strLst
--                                               y = evalExpr r global local strLst
--                                            in case x of Error str -> Error str
--                                                         Ok x' -> case y of Error str -> Error str
--                                                                            Ok y' -> Ok (x' + y')
-- evalExpr (Sub l r) global local strLst = let x = evalExpr l global local strLst 
--                                              y = evalExpr r global local strLst
--                                           in case x of Error str -> Error str
--                                                        Ok x' -> case y of Error str -> Error str
--                                                                           Ok y' -> Ok (x' - y')
-- evalExpr (Mult l r) global local strLst = let x = evalExpr l global local strLst
--                                               y = evalExpr r global local strLst
--                                            in case x of Error str -> Error str
--                                                         Ok x' -> case y of Error str -> Error str
--                                                                            Ok y' -> Ok (x' * y')
-- evalExpr (Div l r) global local strLst = let x = evalExpr l global local strLst
--                                              y = evalExpr r global local strLst
--                                           in case x of Error str -> Error str
--                                                        Ok x' -> case y of Error str -> Error str
--                                                                           Ok y' -> if y' == 0
--                                                                                    then Error "Cannot divide by 0"
--                                                                                    else Ok (x' `div` y')
-- evalExpr (Mod l r) global local strLst = let x = evalExpr l global local strLst
--                                              y = evalExpr r global local strLst
--                                           in case x of Error str -> Error str
--                                                        Ok x' -> case y of Error str -> Error str
--                                                                           Ok y' -> if y' == 0
--                                                                                    then Error "Cannot mod by 0"
--                                                                                    else Ok (x' `mod` y')
-- evalExpr (Eq l r) global local strLst = let x = evalExpr l global local strLst
--                                             y = evalExpr r global local strLst
--                                          in case x of Error str -> Error str
--                                                       x' -> case y of Error str -> Error str
--                                                                       y' -> if x' == y'
--                                                                              then Ok 1
--                                                                              else Ok 0
-- evalExpr (NEq l r) global local strLst = let x = evalExpr l global local strLst
--                                              y = evalExpr r global local strLst
--                                           in case x of Error str -> Error str
--                                                        Ok x' -> case y of Error str -> Error str
--                                                                           Ok y' -> if x' == y'
--                                                                                     then Ok 0
--                                                                                     else Ok 1
-- evalExpr (Lt l r) global local strLst = let x = evalExpr l global local strLst
--                                             y = evalExpr r global local strLst
--                                          in case x of Error str -> Error str
--                                                       Ok x' -> case y of Error str -> Error str
--                                                                          Ok y' -> if x' < y'
--                                                                                   then Ok 1
--                                                                                   else Ok 0
-- evalExpr (LtEq l r) global local strLst = let x = evalExpr l global local strLst
--                                               y = evalExpr r global local strLst 
--                                            in case x of Error str -> Error str
--                                                         Ok x' -> case y of Error str -> Error str
--                                                                            Ok y' -> if x' <= y'
--                                                                                     then Ok 1
--                                                                                     else Ok 0                                                                    
-- evalExpr (Gt l r) global local strLst = let x = evalExpr l global local strLst
--                                             y = evalExpr r global local strLst
--                                          in case x of Error str -> Error str
--                                                       Ok x' -> case y of Error str -> Error str
--                                                                          Ok y' -> if x' > y'
--                                                                                    then Ok 1
--                                                                                    else Ok 0 
-- evalExpr (GtEq l r) global local strLst = let x = evalExpr l global local strLst
--                                               y = evalExpr r global local strLst
--                                            in case x of Error str -> Error str
--                                                         Ok x' -> case y of Error str -> Error str
--                                                                            Ok y' -> if x' > y'
--                                                                                      then Ok 1
--                                                                                      else Ok 0 

-- evalExpr (And l r) global local strLst = let x = evalExpr l global local strLst
--                                           in case x of Error str -> Error str
--                                                        Ok 0 -> Ok 0
--                                                        Ok _ -> let y = evalExpr r global local strLst
--                                                                 in case y of Error str -> Error str
--                                                                              Ok 0 -> Ok 0
--                                                                              Ok _ -> Ok 1
-- evalExpr (Or l r) global local strLst = let x = evalExpr l global local strLst
--                                          in case x of Error str -> Error str
--                                                       Ok 0 -> let y = evalExpr r global local strLst
--                                                               in case y of Error str -> Error str
--                                                                            Ok 0 -> Ok 0
--                                                                            Ok _ -> Ok 1
--                                                       Ok _ -> Ok 1
-- evalExpr (Not val) global local strLst = let x = evalExpr val global local strLst
--                                           in case x of Error str -> Error str
--                                                        Ok 0 -> Ok 1
--                                                        Ok _ -> Ok 0                                              

-- -- --evaluating for function and variable calls
-- evalExpr (Var str) global (lsope:rest) strLst = let x = Map.lookup str lsope 
--                                                  in case x of Nothing -> Error "Variable not found"
--                                                               Just x' -> Ok x'
-- evalExpr (Call str args) global local strLst = let newStack = Map.lookup str global 
--                                                 in case newStack of 
--                                                      Nothing -> Error "Function does not exist"
--                                                      Just (params, ast) -> if (length params) > (length args)
--                                                                           then Error "Not enough"
--                                                                           else 
--                                                                            if (length params) < (length args)
--                                                                                then Error "Too many args given"
--                                                                                else 
--                                                                                  let argsEvaled = Prelude.foldr(\expr rest -> (evalExpr expr global local strLst):rest) [] args 
--                                                                                      lst = filterList argsEvaled
--                                                                                   in if (length params) > (length lst)
--                                                                                       then Error "One of the args given failed"
--                                                                                       else let lstVarSet = zip params lst
--                                                                                                newScope = createLocal lstVarSet
--                                                                                                addedLocal = newScope:local
--                                                                                                val = evalStmt [ast] global addedLocal strLst

--                                                                                             in case val of 
--                                                                                                 (Error str, _) -> Error str
--                                                                                                 (Ok x, strLst') -> Ok x
   
   
   
                                    


