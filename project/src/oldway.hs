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
   
   
   
                                    


