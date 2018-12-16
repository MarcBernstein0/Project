module CCompiler where

import Ast
import ICInterpreter
import StatefulUnsafeMonad
import Data.Map as Map
import Debug.Trace


type Global = Map String (Int, [String])



-- compile :: Program -> IC_Program
-- compile (P (head:rest)) = [compileStmt head]++ compile (P rest)

-- compileStmt :: Stmt -> IC_Instruction
-- compileStmt (Def name (head:rest) body) = undefined
-- compileStmt While cond body = undefined
-- compileStmt Block body = undefined
-- compileStmt If cond body = undefined
-- compileStmt Assign name val = undefined
-- compileStmt Line body = undefined
-- compileStmt Ret body = undefined
-- compileStmt Print body = undefined
-- compileStmt Break = undefined
-- compileStmt Continue = undefined

testPlzWork = [Def "main" [] (Block [Assign "x" (Val 6),Print (Var "x"),Assign "y" (Val 8),Print(Var "y"),Assign "z" (Plus (Div (Mult (Var "x") (Var "y")) (Val 3)) (UnaryMinus (Plus (Var"x") (Mult (Val 2) (Var "y"))))),Print (Var "z"),Assign "w" (Sub (Var "z") (Mod (Var "x") (Sub (Var "x") (Val 2)))),Print (Var "w"),Ret (Val 0)])]

testWork = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 6),Assign' (Var' "x") (Var' "_t0"),Print' "x= " (Var' "x"),Assign' (Var' "_t1") (Val' 8),Assign' (Var' "y") (Var' "_t1"),Print' "y= " (Var' "y"),Times' (Var' "_t2") (Var' "x") (Var' "y"),Assign' (Var' "_t3") (Val' 3),Div' (Var' "_t4") (Var' "_t2") (Var' "_t3"),Assign' (Var' "_t5") (Val' 2),Times' (Var' "_t6") (Var' "_t5") (Var' "y"),Plus' (Var' "_t7") (Var' "x") (Var' "_t6"),Uminus' (Var' "_t8") (Var' "_t7"),Plus' (Var' "_t9") (Var' "_t4") (Var' "_t8"),Assign' (Var' "z") (Var' "_t9"),Print' "z= " (Var' "z"),Assign' (Var' "_t10") (Val' 2),Minus' (Var' "_t11") (Var' "x") (Var' "_t10"),Mod' (Var' "_t12") (Var' "x") (Var' "_t11"),Minus' (Var' "_t13") (Var' "z") (Var' "_t12"),Assign' (Var' "w") (Var' "_t13"),Print' "w= " (Var' "w"),Assign' (Var' "_t14") (Val' 0),Return' (Var' "_t14")]


-- backPatch       TL     FL     CL     BL
type BackPatch = ([Int], [Int], [Int], [Int])

emptyBP :: BackPatch
emptyBP = ([],[],[],[])


type Temp = [Op]

temps :: Temp
temps = [Var' ("_t" ++ (show n)) | n <- [0,1..]]


x = [Assign' (Var' "_t0") (Val' 2),Assign' (Var' "_t1") (Val' 2),Plus' (Var' "_t2") (Var' "_t0") (Var' "_t1"),Assign' (Var' "_t3") (Val' 3),Assign' (Var' "_t4") (Val' 1),Assign' (Var' "_t5") (Val' 1),Minus' (Var' "_t6")(Var' "_t4") (Var' "_t5"),Div' (Var' "_t7") (Var' "_t3") (Var' "_t6"),Lt' (Var' "_t8") (Var' "_t2") (Var' "_t7"),Bzero' (Var' "_t8") 0, Jump' 0]
lst = [10]
line = 12



runTrue a = r (compileProgram a [Push', Call' 0, Halt'] temps emptyBP) Map.empty


runCode a = r (compileProgram a [Push', Call' 3, Halt'] temps emptyBP) Map.empty

extract :: (Unsafe (IC_Program, BackPatch, Temp), Global) -> IC_Program
extract (Ok (x,_,_),_) = x
extract (Error str,_) = []
-- extract'' (x,_,_) = x


--testing test cases

test1 = [Def "main" [] (Block [Assign "x" (Val 6),Print (Var "x"),Assign "y" (Val 8),Print (Var "y"),Assign "z" (Plus (Plus (Div (Mult (Var "x") (Var "y")) (Val 3)) (UnaryMinus (Var "x"))) (Mult (Val 2) (Var "y"))),Print (Var "z"),Assign "w" (Sub (Var "z") (Mod (Var "x") (Sub (Var "x") (Val 2)))),Print (Var "w"),Ret (Val 0)])]
test2 = [Def "main" [] (Block [Assign "x" (Val 4),Assign "y" (Val 2),Assign "z" (Val (-1)),If (Gt (Var "x") (Val 2)) (Block [Print (Var "x")]),If (Lt (Var "y") (Val 2)) (Block [Print (Var "y")]),IfElse (NEq (Var "z") (Val 2)) (Block [Print (Var "z")]) (Block [Print (Var "y")]),Print (Var "z"),IfElse (LtEq (Var "z") (Var "y")) (Block [Print(Var "x"),IfElse (Gt (Plus (Var "x") (Var "y")) (Var "z")) (Block [Print (Var "y")]) (Block [Print (Var "z")])]) (Block [Print (Var "z")]),Ret (Val 0)])]
test3 = [Def "main" [] (Block [Assign "k" (Val 1),Assign "sum" (Val 0),While (LtEq (Var "k") (Val 10)) (Block [Assign "sum" (Plus (Var "sum") (Var "k")),Assign "k" (Plus (Var "k") (Val 1))]),Print (Var "sum"),Ret (Val 0)])]
test4 = [Def "main" [] (Block [Assign "n" (Val 1),Assign "count" (Val 0),While (LtEq (Var "n") (Val 3)) (Block [Assign "m" (Val 1),While (LtEq (Var "m") (Val 4)) (Block [If (Eq (Mod (Var "m") (Var "n")) (Val 0)) (Block [Assign "count" (Plus (Var "count") (Val 1))]),Assign "m" (Plus (Var "m") (Val 1))]),Assign "n" (Plus (Var "n") (Val 1))]),Print (Var "count"),Ret (Val 0)])]
test5 = [Def "main" [] (Block [Assign "count" (Val 0),Assign "limit" (Val 10),Assign "n" (Val 2),While (LtEq (Var"count") (Var "limit")) (Block [Assign "isPrime" (Val 1),Assign "k" (Val 2),While (Lt (Var "k") (Var "n")) (Block [If (Eq (Mod (Var "n") (Var "k")) (Val 0)) (Block [Assign "isPrime" (Val 0)]),Assign "k" (Plus (Var "k") (Val 1))]),If (Eq (Var "isPrime") (Val 1)) (Block [Print (Var "n"),Assign "count" (Plus (Var "count") (Val 1))]),Assign "n" (Plus (Var "n") (Val 1))]),Ret (Val 0)])]
test6 = [Def "main" [] (Block [Assign "x" (Val 3),Assign "y" (Val 5),Assign "z" (Val 1),If (And (Gt (Var "x") (Val 2)) (Lt (Var "y") (Val 5))) (Block [Print (Var "x")]),If (Or (Gt (Var "x") (Val 2)) (Lt (Var "y") (Val 5))) (Block [Print (Var "x")]),If (Or (And (Gt (Var "x") (Val 2)) (Lt (Var "y") (Val 5))) (Not (Eq (Var "z") (Val 2)))) (Block [Print (Var "x")]),If (Or (NEq (Var "z") (Val 2)) (And (Gt (Var "x") (Val 2)) (Lt (Var "y") (Val 5)))) (Block [Print (Var "x")]),Ret (Val 0)])]
test7 = [Def "f" ["x"] (Block [Assign "n" (Plus (Var "x") (Val 1)),Assign "n" (Mult (Var "n") (Val 2)),Ret (Var "n")]),Def "main" [] (Block [Assign "y" (Val 2),Assign "n" (Val 3),Assign "z" (Call "f" [Plus (Var "y") (Var "n")]),Print (Var "z"),Ret (Val 0)])]
test8 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "a" (Val 1),Assign "z" (Plus (Call "succ" [Var "a"]) (Mult (Call "succ" [Plus (Var "a") (Val 1)]) (Call "succ" [Mult (Var "a") (Val 2)]))),Print (Var "z"),Ret (Val 0)])]
test8' = [Def "succ" ["x"] (Block [Print (Var "x"),Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "a" (Val 1),Assign "z" (Plus (Call "succ" [Var "a"]) (Mult (Call "succ" [Plus (Var "a") (Val 1)]) (Call "succ" [Mult (Var "a") (Val 2)]))),Print (Var "z"),Ret (Val 0)])]
test9 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "a" (Val 5),Assign "z" (Call "succ" [Call "succ" [Call "succ" [Var "a"]]]),Print (Var "z"),Ret (Val 0)])]
test10 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "times2" ["x"] (Block [Ret (Mult (Var "x") (Val 2))]),Def "f" ["y"] (Block [Assign "z" (Call "succ" [Var "y"]),Assign "y" (Call "times2" [Var "z"]),Ret (Var "y")]),Def "main" [] (Block [Assign "z" (Call "f" [Val 10]),Print (Var "z"),Ret (Val 0)])]
test11 = [Def "gcd" ["b"] (Block [Assign "a" (Val 2854),While (NEq (Var "b") (Val 0)) (Block [Assign "t" (Var "b"),Assign "b" (Mod (Var "a") (Var "b")),Assign "a" (Var "t")]),Ret (Var "a")]),Def "main" [] (Block [Assign "m" (Val 264),Assign "res" (Call "gcd" [Var "m"]),Print (Var "res"),Ret (Val 0)])]
test12 = [Def "Q" ["n"] (Block [IfElse (LtEq (Var "n") (Val 2)) (Block [Ret (Val 1)]) (Block [Ret (Plus (Call "Q" [Sub (Var "n") (Call "Q" [Sub (Var "n") (Val 1)])]) (Call "Q" [Sub (Var "n") (Call "Q" [Sub (Var "n") (Val 2)])]))])]),Def "main" [] (Block [Assign "k" (Val 1),While (Lt (Var "k") (Val 20)) (Block [Assign "q" (Call "Q" [Var "k"]),Print (Var "q"),Assign "k" (Plus (Var "k") (Val 1))]),Ret (Val 0)])]
testFib = [Def "fib" ["x"] (Block [If (Eq (Var "x") (Val 0)) (Block [Ret (Val 0)]),IfElse (Eq (Var "x") (Val 1)) (Block [Ret (Val 1)]) (Block [Ret (Plus (Call "fib" [Sub (Var "x") (Val 1)]) (Call "fib" [Sub (Var "x") (Val 2)]))])]),Def "main" [] (Block [Assign "x" (Call "fib" [Val 4]),Print (Val 4),Ret (Val 0)])]

backPatch :: [Int] -> Int -> IC_Program -> IC_Program
backPatch [] set ic = ic
backPatch (x:xs) set ic = 
    let step = ic !! (x-1) in 
        case step of
            (Jump' _) -> let 
                            newIc = (take (x-1) ic) ++ [Jump' (set)] ++ (drop (x) ic)
                         in backPatch xs set newIc
            (Bzero' t _) -> let
                                newIc = (take (x-1) ic) ++ [Bzero' t (set)] ++ (drop (x) ic) 
                            in backPatch xs set newIc
            (Call' _) -> let
                                newIc = (take (x-1) ic) ++ [Call' (set)] ++ (drop (x) ic) 
                            in backPatch xs set newIc
            (otherwise) -> backPatch xs set ic
   
-- run a = r (compileExpr a [Push', Call' 3, Halt'] emptyBP temps) Map.empty
-- test = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 1),Assign' (Var' "_t1") (Val' 2),Times' (Var' "_t2") (Var' "_t0") (Var' "_t1")]
-- runStmt a = r (compileStmt a [] emptyBP temps) Map.empty
-- plzWork2 = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' (-1111)),Print' "print expr = " (Var' "_t0")]

compileProgram :: [Stmt] -> IC_Program -> Temp -> BackPatch -> StatefulUnsafe Global (IC_Program, BackPatch, Temp)
compileProgram [] ic temp bp =
    do --traceShowM "Gotten to end of program"
       --traceShowM ic
       return (ic, bp, temp)
compileProgram (x:xs) ic temp bp  =
    do --traceShowM "Still running"
       (ic2, bp2, temp2) <- compileStmt x ic bp temp
       --traceShowM $ "line of code " ++ (show ic2)
       (ic3, bp3, temp3) <- compileProgram xs ic2 temp2 bp2
       return (ic3, bp3, temp3)



compileStmt :: Stmt ->  IC_Program -> BackPatch -> Temp -> StatefulUnsafe Global (IC_Program, BackPatch, Temp)
compileStmt (Def name params body) ic bp temp = 
    do g <- get
       let  lineFunc = length ic
            newG = Map.insert name (lineFunc, params) g
        in do --traceShowM newG
              if name == "main"
                then let ic2 = backPatch [2] lineFunc ic
                      in do put newG
                            -- traceShowM ic2
                            -- traceShowM (ic2 !! 1)
                            compileStmt body ic2 bp temp
                else do put newG
                        traceShowM "This is being hit for some resone"
                        compileStmt body ic bp temp
compileStmt (Ret expr) ic bp temp =
    do (locRet,ic2,bp2,temp2) <- compileExpr expr ic bp temp
       -- traceShowM locRet
       -- traceShowM ic2
       let ic3 = ic2 ++ [(Return' locRet)]
        in do --traceShowM ic3
              return (ic3, bp, temp2)
compileStmt (Assign var expr) ic bp temp =
    do (locVar, ic2, bp2, temp2) <- compileExpr expr ic bp temp
       let ic3 = ic2 ++[(Assign' (Var' var) locVar)]
        in do --traceShowM ic3
              return (ic3, bp2, temp2)
compileStmt (Print expr) ic bp temp =
    do (locPrint, ic2, bp2, temp2) <- compileExpr expr ic bp temp
       case expr of
        Var var -> let ic3 = ic2 ++ [(Print' (var ++ "= ") locPrint)]
                    in do --traceShowM ic3
                          return (ic3, bp2, temp2)
        otherwise -> let ic3 = ic2 ++ [(Print' "print expr = " locPrint)]
                    in do --traceShowM ic3
                          return (ic3, bp2, temp2)
compileStmt (Break) ic (tL,fL,cL,bL) temp = 
    let ic2 = ic ++ [Jump' 0]
        b = [length ic2]
    in return (ic2, (tL,fL,cL,(bL++b)), temp)
compileStmt (Continue) ic (tL,fL,cL,bL) temp = 
    let ic2 = ic ++ [Jump' 0]
        c = [length ic2]
    in return (ic2, (tL,fL,(cL++c),bL),temp)
compileStmt (Block body) ic bp temp =
    do --traceShowM body
       (ic2, bp2, temp2) <- compileProgram body ic temp bp
       --traceShowM $ "Body compiled " ++ (show ic2)
       return (ic2, bp2, temp2)
compileStmt (While cond body) ic bp temp =
    let start = length ic
     in do (_,ic2,(tl,fl,cl,bl),temp2) <- compileExpr cond ic bp temp
           -- traceShowM "while started"
           let loop = length ic2
            in do (ic3, (_,_,cl2,bl2), temp3) <- compileStmt body ic2 (tl,fl,cl,bl) temp2
                  -- traceShowM $ "true list " ++ (show tl)
                  -- traceShowM $ "false list " ++ (show fl)
                  -- traceShowM $ "breakList " ++ (show bl)
                  let ic4 = backPatch tl loop ic3
                      ic5 = ic4 ++ [Jump' (start)]
                      ic6 = backPatch (bl2++fl) (length ic5) ic5
                      ic7 = backPatch cl2 start ic6
                    in do --traceShowM ic7
                          return (ic7, emptyBP, temp3)
compileStmt (If cond body) ic bp temp = 
    do (_,ic2,(tl,fl,cl,bl),temp2) <- compileExpr cond ic bp temp
       let code = length ic2
        in do (ic3,bp2,temp3) <- compileStmt body ic2 (tl,fl,cl,bl) temp2
              -- traceShowM "started if"
              -- traceShowM ic3
              -- traceShowM (length ic3)
              -- traceShowM $ "true list inside if " ++ (show tl)
              let ic4 = backPatch tl code ic3
                in do --traceShowM ic4
                      let ic5 = backPatch fl (length ic4) ic4
                        in do return (ic5, ([],[],cl,bl),temp3)
compileStmt (IfElse cond body body2) ic bp temp =
    do (_,ic2,(tl,fl,cl,bl),temp2) <- compileExpr cond ic bp temp
       let code = length ic2
        in do (ic3, bp2,temp3) <- compileStmt body ic2 (tl,fl,cl,bl) temp2
              -- traceShowM "If block "
              -- traceShowM cond
              -- traceShowM body
              -- traceShowM (length ic3)
              -- traceShowM $ "tl: " ++ (show tl)
              -- traceShowM $ "fl: " ++ (show fl)
              let ic4 = backPatch tl code ic3
                  ic5 = backPatch fl ((length ic4) + 1) ic4
                  ic6 = ic5 ++ [Jump' 0] --jump to end of else
                  elseSkip = length ic6
                in do (ic7, (_,_,cl2,bl2),temp4) <- compileStmt body2 ic6 bp2 temp3
                      let ic8 = backPatch [elseSkip] (length ic7) ic7
                       in do --traceShowM $ "else block "
                             -- traceShowM $ "tl2: " ++ (show tl2)
                             -- traceShowM $ "fl2: " ++ (show fl2)
                             return (ic8,([],[],cl2,bl2), temp4)



t = [Def "main" [] (Block [Assign "x" (Val 4),Assign "y" (Val 2),Assign "z" (Val (-1)),IfElse (LtEq (Var "z") (Var "y")) (Block [Print (Var "x"),IfElse (Gt (Var "x") (Var "z")) (Block [Print (Var "y")]) (Block [Print (Var "z")])]) (Block [Print (Var "z")]),Ret (Val 0)])]
ifInside = [Def "main" [] (Block [Assign "x" (Val 2),IfElse (Eq (Var "x") (Val 2)) (Block [If (Eq (Var "x") (Val 3)) (Block [Print (Val 3)]),Print (Val 4)]) (Block [Print (Var "x")])])]
funcCalling = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Call "succ" [Val 1]),Print (Var "y"),Ret (Val 0)])]
funcCalling2 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Plus (Call "succ" [Var "y"]) (Call "succ" [Plus (Var "y") (Val 2)])),Print (Var "z"),Ret (Val 0)])]
funcCalling3 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Plus (Call "succ" [Var "y"]) (Mult (Call "succ" [Plus (Var "y") (Val 2)]) (Call "succ" [Mult (Var "y") (Val 2)]))),Print (Var "z"),Ret (Val 0)])]
funcCalling4 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Call "succ" [Mult (Var "y") (Val 2)]),Print (Var "z"),Ret (Val 0)])]
funcCalling5 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Mult (Call "succ" [Plus (Var "y") (Val 1)]) (Call "succ" [Mult (Var "y") (Val 2)])),Print (Var "z"),Ret (Val 0)])]
funcCalling6 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Plus (Call "succ" [Var "y"]) (Mult (Call "succ" [Plus (Var "y") (Val 1)]) (Call "succ" [Mult (Var "y") (Val 2)]))),Print (Var "z"),Ret (Val 0)])]
funcCalling7 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Plus (Mult (Call "succ" [Plus (Var "y") (Val 1)]) (Call "succ" [Mult (Var "y") (Val 2)])) (Call "succ" [Var "y"])),Print (Var "z"),Ret (Val 0)])]
funcCalling8 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Plus (Call "succ" [Var "y"]) (Call "succ" [Plus (Var "y") (Val 1)])),Print (Var "z"),Ret (Val 0)])]
funcCalling9 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Plus (Call "succ" [Var "y"]) (Mult (Call "succ" [Plus (Var "y") (Val 1)]) (Call "succ" [Val 2]))),Print (Var "y"), Print (Var "z"),Ret (Val 0)])]
funcCalling10 = [Def "succ" ["x"] (Block [Ret (Plus (Var "x") (Val 1))]),Def "main" [] (Block [Assign "y" (Val 1),Assign "z" (Call "succ" [Val 2]),Print (Var "z"),Ret (Val 0)])]

compileExpr :: Expr -> IC_Program -> BackPatch -> Temp -> StatefulUnsafe Global (Op, IC_Program, BackPatch, Temp)
compileExpr (Val i) ic bp (t:rest) = 
    let newAddition = [(Assign' (t) (Val' (fromIntegral i)))]
        ic2 = ic ++ newAddition
    in do traceShowM i
          return (t, ic2, bp, rest)
compileExpr (Plus l r) ic bp temp = 
    do (locX, ic2, bp2, temp2) <- compileExpr l ic bp temp
       (locY, ic3, bp3, (t:temp3)) <- compileExpr r ic2 bp2 temp2
       return (t, (ic3++[(Plus' (t) (locX) (locY))]), bp3, temp3)
compileExpr (Sub l r) ic bp temp = 
    do (locX, ic2, bp2, temp2) <- compileExpr l ic bp temp
       (locY, ic3, bp3, (t:temp3)) <- compileExpr r ic2 bp2 temp2
       return (t, (ic3++[(Minus' (t) (locX) (locY))]), bp3, temp3)
compileExpr (Mult l r) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr l ic bp temp
       traceShowM $"Mult left " ++ (show l)
       traceShowM $ "Mult right " ++ (show r)
       (locY, ic3, bp3, (t:temp3)) <- compileExpr r ic2 bp2 temp2
       return (t, (ic3++[(Times' (t) (locX) (locY))]), bp3, temp3)
compileExpr (Div l r) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr l ic bp temp
       (locY, ic3, bp3, (t:temp3)) <- compileExpr r ic2 bp2 temp2
       return (t, (ic3++[(Div' (t) (locX) (locY))]), bp3, temp3)
compileExpr (Mod l r) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr l ic bp temp
       (locY, ic3, bp3, (t:temp3)) <- compileExpr r ic2 bp2 temp2
       --traceShowM ic3
       return (t, (ic3++[(Mod' (t) (locX) (locY))]), bp3, temp3)
compileExpr (Lt x y) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr x ic bp temp 
       (locY, ic3, bp3, (t:temp3)) <- compileExpr y ic2 bp2 temp2
       let ic4 = ic3 ++ [(Lt' t locX locY)] ++ [(Bzero' t 0)]
           floc = length ic4
           ic5 = ic4 ++ [(Jump' 0)]
           tloc = length ic5
        in do return (t, ic5, ([tloc], [floc],[],[]), temp3)
compileExpr (LtEq x y) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr x ic bp temp 
       (locY, ic3, bp3, (t:temp3)) <- compileExpr y ic2 bp2 temp2
       let ic4 = ic3 ++ [(Le' t locX locY)] ++ [(Bzero' t 0)]
           floc = length ic4
           ic5 = ic4 ++ [(Jump' 0)]
           tloc = length ic5
        in do return (t, ic5, ([tloc], [floc],[],[]), temp3)
compileExpr (Eq x y) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr x ic bp temp 
       (locY, ic3, bp3, (t:temp3)) <- compileExpr y ic2 bp2 temp2
       let ic4 = ic3 ++ [(Equal' t locX locY)] ++ [(Bzero' t 0)]
           floc = length ic4
           ic5 = ic4 ++ [(Jump' 0)]
           tloc = length ic5
        in do return (t, ic5, ([tloc], [floc],[],[]), temp3)
compileExpr (NEq x y) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr x ic bp temp 
       (locY, ic3, bp3, (t:temp3)) <- compileExpr y ic2 bp2 temp2
       let ic4 = ic3 ++ [(NotEq' t locX locY)] ++ [(Bzero' t 0)]
           floc = length ic4
           ic5 = ic4 ++ [(Jump' 0)]
           tloc = length ic5
        in do return (t, ic5, ([tloc], [floc],[],[]), temp3)
compileExpr (Gt x y) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr x ic bp temp 
       (locY, ic3, bp3, (t:temp3)) <- compileExpr y ic2 bp2 temp2
       let ic4 = ic3 ++ [(Gt' t locX locY)] ++ [(Bzero' t 0)]
           floc = length ic4
           ic5 = ic4 ++ [(Jump' 0)]
           tloc = length ic5
        in do return (t, ic5, ([tloc], [floc],[],[]), temp3)
compileExpr (GtEq x y) ic bp temp =
    do (locX, ic2, bp2, temp2) <- compileExpr x ic bp temp 
       (locY, ic3, bp3, (t:temp3)) <- compileExpr y ic2 bp2 temp2
       let ic4 = ic3 ++ [(Ge' t locX locY)] ++ [(Bzero' t 0)]
           floc = length ic4
           ic5 = ic4 ++ [(Jump' 0)]
           tloc = length ic5
        in do return (t, ic5, ([tloc], [floc],[],[]), temp3)
compileExpr (And x y) ic bp temp =
    do (_,ic2,(tl,fl,cl,bl),temp2) <- compileExpr x ic bp temp
       -- traceShowM ic2
       -- traceShowM (length ic2)
       let ic3 = backPatch tl (length ic2) ic2
        in do --traceShowM $"ic3: " ++ (show ic3)
              (_,ic4,(tl2,fl2,cl2,bl2),temp3) <- compileExpr y ic3 bp temp2
              return (Var' "",ic4,(tl2,fl++fl2,cl2,bl2), temp3 )
compileExpr (Or x y) ic bp temp =
    do (_, ic2,(tl,fl,cl,bl),temp2) <- compileExpr x ic bp temp
       let ic3 = backPatch fl (length ic2) ic2
        in do (_,ic4,(tl2,fl2,cl2,bl2),temp3) <- compileExpr y ic3 bp temp2
              return (Var' "",ic4,(tl++tl2,fl2,cl2,bl2), temp3)
compileExpr (Not x) ic bp temp =
    do (_, ic2, (tl,fl,cl,bl), temp2) <- compileExpr x ic bp temp
       return (Var' "", ic2, (fl,tl,cl,bl), temp2)
compileExpr (Var var) ic bp temp =
    do --traceShowM var
       return ((Var' var), ic, bp, temp)
compileExpr (UnaryMinus x) ic bp temp =
    do (locX, ic2, bp2, (t:temp2)) <- compileExpr x ic bp temp
       -- traceShowM $ "Uminus "++ (show x)
       -- traceShowM locX
       let ic3 = ic2 ++ [(Uminus' t locX)]
        in do traceShowM ic3
              return (t, ic3, bp2, temp2)
compileExpr (Call func []) ic bp (t:temp) = 
    do g <- get 
       --traceShowM g 
       case Map.lookup func g of
        Nothing -> err "Function does not exist"
        Just (line, params) -> if (length params) == 0
                                then let ic2 = ic ++ [Push', Call' line, Assign' t (Var' "_ret_val")]
                                      in do return (t, ic2, bp, temp) 
                                else err "Pramas do not match up"


compileExpr (Call func (x:xs)) ic bp temp =
    do g <- get 
       --traceShowM g 
       case Map.lookup func g of
        Nothing -> err "Function does not exist"
        Just (line, (p:ps)) -> if (length (p:ps)) == (length (x:xs))
                                then let ic2 = ic ++ [Push']
                                      in do traceShowM x
                                            (locX, ic3, bp2, (t:temp2)) <- compileExpr x ic2 bp temp
                                            -- traceShowM ic3
                                            -- traceShowM p
                                            let ic4 = ic3 ++ [Assign' (Var' p) locX, Call' line, Assign' t (Var' "_ret_val")]
                                             in do return (t,ic4,bp,temp) 
                                else err "Pramas do not match up"




-- Tests --

-- test2True = [Def "main" [] (Block [Assign "x" (Val 4),Assign "y" (Val 2),Assign "z" (Val (-1)),If (Gt (Var "x") (Val 2)) (Block [Print (Var "x")]),If (Lt (Var "y") (Val 2)) (Block [Print (Var "y")]),IfElse (NEq (Var "z") (Val 2)) (Block [Print (Var "z")]) (Block [Print (Var "y")]),Print (Var "z"),IfElse (LtEq (Var "z") (Var "y")) (Block [Print (Var "x"),IfElse (Gt (Plus (Var "x") (Var "y")) (Var "z")) (Block [Print (Var "y")]) (Block [Print (Var "z")])]) (Block [Print (Var "z")]),Ret (Val 0)])]

-- testIf = [Def "main" [] (Block [Assign "x" (Val 2),If (Eq (Var "x") (Val 3)) (Block [Print (Val 3)]),Print (Var "x"),Ret (Val 0)])]

-- testFuncCall = [Def "foo" [] (Block [Ret (Val 4)]),Def "main" [] (Block [Assign "x" (Call "foo" []),Print (Var "x"),Ret (Val 0)])]

-- test2FromParser = [Def "main" [] (Block [Assign "x" (Val 2),IfElse (Eq (Var "x") (Val 3)) (Block [Print (Val 3)]) (Block [IfElse (Eq (Var "x") (Val 2)) (Block [Print (Val 111)]) (Block [Print (Var "x")]),Print (Var "x")]),Ret (Val 0)])]

-- test2 = [Def "main" [] (Block [Assign "x" (Val 4),Assign "y" (Val 2),Assign "z" (Val (-1)),If (Gt (Var "x") (Val 2)) (Block [Print (Var "x")]),If (Lt (Var "y") (Val 2)) (Block [Print (Var "y")]),IfElse (NEq (Var "z") (Val 2)) (Block [Print (Var "z")]) (Block [Print (Var "y")]),Print (Var "z"),IfElse (LtEq (Var "z") (Var "y")) (Block [Print (Var "x"),IfElse (Gt (Plus (Var "x") (Var "y")) (Var "z")) (Block [Print (Var "y")]) (Block [Print (Var "z")])]) (Block [Print (Var "z")]),Ret (Val 0)])]


-- testNestedLoop = [Def "main" [] (Block [Assign "n" (Val 1),Assign "count" (Val 0),While (LtEq (Var "n") (Val 3)) (Block [Assign "m" (Val 1),While (LtEq (Var "m") (Val 4)) (Block [If (Eq (Mod (Var "m") (Var "n")) (Val 0)) (Block [Assign "count"(Plus (Var "count") (Val 1))]),Assign "m" (Plus (Var "m") (Val 1))]),Assign "n" (Plus (Var "n") (Val 1))]),Print (Var "count"),Ret (Val 0)])]

-- execNest = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 1),Assign' (Var' "n") (Var' "_t0"),Assign' (Var' "_t1") (Val' 0),Assign' (Var' "count") (Var' "_t1"),Assign' (Var' "_t2") (Val' 3),Le' (Var' "_t3") (Var' "n") (Var' "_t2"),Bzero' (Var' "_t3") 33,Jump' 11,Assign' (Var' "_t4") (Val' 1),Assign' (Var' "m") (Var' "_t4"),Assign' (Var' "_t5") (Val' 4),Le' (Var' "_t6") (Var' "m") (Var' "_t5"),Bzero' (Var' "_t6") 29,Jump' 17,Mod' (Var' "_t7") (Var' "m") (Var' "n"),Assign'(Var' "_t8") (Val' 0),Equal' (Var' "_t9") (Var' "_t7") (Var' "_t8"),Bzero' (Var' "_t9") 25,Jump' 22,Assign' (Var' "_t10") (Val' 1),Plus' (Var' "_t11") (Var' "count") (Var' "_t10"),Assign' (Var' "count") (Var' "_t11"),Assign' (Var' "_t12") (Val' 1),Plus' (Var' "_t13") (Var' "m") (Var' "_t12"),Assign' (Var' "m") (Var' "_t13"),Jump' 13,Assign' (Var' "_t14") (Val' 1),Plus' (Var' "_t15") (Var' "n") (Var' "_t14"),Assign' (Var' "n") (Var' "_t15"),Jump' 7,Print' "count= " (Var' "count"),Assign' (Var' "_t16") (Val' 0),Return' (Var' "_t16")]


-- testWhile = (While ((Eq (Var "x") (Val 2))) (Assign "x" (Plus (Var "x") (Val 1))))

-- testFuncWithBreak = [Def "main" [] (Block [While (Lt (Var "x") (Val 1)) (Block [If (Eq (Var "x") (Val 0)) (Block [Continue]),Assign "x" (Plus (Var "x") (Val 1))])])]
-- testIfElse = [Def "main" [] (Block [Assign "x" (Val 2),IfElse (Eq (Var "x") (Val 3)) (Block [Print (Var "x")]) (Block [Print (Val 111),Ret (Val 0)])])]

-- testWhile2 = [Def "main" [] (Block [Assign "k" (Val 1),Assign "sum" (Val 0),While (LtEq (Var "k") (Val 10)) (Block [Assign "sum" (Plus (Var "sum") (Var "k")),Assign "k" (Plus (Var "k") (Val 1))]),Print (Var "sum"),Ret (Val 0)])]

-- execFuncCall = [Push',Call' 4,Halt',Return' (Var' "x"),Push',Assign' (Var' "_t0") (Val' 4),Assign' (Var' "x") (Var' "_t0"),Call' 3,Assign' (Var' "_t1") (Var' "_ret_val"),Assign' (Var' "x") (Var' "_t1"),Print' "x= " (Var' "x"),Assign' (Var' "_t0") (Val' 0),Return' (Var' "_t0")]

-- execFuncCall2 = [Push',Call' 5,Halt',Assign' (Var' "_t0") (Val' 4),Return' (Var' "_t0"),Push',Call' 3,Assign' (Var' "_t1") (Var' "_ret_val"),Assign' (Var' "x") (Var' "_t1"),Print' "x= " (Var' "x"),Assign' (Var' "_t2") (Val' 0),Return' (Var' "_t2")]


-- plzGodWork = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 2),Assign' (Var' "x") (Var' "_t0"),Assign' (Var' "_t1") (Val' 3),Equal' (Var' "_t2") (Var' "x") (Var' "_t1"),Bzero' (Var' "_t2") 11,Jump' 9,Print' "x= " (Var' "x"),Jump' 15,Assign' (Var' "_t3") (Val' 111),Print' "print expr = " (Var' "_t3"),Assign' (Var' "_t4") (Val' 0),Return' (Var' "_t4")]


-- testIfElseInside = [Def "main" [] (Block [Assign "x" (Val 2),Assign "y" (Val 3),Assign "z" (Val 4),If (Eq (Var "x") (Val 2)) (Block [IfElse (NEq (Var "y") (Val 3)) (Block [Print (Var "y")]) (Block [If (Or (Eq (Var "z") (Val 4)) (Eq (Var "x") (Val 3))) (Block [Print (Var "z")]),Print (Val 444)])]), Ret (Val 0)])]


-- excIfElseIn = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 2),Assign' (Var' "x") (Var' "_t0"),Assign' (Var' "_t1") (Val' 3),Assign' (Var' "y") (Var' "_t1"),Assign' (Var' "_t2") (Val' 4),Assign' (Var' "z") (Var' "_t2"),Assign' (Var' "_t3") (Val' 2),Equal' (Var' "_t4") (Var' "x") (Var' "_t3"),Bzero' (Var' "_t4") 0,Jump' 0,Assign' (Var' "_t5") (Val' 3),NotEq' (Var' "_t6") (Var' "y") (Var' "_t5"),Bzero' (Var' "_t6") 19,Jump' 17,Print' "y= " (Var' "y"),Jump' 30,Assign' (Var' "_t7") (Val' 4),Equal' (Var' "_t8") (Var' "z") (Var' "_t7"),Bzero' (Var' "_t8") 23,Jump' 27,Assign' (Var' "_t9") (Val' 3),Equal' (Var' "_t10") (Var' "x") (Var' "_t9"),Bzero' (Var' "_t10") 28,Jump' 27,Print' "z= " (Var' "z"),Assign' (Var' "_t11") (Val' 444),Print' "print expr = " (Var' "_t11"),Assign' (Var' "_t12") (Val' 0),Return' (Var' "_t12")]


-- letSeeIfWorks = [Def "main" [] (Block [Assign "x" (Val 3),IfElse (Eq (Var "x") (Val 2)) (Block [Print (Val 42)]) (Block [IfElse (Lt (Var "x") (Val 1)) (Block [Print (Var "x")]) (Block [IfElse (Eq (Var "x") (Val 3)) (Block [Print (Val 47)]) (Block [Print (Val 111)])])]),Print (Var "x")])]

-- lll=  [Push',Call' 3,Halt',
--       Assign' (Var' "_t0") (Val' 3),
--       Assign' (Var' "x") (Var' "_t0"),
--       Assign' (Var' "_t1") (Val' 2),
--       Equal' (Var' "_t2") (Var' "x") (Var' "_t1"),
--       Bzero' (Var' "_t2") 12,
--       Jump' 9,
--       Assign' (Var' "_t3") (Val' 42),
--       Print' "print expr = " (Var' "_t3"),
--       Jump' 27,
--       Assign' (Var' "_t4") (Val' 1),
--       Lt' (Var' "_t5") (Var' "x") (Var' "_t4"),
--       Bzero' (Var' "_t5") 18,
--       Jump' 16,
--       Print' "x= " (Var' "x"),
--       Jump' 27,
--       Assign' (Var' "_t6") (Val' 3),
--       Equal' (Var' "_t7") (Var' "x") (Var' "_t6"),
--       Bzero' (Var' "_t7") 25,
--       Jump' 22,Assign' (Var' "_t8") (Val' 47),
--       Print' "print expr = " (Var' "_t8"),
--       Jump' 27,Assign' (Var' "_t9") (Val' 111),
--       Print' "print expr = " (Var' "_t9"),
--       Print' "x= " (Var' "x"),
--       Return' (Val' 0)]

-- l' =  [Def "main" [] (Block [Assign "x" (Val 4),Assign "y" (Val 2),Assign "z" (Val (-1)),If (Gt (Var "x") (Val 2)) (Block [Print (Var "x")]),If (Lt (Var "y") (Val 2)) (Block [Print (Var "y")]),IfElse (NEq (Var "z") (Val 2)) (Block [Print (Var "z")]) (Block [Print (Var "y")]),Print (Var "z"),IfElse (LtEq (Var "z") (Var "y")) (Block [Print (Var "x"),IfElse (Gt (Plus (Var "x") (Var "y")) (Var "z")) (Block [Print (Var "y")]) (Block [Print (Var "z")])]) (Block [Print (Var "z")]),Ret (Val 0)])]

-- exl' = undefined


-- testMultiIfs = [Def "main" [] (Block [Assign "x" (Val 4),Assign "y" (Val 2),Assign "z" (Val (-1)),If (Gt (Var "x") (Val 2)) (Block [Print (Var "x")]),If (Lt (Var "y") (Val 2)) (Block [Print (Var "y")]),Print (Val 111),Ret (Val 0)])]

-- excMultIfs = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 4),Assign' (Var' "x") (Var' "_t0"),Assign' (Var' "_t1") (Val' 2),Assign' (Var' "y") (Var' "_t1"),Assign' (Var' "_t2") (Val' (-1)),Assign' (Var' "z") (Var' "_t2"),Assign' (Var' "_t3") (Val' 2),Gt' (Var' "_t4") (Var' "x") (Var' "_t3"),Bzero' (Var' "_t4") 14,Jump' 13,Print' "x= " (Var' "x"),Assign' (Var' "_t5") (Val' 2),Lt' (Var' "_t6") (Var' "y") (Var' "_t5"),Bzero' (Var' "_t6") 19,Jump' 18,Print' "y= " (Var' "y"),Assign' (Var' "_t7") (Val' 111),Print' "print expr = " (Var' "_t7"),Assign' (Var' "_t8") (Val' 0),Return' (Var' "_t8")]

-- testNot = [Def "main" [] (Block [Assign "x" (Val 3),If (NEq (Var "x") (Val 2)) (Block [Print (Val 1111)]),Ret (Val 0)])]


-- excNot = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 3),Assign' (Var' "x") (Var' "_t0"),Assign' (Var' "_t1") (Val' 2),NotEq' (Var' "_t2") (Var' "x") (Var' "_t1"),Bzero' (Var' "_t2") 11,Jump' 9,Assign' (Var' "_t3") (Val' 1111),Print' "print expr = " (Var' "_t3"),Assign' (Var' "_t4") (Val' 0),Return' (Var' "_t4")]


-- monad = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 2),Assign' (Var' "x") (Var' "_t0"),Assign' (Var' "_t1") (Val' 1),Equal' (Var' "_t2") (Var' "x") (Var' "_t1"),Bzero' (Var' "_t2") 9,Jump' 13,Assign' (Var' "_t3") (Val' 1),Lt' (Var' "_t4") (Var' "x") (Var' "_t3"),Bzero' (Var' "_t4") 15,Jump' 13,Print' "x= " (Var' "x"),Jump' 22,Assign' (Var' "_t5") (Val' 3),Equal' (Var' "_t6") (Var' "x") (Var' "_t5"),Bzero' (Var' "_t6") 21,Jump' 19,Assign' (Var' "_t7") (Val' 3),Print' "print expr = " (Var' "_t7"),Print' "x= " (Var' "x"),Assign' (Var' "_t8") (Val' 0),Return' (Var' "_t8")]

-- holyHellPlzWork = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 4),Assign' (Var' "x") (Var' "_t0"),Assign' (Var' "_t1") (Val' 2),Assign' (Var' "y") (Var' "_t1"),Assign' (Var' "_t2") (Val' (-1)),Assign' (Var' "z") (Var' "_t2"),Assign' (Var' "_t3") (Val' 2),Gt' (Var' "_t4") (Var' "x") (Var' "_t3"),Bzero' (Var' "_t4") 14,Jump' 13,Print' "x= " (Var' "x"),Assign' (Var' "_t5") (Val' 2),Lt' (Var' "_t6") (Var' "y") (Var' "_t5"),Bzero' (Var' "_t6") 19,Jump' 18,Print' "y= " (Var' "y"),Assign' (Var' "_t7") (Val' 2),NotEq' (Var' "_t8") (Var' "z") (Var' "_t7"),Bzero' (Var' "_t8") 25,Jump' 23,Print' "z= " (Var' "z"),Jump' 26,Print' "y= " (Var' "y"),Print' "z= " (Var' "z"),Le' (Var' "_t9") (Var' "z") (Var' "y"),Bzero'(Var' "_t9") 0,Jump' 0,Print' "x= " (Var' "x"),Plus' (Var' "_t10") (Var' "x") (Var' "y"),Gt' (Var' "_t11") (Var' "_t10") (Var' "z"),Bzero' (Var' "_t11") 37,Jump' 35,Print' "y= " (Var' "y"),Jump' 38,Print' "z= " (Var' "z"),Jump' 40,Print' "z= " (Var' "z"),Assign' (Var' "_t12") (Val' 0),Return' (Var' "_t12")]


-- test2TestProj = [Def "main" [] (Block [Assign "x" (Val 4),Assign "y" (Val 2),Assign "z" (Val (-1)),If (Gt (Var "x") (Val 2)) (Block [Print (Var "x")]),If (Lt (Var "y") (Val 2)) (Block [Print (Var "y")]),IfElse (NEq (Var "z") (Val 2)) (Block [Print (Var "z")]) (Block [Print (Var "y")]),Print (Var "z"),IfElse (LtEq (Var "z") (Var "y")) (Block [Print (Var "x"),IfElse (Gt (Plus (Var "x") (Var "y")) (Var "z")) (Block [Print (Var "y")]) (Block [Print (Var "z")])]) (Block [Print (Var "z")]),Ret (Val 0)])]

-- testIfInside = [Def "main" [] (Block [Assign "x" (Val 2),IfElse (Or (Eq (Var "x") (Val 1)) (Lt (Var "x") (Val 1))) (Block [Print (Var "x")]) (Block [If (Eq (Var "x") (Val 3)) (Block [Print (Val 3)]),Print (Var "x")]),Ret (Val 0)])]


-- testFunction = [Def "main" [] (Block [Assign "x" (Val 6),Print (Var "x"),Assign "y" (Val 8),Print (Var "y"),Assign "z" (Plus (Div (Mult (Var "x") (Var "y")) (Val 3)) (UnaryMinus (Plus (Var "x") (Mult (Val 2) (Var "y"))))),Print (Var "z"),Assign "w" (Sub (Var "z") (Mod (Var "x") (Sub (Var "x") (Val 2)))),Print (Var "w"),Ret (Val 0)])]
-- testFunction2 =  [Def "main" [] (Block [Assign "x" (Val 6),Print (Var "x"),Assign "y" (Val 8),Print (Var "y"),Assign "z" (Plus (Plus (Div (Mult (Var "x") (Var "y")) (Val 3)) (UnaryMinus (Var "x"))) (Mult (Val 2) (Var "y"))),Print (Var "z"),Assign "w" (Sub (Var "z") (Mod (Var "x") (Sub (Var "x") (Val 2)))),Print (Var "w"),Ret (Val 0)])]

-- whileOut = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 1),Assign' (Var' "k") (Var' "_t0"),Assign' (Var' "_t1") (Val' 0),Assign' (Var' "sum") (Var' "_t1"),Assign' (Var' "_t2") (Val' 10),Le' (Var' "_t3") (Var' "k") (Var' "_t2"),Bzero' (Var' "_t3") 17,Jump' 11,Plus' (Var' "_t4") (Var' "sum") (Var' "k"),Assign' (Var' "sum") (Var' "_t4"),Assign' (Var' "_t5") (Val' 1),Plus' (Var' "_t6") (Var' "k") (Var' "_t5"),Assign' (Var' "k") (Var' "_t6"),Jump' 7,Print' "sum= " (Var' "sum"),Assign' (Var' "_t7") (Val' 0),Return' (Var' "_t7")]



-- ohGod = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 6),Assign' (Var' "x") (Var' "_t0"),Print' "x= " (Var' "x"),Assign' (Var' "_t1") (Val' 8),Assign' (Var' "y") (Var' "_t1"),Print' "y= " (Var' "y"),Times' (Var' "_t2") (Var' "x") (Var' "y"),Assign' (Var' "_t3") (Val' 3),Div' (Var' "_t4") (Var' "_t2") (Var' "_t3"),Uminus' (Var' "_t5") (Var' "x"),Plus' (Var' "_t6") (Var' "_t4") (Var' "_t5"),Assign' (Var' "_t7") (Val' 2),Times' (Var' "_t8") (Var' "_t7") (Var' "y"),Plus' (Var' "_t9") (Var' "_t6") (Var' "_t8"),Assign' (Var' "z") (Var' "_t9"),Print' "z= " (Var' "z"),Assign' (Var' "_t10") (Val' 2),Minus' (Var' "_t11") (Var' "x") (Var' "_t10"),Mod' (Var' "_t12") (Var' "x") (Var' "_t11"),Minus' (Var' "_t13") (Var' "z")(Var' "_t12"),Assign' (Var' "w") (Var' "_t13"),Print' "w= " (Var' "w"),Assign' (Var' "_t14") (Val' 0),Return' (Var' "_t14")]


-- testWorkPlz = [Push', Call' 3, Halt', Assign' (Var' "_t0") (Val' 3),Assign' (Var' "_t1") (Val' 3),Div' (Var' "_t2") (Var' "_t0") (Var' "_t1"),Assign' (Var' "_t3") (Val' 1),Plus' (Var' "_t4") (Var' "_t2") (Var' "_t3"),Print' "x" (Var' "_t4"), Return' (Var' "_t4")]
-- xfunc = Def "x" [] (Ret (Plus (Div (Val 3) (Val 3)) (Val 1)))

-- ohMyGod = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 6),Assign' (Var' "x") (Var' "_t0"),Print' "x= " (Var' "x"),Assign' (Var' "_t1") (Val' 8),Assign' (Var' "y") (Var' "_t1"),Print' "y= " (Var' "y"),Times' (Var' "_t2") (Var' "x") (Var' "y"),Assign' (Var' "_t3") (Val' 3),Div' (Var' "_t4") (Var' "_t2") (Var' "_t3"),Assign' (Var' "z") (Var' "_t4"),Print' "z= " (Var' "z"),Assign' (Var' "_t5") (Val' 0),Return' (Var' "_t5")]

-- ohMyGod8 = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 6),Assign' (Var' "x") (Var' "_t0"),Print' "x= " (Var' "x"),Assign' (Var' "_t1") (Val' 8),Assign' (Var' "y") (Var' "_t1"),Print' "y= " (Var' "y"),Times' (Var' "_t2") (Var' "x") (Var' "y"),Assign' (Var' "_t3") (Val' 3),Div' (Var' "_t4") (Var' "_t2") (Var' "_t3"),Uminus' (Var' "_t5") (Var' "x"),Plus' (Var' "_t6") (Var' "_t4") (Var' "_t5"),Assign' (Var' "z") (Var' "_t6"),Print' "z= " (Var' "z"),Assign' (Var' "_t7") (Val' 0),Return' (Var' "_t7")]

-- ohMy = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 6),Assign' (Var' "x") (Var' "_t0"),Print' "x= " (Var' "x"),Assign' (Var' "_t1") (Val' 8),Assign' (Var' "y") (Var' "_t1"),Print' "y= " (Var' "y"),Times' (Var' "_t2") (Var' "x") (Var' "y"),Assign' (Var' "_t3") (Val' 3),Div' (Var' "_t4") (Var' "_t2") (Var' "_t3"),Assign' (Var' "_t5") (Val' 2),Plus' (Var' "_t6") (Var' "x") (Var' "_t5"),Uminus' (Var' "_t7") (Var' "_t6"),Plus' (Var' "_t8") (Var' "_t4") (Var' "_t7"),Assign' (Var' "z") (Var' "_t8"),Print' "z= " (Var' "z"),Assign' (Var' "_t9") (Val' 0),Return' (Var' "_t9")]

-- oh = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 6),Assign' (Var' "x") (Var' "_t0"),Print' "x= " (Var' "x"),Assign' (Var' "_t1") (Val' 8),Assign' (Var' "y") (Var' "_t1"),Print' "y= " (Var' "y"),Times' (Var' "_t2") (Var' "x") (Var' "y"),Assign' (Var' "_t3") (Val' 3),Div' (Var' "_t4") (Var' "_t2") (Var' "_t3"),Minus' (Var' "_t5") (Var' "_t4") (Var' "x"),Assign' (Var' "_t6") (Val' 2),Plus' (Var' "_t7") (Var' "_t5") (Var' "_t6"),Assign' (Var' "z") (Var' "_t7"),Print' "z= " (Var' "z"),Assign' (Var' "_t8") (Val' 0),Return' (Var' "_t8")]






-- runThis = [Push',Call' 3,Halt',Assign' (Var' "_t0") (Val' 6),Assign' (Var' "x") (Var' "_t0"),Print' "x= " (Var' "x"),Assign' (Var' "_t1") (Val' 8),Assign' (Var' "y") (Var' "_t1"),Print' "y= " (Var' "y"),Times' (Var' "_t2") (Var' "x") (Var' "y"),Assign' (Var' "_t3") (Val' 3),Div' (Var' "_t4") (Var' "_t2") (Var' "_t3"),Minus' (Var' "_t5") (Var' "_t4") (Var' "x"),Assign' (Var' "_t6") (Val' 2),Plus' (Var' "_t7") (Var' "_t5") (Var' "_t6"),Assign' (Var' "z") (Var' "_t7"),Print' "z= " (Var' "z"),Assign' (Var' "_t8") (Val' 0),Return' (Var' "_t8")]

-- testWithoutUmin = [Def "main" [] (Block [Assign "x" (Val 6),Print (Var "x"),Assign "y" (Val 8),Print (Var "y"),Assign "z" (Plus (Sub (Div (Mult (Var "x") (Var "y")) (Val 3)) (Var "x")) (Val 2)),Print (Var "z"),Ret (Val 0)])]






--[push,call 3,halt,_t0 = 6,x = _t0,print "x= " x,_t1 = 8,y = _t1,print "y= " y,_t2 = x * y,_t3 = 3,_t4 = _t2 / _t3,_t5 = 2,_t6 = x + _t5,_t7 = - _t6,_t8 = _t4 + _t7,z = _t8,print "z= " z,_t9 = 0,return _t9]