module CCompiler where

import Ast
import ICInterpreter
import Data.Map as Map
import Debug.Trace



type Global = Map String (Int, [String])

data Unsafe a = Ok a | Error String deriving Show

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


-- backPatch       TL     FL     CL     BL
type BackPatch = ([Int], [Int], [Int], [Int])

emptyBP :: BackPatch
emptyBP = ([],[],[],[])


type Temp = [Op]

temps :: Temp
temps = [Var' ("_t" ++ (show n)) | n <- [0,1..]]


run a = compileExpr a [] emptyBP temps Map.empty

--going to need to have a global mapping for all the function localtions and parameters, Map String (Int, [String])

--Compile :: Program -> IC_Program
--Compile' :: Program -> IC_Program -> Temps -> Gmap


-- compileStmt :: Stmt -> IC_Program -> Temps -> Gmap -> (IC_Program, BackPatch, Temp)
-- compileStmt (Def funcName args body) ic temp g = 
--     let (icNew, bp, tempNew) = compileStmt body ic temp (Map.insert funcName ((length ic), args) g)

-- compileStmt [] ic temp g = (ic, emptyBP, temp)
-- compileStmt (Assign var expr) ic temp g = 
    -- let (loc,icRes,bp,temp) = 
-- compileStmt (Break) ic temp g = 
--     let loc = length ic 
--     in ((ic ++ [(Jump' 0)]), ([],[],[loc],[]), temps) cont would go in the other one 

-- compileStmt (While cond body) ic temp bp g = 
--     Let start = length ic 
--         (_,ic2,(tl,fl,_,_), temp2) = compileExpr cond ic temp bg g
--         loop = length ic2
--         (ic3,(_,_,bl,cl), temp3) = evalStmt body ic2 temp2 g
--         ic4 = backPatch tl loop ic3
--         ic5 = ic4 ++ [Jump' start] 
--         ic6 = backPatch (bl ++ fl) (length ic5)
--         ic7 = backPatch cl start ic6

-- (Var' "_t8",[Assign' (Var' "_t0") (Val' 2),Assign' (Var' "_t1") (Val' 2),Plus' (Var' "_t2") (Var' "_t0") (Var' "_t1"),Assign' (Var' "_t3") (Val' 3),Assign' (Var' "_t4") (Val' 1),Assign' (Var' "_t5") (Val' 1),Minus' (Var' "_t6")
-- (Var' "_t4") (Var' "_t5"),Div' (Var' "_t7") (Var' "_t3") (Var' "_t6"),Ge' (Var' "_t8") (Var' "_t2") (Var' "_t7"),Bzero' (Var' "_t8") 0,Jump' 0],([10],[9],[],[]),[Var' "_t9"])



x = [Assign' (Var' "_t0") (Val' 2),
     Assign' (Var' "_t1") (Val' 2),
     Plus' (Var' "_t2") (Var' "_t0") (Var' "_t1"),
     Assign' (Var' "_t3") (Val' 3),
     Assign' (Var' "_t4") (Val' 1),
     Assign' (Var' "_t5") (Val' 1),
     Minus' (Var' "_t6") (Var' "_t4") (Var' "_t5"),
     Div' (Var' "_t7") (Var' "_t3") (Var' "_t6"),
     Ge' (Var' "_t8") (Var' "_t2") (Var' "_t7"),
     Bzero' (Var' "_t8") 0,Jump' 0]
lst = [10]
line = 12

test = backPatch lst line x



--test = run (GtEq (Plus (Val 2) (Val 2)) (Div (Val 3) (Sub (Val 1) (Val 1))))
testPatch = run (And (Gt (Val 2) (Val 1)) (Lt (Val 1) (Val 2)))
backPatch :: [Int] -> Int -> IC_Program -> IC_Program
backPatch [] set [] = []
backPatch (x:xs) set [] = []
backPatch [] set (step:rest) = step:rest
backPatch (x:xs) set ((Bzero' t _):rest) = traceShow (t, rest) (Bzero' t set):(backPatch xs set rest)
backPatch (x:xs) set ((Jump' _):rest) = traceShow (rest) (Jump' set):(backPatch xs set rest)
backPatch (x:xs) set (step:rest) = step:(backPatch xs set rest)
--     let currentPlace = length ((Bzero' t current):rest)
--         test = traceShow currentPlace 
--     in 
--         if currentPlace == x 
--             then (Bzero' t set):(backPatch xs set rest)
--             else ((Bzero' t current):(backPatch xs set rest))
-- backPatch (x:xs) set ((Jump' current):rest) = 
--     let currentPlace = length ((Jump' current):rest) in 
--         if currentPlace == x 
--             then (Jump' set):(backPatch xs set rest)
--             else ((Jump' current):(backPatch xs set rest))
-- backPatch [] set (step:rest) = step:rest




compileExpr :: Expr -> IC_Program -> BackPatch -> Temp -> Global -> (Op,IC_Program,BackPatch,Temp)
compileExpr (Val i) ic bp (t:rest) g = (t, (ic ++ [Assign' (t) (Val' (fromIntegral i))]), bp, rest) -- going to need to change for testing purposes
compileExpr (Plus l r) ic bp temp g = let (locl, icL, bpl, templ) = compileExpr l ic bp temp g
                                          (locr, icR, bpr, (t:rest)) = compileExpr r icL bpl templ g
									   in (t, (icR ++ [ (Plus' (t) (locl) (locr))]), bpr, rest)
compileExpr (Sub x y) ic bp temp g = let  (locl, icL, bpl, templ) = compileExpr x ic bp temp g
                                          (locr, icR, bpr, (t:rest)) = compileExpr y icL bpl templ g
                                     in (t, (icR ++ [ (Minus' (t) (locl) (locr))]), bpr, rest)
compileExpr (Mult x y) ic bp temp g = let (locl, icL, bpl, templ) = compileExpr x ic bp temp g
                                          (locr, icR, bpr, (t:rest)) = compileExpr y icL bpl templ g
                                      in (t, (icR ++ [ (Times' (t) (locl) (locr))]), bpr, rest)
compileExpr (Div x y) ic bp temp g = let (locl, icL, bpl, templ) = compileExpr x ic bp temp g
                                         (locr, icR, bpr, (t:rest)) = compileExpr y icL bpl templ g
                                     in (t, (icR ++ [ (Div' (t) (locl) (locr))]), bpr, rest)
compileExpr (Mod x y) ic bp temp g = let (locl, icL, bpl, templ) = compileExpr x ic bp temp g
                                         (locr, icR, bpr, (t:rest)) = compileExpr y icL bpl templ g
                                     in (t, (icR ++ [ (Mod' (t) (locl) (locr))]), bpr, rest)
compileExpr (Lt x y) ic bp temp g = 
    let (locx, ic2, bpl, templ) = compileExpr x ic bp temp g
        (locy, ic3, bpr, (t:rest)) = compileExpr y ic2 bpl templ g
        ic4 = ic3 ++  [Lt' t locx locy]
        fLoc = length ic4
        tLoc = fLoc + 1
        ic5 = ic4 ++ [(Bzero' t 0), (Jump' 0)]
    in (t, (ic5), ([tLoc], [fLoc], [],[]), [head rest])
compileExpr (LtEq x y) ic bp temp g = 
    let (locx, ic2, bpl, templ) = compileExpr x ic bp temp g
        (locy, ic3, bpr, (t:rest)) = compileExpr y ic2 bpl templ g
        ic4 = ic3 ++  [Le' t locx locy]
        fLoc = length ic4
        tLoc = fLoc + 1
        ic5 = ic4 ++ [Bzero' t 0, Jump' 0]
    in (t, (ic5), ([tLoc], [fLoc], [],[]), rest)
compileExpr (Eq x y) ic bp temp g = 
    let (locx, ic2, bpl, templ) = compileExpr x ic bp temp g
        (locy, ic3, bpr, (t:rest)) = compileExpr y ic2 bpl templ g
        ic4 = ic3 ++  [Equal' t locx locy]
        fLoc = length ic4
        tLoc = fLoc + 1
        ic5 = ic4 ++ [Bzero' t 0, Jump' 0]
    in (t, (ic5), ([tLoc], [fLoc], [],[]), rest)
compileExpr (NEq x y) ic bp temp g = 
    let (locx, ic2, bpl, templ) = compileExpr x ic bp temp g
        (locy, ic3, bpr, (t:rest)) = compileExpr y ic2 bpl templ g
        ic4 = ic3 ++  [NotEq' t locx locy]
        fLoc = length ic4
        tLoc = fLoc + 1
        ic5 = ic4 ++ [Bzero' t 0, Jump' 0]
    in (t, (ic5), ([tLoc], [fLoc], [],[]), rest)
compileExpr (Gt x y) ic bp temp g = 
    let (locx, ic2, bpl, templ) = compileExpr x ic bp temp g
        (locy, ic3, bpr, (t:rest)) = compileExpr y ic2 bpl templ g
        ic4 = ic3 ++  [Gt' t locx locy]
        fLoc = length ic4
        tLoc = fLoc + 1
        ic5 = ic4 ++ [Bzero' t 0, Jump' 0]
    in (t, (ic5), ([tLoc], [fLoc], [],[]), [head rest])
compileExpr (GtEq x y) ic bp temp g = 
    let (locx, ic2, bpl, templ) = compileExpr x ic bp temp g
        (locy, ic3, bpr, (t:rest)) = compileExpr y ic2 bpl templ g
        ic4 = ic3 ++  [Ge' t locx locy]
        fLoc = length ic4
        tLoc = fLoc + 1
        ic5 = ic4 ++ [Bzero' t 0, Jump' 0]
    in (t, (ic5), ([tLoc], [fLoc], [],[]), [head rest])
compileExpr (And x y) ic bp temp g =
    let (_, ic2, (tL,fL,bL,cL), temp2) = compileExpr x ic bp temp g
        (_, ic3, (tL2,fL2,bL2,cL2), temp3) = compileExpr y ic2 bp temp2 g 
        ic4 = backPatch tL (length ic2) ic3
    in (Var' "", ic4,(tL2,(fL++fL2),bL2,cL2),[head temp3])


-- compileExpr (And x y) = undefined
-- compileExpr (Or x y) = undefined
-- compileExpr (Not x) = undefined

