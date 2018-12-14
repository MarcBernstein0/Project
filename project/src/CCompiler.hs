module CCompiler where

import Ast
import ICInterpreter
import Data.Map as Map



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


test = run (GtEq (Plus (Val 2) (Val 2)) (Div (Val 3) (Sub (Val 1) (Val 1))))

backPatch :: [Int] -> Int -> IC_Program -> IC_Program
backPatch = undefined




compileExpr :: Expr -> IC_Program -> BackPatch -> Temp -> Global -> (Op,IC_Program,BackPatch,Temp)
compileExpr _ _ _ [] _ = undefined
compileExpr (Val i) ic bp (t:rest) g = (t, (ic ++ [Assign' (t) (Val' (fromIntegral i))]), bp, rest) -- going to need to change for testing purposes
compileExpr (Plus l r) ic bp temp g = let (locl, icL, bpl, templ) = compileExpr l ic bp temp g
                                          (locr, icR, bpr, (t:rest)) = compileExpr r icL bpl templ g
									   in (t, (icR ++ [ (Plus' (t) (locl) (locr))]), bpr, [head rest])
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
    in (t, (ic5), ([tLoc], [fLoc], [],[]), rest)
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
    in (Var' "", ic4,(tL2,(fL++fL2),bL2,cL2),temp3)


-- compileExpr (And x y) = undefined
-- compileExpr (Or x y) = undefined
-- compileExpr (Not x) = undefined

