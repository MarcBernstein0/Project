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
                                     in (t, (icR ++ [ (Mod' (t) (locl) (locr))]), bpr, [head rest])
-- compileExpr (Eq x y) ic bp temp g = 
--     let (locl, icL, bpl, templ) = compileExpr l ic bp temp g
--         (locr, icR, bpr, (t:rest)) = compileExpr r icL bpl templ g
--     in ()
-- compileExpr (NEq x y) = undefined
-- compileExpr (Lt x y) = undefined
-- compileExpr (LtEq x y) = undefined
-- compileExpr (Gt x y) = undefined
-- compileExpr (GtEq x y) = undefined
-- compileExpr (And x y) = undefined
-- compileExpr (Or x y) = undefined
-- compileExpr (Not x) = undefined
