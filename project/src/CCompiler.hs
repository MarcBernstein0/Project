module CCompiler where

import Ast
import ICInterpreter


compile :: Program -> IC_Program
compile (P (head:rest)) = [compileStmt head]++ compile (P rest)

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




compileExpr :: Expr -> IC_Instruction
compileExpr Plus x y = undefined
compileExpr Sub x y = undefined
compileExpr Mult x y = undefined
compileExpr Div x y = undefined
compileExpr Mod x y = undefined
compileExpr Eq x y = undefined
compileExpr NEq x y = undefined
compileExpr Lt x y = undefined
compileExpr LtEq x y = undefined
compileExpr Gt x y = undefined
compileExpr GtEq x y = undefined
compileExpr And x y = undefined
compileExpr Or x y = undefined
compileExpr Not x = undefined
