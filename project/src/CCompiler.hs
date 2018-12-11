module CCompiler where

import Ast
import ICInterpreter


compile :: Program -> IC_Program
compile (P (head:rest)) = [compileStmt head]++ compile (P rest)

compileStmt :: Stmt -> IC_Instruction
compileStmt = undefined

compileExpr :: Expr -> IC_Instruction
compileExpr = undefined