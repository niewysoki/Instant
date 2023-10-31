{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Instant.Llvm.Transpiler (run) where

import Control.Monad.State (StateT, evalStateT, gets, modify)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromString, toLazyText)
import Instant.Common (Emit (emit))
import Instant.Grammar.AbsInstant (
    BNFC'Position,
    Exp,
    Exp' (..),
    Ident (..),
    Program,
    Program' (..),
    Stmt,
    Stmt' (..),
 )
import Instant.Grammar.ErrM (Err)
import Instant.Grammar.ParInstant (myLexer, pProgram)
import Instant.Llvm.Instructions (
    BinOp (..),
    CallParam (..),
    ComplexInstruction (..),
    Instruction (..),
    Register (..),
    nextRegister,
 )

type AllocRegisters = M.Map Ident Register

data TranspilerState = State {_registers :: AllocRegisters, _first_unused_register :: Register, _division_number :: Integer}

type family TranspilerResult a where
    TranspilerResult Program = Builder
    TranspilerResult Stmt = Builder
    TranspilerResult Exp = (CallParam, Builder)

type Transpiler x = StateT TranspilerState Err x

run :: String -> Err Text
run text = do
    ast <- pProgram . myLexer $ text
    code <- evalStateT (transpile ast) State{_registers = M.empty, _first_unused_register = RegNum 0, _division_number = 0}
    return $ toStrict $ toLazyText code

class Transp code where
    transpile :: code -> Transpiler (TranspilerResult code)

instance Transp Program where
    transpile (Prog _ stmts) = do
        codePieces <- mapM transpile stmts
        let mainBody = L.foldl' (<>) (fromString "") codePieces
        return $ emit CIPrintInt <> emit CIMainHeader <> mainBody <> emit CIMainFooter

instance Transp Stmt where
    transpile (SExp _ x) = do
        (param, expCode) <- transpile x
        let printCode = emit $ IPrint param
        return $ expCode <> printCode
    transpile (SAss _ ident x) = do
        (param, expCode) <- transpile x
        (Reg reg, allocCode) <- getReg ident (alloc ident)
        let assignCode = emit $ IStore reg param
        return $ allocCode <> expCode <> assignCode

instance Transp Exp where
    transpile (ExpAdd _ x y) = transpileBinOp OpAdd x y
    transpile (ExpMul _ x y) = transpileBinOp OpMul x y
    transpile (ExpSub _ x y) = transpileBinOp OpSub x y
    transpile (ExpDiv _ x y) = transpileBinOp OpDiv x y
    transpile (ExpLit _ value) = return (Const $ fromInteger value, fromString "")
    transpile (ExpVar pos ident) = do
        (Reg source, _) <- getReg ident (noReg ident pos)
        destination <- newReg
        let code = emit $ ILoad destination source
        return (Reg destination, code)

transpileBinOp :: BinOp -> Exp -> Exp -> Transpiler (TranspilerResult Exp)
transpileBinOp op x y = do
    (xParam, xCode) <- transpile x
    (yParam, yCode) <- transpile y
    destination <- newReg
    runtimeFailCode <- getRuntimeDivisionByZeroCheck op yParam
    let code = emit $ IBinOp op destination xParam yParam
    return (Reg destination, xCode <> yCode <> runtimeFailCode <> code)

getRuntimeDivisionByZeroCheck :: BinOp -> CallParam -> Transpiler Builder
getRuntimeDivisionByZeroCheck OpDiv param = do
    num <- gets _division_number
    modify (\s -> s{_division_number = 1 + num})
    return $ emit $ CIDivisionCheck num param
getRuntimeDivisionByZeroCheck _ _ = return $ fromString ""

getReg :: Ident -> Transpiler (TranspilerResult Exp) -> Transpiler (TranspilerResult Exp)
getReg ident onFail = do
    maybeRegister <- gets (M.lookup ident . _registers)
    case maybeRegister of
        Nothing -> onFail
        Just reg -> return (Reg reg, fromString "")

alloc :: Ident -> Transpiler (TranspilerResult Exp)
alloc ident = do
    reg <- newReg
    modify (bindRegister ident reg)
    return (Reg reg, emit $ IAlloc reg)
  where
    bindRegister i r s = s{_registers = M.insert i r (_registers s)}

newReg :: Transpiler Register
newReg = do
    reg <- gets _first_unused_register
    modify (\s -> s{_first_unused_register = nextRegister reg})
    return reg

noReg :: Ident -> BNFC'Position -> Transpiler (TranspilerResult Exp)
noReg (Ident ident) (Just (row, col)) = fail $ "Uninitialized variable '" ++ show ident ++ "' at " ++ show row ++ ":" ++ show col
noReg (Ident ident) Nothing = fail $ "Uninitialized variable '" ++ show ident ++ "' at unknown position"