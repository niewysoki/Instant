{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Instant.Llvm.Transpiler where

import Control.Monad.State (StateT, gets, modify)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text.Lazy.Builder (Builder, fromString)
import Instant.Common (Emit (emit))
import Instant.Grammar.AbsInstant (
    BNFC'Position,
    Exp,
    Exp' (..),
    Ident,
    Program,
    Program' (..),
    Stmt,
    Stmt' (..),
 )
import Instant.Grammar.ErrM (Err)
import Instant.Llvm.Instructions (
    BinOp (..),
    CallParam (..),
    Instruction (..),
    Register,
    StaticCode (..),
    nextRegister,
 )

type AllocRegisters = M.Map Ident Register

data TranspilerState = State {_registers :: AllocRegisters, _first_unused_register :: Register}

type family TranspilerResult a where
    TranspilerResult Program = Builder
    TranspilerResult Stmt = Builder
    TranspilerResult Exp = (CallParam, Builder)

type Transpiler x = StateT TranspilerState Err x

class Transp code where
    transpile :: code -> Transpiler (TranspilerResult code)

instance Transp Program where
    transpile (Prog _ stmts) = do
        codePieces <- mapM transpile stmts
        let mainBody = L.foldl' (<>) (fromString "") codePieces
        return $ emit SCPrintInt <> emit SCMainHeader <> mainBody <> emit SCMainFooter

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
    transpile (ExpAdd pos x y) = transpileBinOp OpAdd pos x y
    transpile (ExpMul pos x y) = transpileBinOp OpMul pos x y
    transpile (ExpSub pos x y) = transpileBinOp OpSub pos x y
    transpile (ExpDiv pos x y) = transpileBinOp OpDiv pos x y
    transpile (ExpLit _ value) = return (Const $ fromInteger value, fromString "")
    transpile (ExpVar pos ident) = do
        (Reg source, _) <- getReg ident (noReg ident pos)
        destination <- newReg
        let code = emit $ ILoad destination source
        return (Reg destination, code)

transpileBinOp :: BinOp -> BNFC'Position -> Exp -> Exp -> Transpiler (TranspilerResult Exp)
transpileBinOp op pos x y = do
    (xParam, xCode) <- transpile x
    (yParam, yCode) <- transpile y
    destination <- newReg
    let code = emit $ IBinOp op destination xParam yParam
    return (Reg destination, xCode <> yCode <> code)

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
noReg ident pos = fail $ "Uninitialized variable '" ++ show ident ++ "' at " ++ show pos