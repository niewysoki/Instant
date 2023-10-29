{-# LANGUAGE FlexibleInstances #-}

module Instant.Llvm.Instructions where

import qualified Data.Text.Lazy.Builder as TLB
import Instant.Common (Emit (emit), withIndent)

newtype Register = RegNum Int

data CallParam = Const Int | Reg Register

data BinOp = OpAdd | OpSub | OpMul | OpDiv

data Instruction
    = IBinOp BinOp Register CallParam CallParam
    | IPrint CallParam
    | IAlloc Register
    | IStore Register CallParam
    | ILoad Register Register

instance Show Register where
    show r = "%R." ++ show r

instance Show CallParam where
    show (Const n) = show n
    show (Reg r) = show r

instance Show BinOp where
    show OpAdd = "add i32"
    show OpSub = "sub i32"
    show OpMul = "mul i32"
    show OpDiv = "sdiv i32"

instance Show Instruction where
    show (IPrint x) = "call void @internalPrintInt(i32 " ++ show x ++ ")"
    show (IAlloc reg) = show reg ++ " = alloca i32, align 4"
    show (IStore reg x) = "store i32 " ++ show x ++ ", i32* " ++ show reg
    show (ILoad dst src) = show dst ++ " = load i32, i32* " ++ show src
    show (IBinOp op reg x y) = show reg ++ " = " ++ show op ++ " " ++ show x ++ ", " ++ show y

instance Emit Instruction where
    emit val = TLB.fromString $ withIndent $ show val ++ "\n"