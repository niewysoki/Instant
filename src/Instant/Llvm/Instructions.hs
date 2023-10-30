{-# LANGUAGE FlexibleInstances #-}

module Instant.Llvm.Instructions where

import Data.Text.Lazy.Builder (fromString)
import Instant.Common (Emit (emit), withIndent)

newtype Register = RegNum Int

instance Show Register where
    show (RegNum num) = "%R." ++ show num

nextRegister :: Register -> Register
nextRegister (RegNum num) = RegNum $ num + 1

data CallParam = Const Int | Reg Register

instance Show CallParam where
    show (Const n) = show n
    show (Reg r) = show r

data BinOp = OpAdd | OpSub | OpMul | OpDiv

instance Show BinOp where
    show OpAdd = "add i32"
    show OpSub = "sub i32"
    show OpMul = "mul i32"
    show OpDiv = "sdiv i32"

data Instruction
    = IBinOp BinOp Register CallParam CallParam
    | IPrint CallParam
    | IAlloc Register
    | IStore Register CallParam
    | ILoad Register Register

instance Show Instruction where
    show (IPrint x) = "call void @internalPrintInt(i32 " ++ show x ++ ")"
    show (IAlloc reg) = show reg ++ " = alloca i32, align 4"
    show (IStore reg x) = "store i32 " ++ show x ++ ", i32* " ++ show reg
    show (ILoad dst src) = show dst ++ " = load i32, i32* " ++ show src
    show (IBinOp op reg x y) = show reg ++ " = " ++ show op ++ " " ++ show x ++ ", " ++ show y

instance Emit Instruction where
    emit val = fromString $ withIndent $ show val ++ "\n"

data StaticCode
    = SCPrintInt
    | SCMainHeader
    | SCMainFooter

instance Emit StaticCode where
    emit SCPrintInt =
        fromString $
            unlines
                [ "declare i32 @printf(i8*, ...)"
                , "@fmt = internal constant [4 x i8] c\"%d\\0A\\00\""
                , ""
                , "define void @internalPrintInt(i32 %x) {"
                , withIndent "%t0 = getelementptr [4 x i8], [4 x i8]* @fmt, i32 0, i32 0"
                , withIndent "call i32 (i8*, ...) @printf(i8* %t0, i32 %x) "
                , withIndent "ret void"
                , "}"
                ]
    emit SCMainHeader = fromString $ "define i32 @main() {" ++ "\n"
    emit SCMainFooter = fromString $ unlines [withIndent "ret i32 0", "}"]