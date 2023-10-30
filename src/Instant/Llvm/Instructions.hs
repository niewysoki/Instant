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

data BinOp = OpAdd | OpSub | OpMul | OpDiv deriving (Eq)

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

data ComplexInstruction
    = CIPrintInt
    | CIMainHeader
    | CIMainFooter
    | CIDivisionCheck Integer CallParam

instance Emit ComplexInstruction where
    emit (CIDivisionCheck num val) =
        fromString $
            unlines
                [ withIndent "%R.cmp." ++ show num ++ " = icmp eq i32 0, " ++ show val
                , withIndent "br i1 %R.cmp." ++ show num ++ ", label %on_division_by_zero, label %on_good_" ++ show num
                , withIndent "ret i32 0"
                , "on_good_" ++ show num ++ ":"
                ]
    emit CIMainHeader = fromString $ "define i32 @main() {" ++ "\n"
    emit CIMainFooter =
        fromString $
            unlines
                [ withIndent "ret i32 0"
                , "on_division_by_zero:"
                , withIndent "call void @internalPrintDivByZeroMsg()"
                , withIndent "ret i32 0"
                , "}"
                ]
    emit CIPrintInt =
        fromString $
            unlines
                [ "declare void @exit(i32)"
                , "declare i32 @printf(i8*, ...)"
                , "@fmtInt = internal constant [4 x i8] c\"%d\\0A\\00\""
                , "@divByZeroMsgFmt = internal constant [34 x i8] c\"RUNTIME ERROR: Division by zero!\\0A\\00\""
                , ""
                , "define void @internalPrintDivByZeroMsg() {"
                , withIndent "%t0 = getelementptr [34 x i8], [34 x i8]* @divByZeroMsgFmt, i32 0, i32 0"
                , withIndent "call i32 (i8*, ...) @printf(i8* %t0)"
                , withIndent "call void @exit(i32 1)"
                , withIndent "ret void"
                , "}"
                , ""
                , "define void @internalPrintInt(i32 %x) {"
                , withIndent "%t0 = getelementptr [4 x i8], [4 x i8]* @fmtInt, i32 0, i32 0"
                , withIndent "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)"
                , withIndent "ret void"
                , "}"
                ]
