module Instant.Jvm.Instructions where

import Data.Int (Int32)
import Data.Text.Lazy.Builder (fromString)
import Instant.Common (Emit (emit), withIndent)

type Loc = Int

data Instruction
    = ILoad Loc
    | IStore Loc
    | IConst Int32
    | IPrint
    | IBinOp BinOp
    | ISwap

data BinOp = OpAdd | OpSub | OpDiv | OpMul

commutative :: BinOp -> Bool
commutative OpAdd = True
commutative OpMul = True
commutative _ = False

instance Show BinOp where
    show OpAdd = "iadd"
    show OpSub = "isub"
    show OpDiv = "idiv"
    show OpMul = "imul"

instance Show Instruction where
    show IPrint = "invokestatic Runtime/printInt(I)V"
    show ISwap = "swap"
    show (IBinOp op) = show op
    show (ILoad loc)
        | loc < 4 = "iload_" ++ show loc
        | otherwise = "iload " ++ show loc
    show (IStore loc)
        | loc < 4 = "istore_" ++ show loc
        | otherwise = "istore " ++ show loc
    show (IConst n)
        | n == -1 = "iconst_m1"
        | n >= min_const && n <= max_const = "iconst_" ++ show n
        | n >= min_one_byte && n <= max_one_byte = "bipush " ++ show n
        | n >= min_two_bytes && n <= max_two_bytes = "sipush " ++ show n
        | otherwise = "ldc " ++ show n
      where
        min_const = 0 :: Int32
        max_const = 5 :: Int32
        min_one_byte = -128 :: Int32
        max_one_byte = 127 :: Int32
        min_two_bytes = -32768 :: Int32
        max_two_bytes = 32767 :: Int32

instance Emit Instruction where
    emit val = fromString $ withIndent $ show val ++ "\n"

data StaticCode
    = SCClassHeader String
    | SCDefaultConstructor
    | SCMainHeader Int Int
    | SCMainFooter

instance Emit StaticCode where
    emit (SCClassHeader name) = fromString $ unlines [".class public " ++ name, ".super java/lang/Object"]
    emit SCDefaultConstructor =
        fromString $
            unlines
                [ ".method public <init>()V"
                , withIndent ".limit stack 1"
                , withIndent ".limit locals 1"
                , withIndent "aload_0"
                , withIndent "invokespecial java/lang/Object/<init>()V"
                , withIndent "return"
                , ".end method"
                , ""
                ]
    emit (SCMainHeader stack locals) =
        fromString $
            unlines
                [ ".method public static main([Ljava/lang/String;)V"
                , withIndent ".limit stack " ++ show (max stack 1)
                , withIndent ".limit locals " ++ show (max locals 1)
                , ""
                ]
    emit SCMainFooter = fromString $ unlines [withIndent "return", ".end method"]