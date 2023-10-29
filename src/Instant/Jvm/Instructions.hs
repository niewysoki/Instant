module Instant.Jvm.Instructions where

import Data.Int (Int32)
import qualified Data.Text.Lazy.Builder as TB
import Instant.Utils (withIndent)

type Loc = Int

data BinOp = OpAdd | OpSub | OpDiv | OpMul

instance Show BinOp where
    show OpAdd = "iadd"
    show OpSub = "isub"
    show OpDiv = "idiv"
    show OpMul = "imul"

commutative :: BinOp -> Bool
commutative OpAdd = True
commutative OpMul = True
commutative _ = False

data Instruction
    = ILoad Loc
    | IStore Loc
    | IConst Int32
    | IPrint
    | IBinOp BinOp
    | ISwap

class Emit x where
    emit :: x -> TB.Builder

instance Emit Instruction where
    emit IPrint = TB.fromString $ withIndent "invokestatic Runtime/printInt(I)V\n"
    emit ISwap = TB.fromString $ withIndent "swap\n"
    emit (IBinOp op) = TB.fromString $ withIndent $ show op ++ "\n"
    emit (ILoad loc)
        | loc < 4 = TB.fromString $ withIndent $ "iload_" ++ show loc ++ "\n"
        | otherwise = TB.fromString $ withIndent $ "iload " ++ show loc ++ "\n"
    emit (IStore loc)
        | loc < 4 = TB.fromString $ withIndent $ "istore_" ++ show loc ++ "\n"
        | otherwise = TB.fromString $ withIndent $ "istore " ++ show loc ++ "\n"
    emit (IConst n)
        | n == -1 = TB.fromString $ withIndent "iconst_m1" ++ "\n"
        | n >= 0 && n < 6 = TB.fromString $ withIndent $ "iconst_" ++ show n ++ "\n"
        | n >= min_one_byte && n <= max_one_byte = TB.fromString $ withIndent $ "bipush " ++ show n ++ "\n"
        | n >= min_two_bytes && n <= max_two_bytes = TB.fromString $ withIndent $ "sipush " ++ show n ++ "\n"
        | otherwise = TB.fromString $ withIndent $ "ldc " ++ show n ++ "\n"
      where
        min_one_byte = -(2 ^ 7)
        max_one_byte = 2 ^ 7 - 1
        min_two_bytes = -(2 ^ 15)
        max_two_bytes = 2 ^ 15 - 1