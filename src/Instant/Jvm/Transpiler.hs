{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Instant.Jvm.Transpiler (run) where

import Control.Monad.State (StateT, evalStateT, gets, modify)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder as TLB (Builder, fromString, toLazyText)
import Instant.Common (Emit (emit))
import Instant.Grammar.AbsInstant (
    BNFC'Position,
    Exp,
    Exp' (..),
    Ident,
    Program,
    Program' (Prog),
    Stmt,
    Stmt' (SAss, SExp),
 )
import Instant.Grammar.ErrM (Err)
import Instant.Grammar.ParInstant (myLexer, pProgram)
import Instant.Jvm.Instructions (
    BinOp (..),
    Instruction (..),
    Loc,
    StaticCode (..),
    commutative,
 )

type Store = M.Map Ident Loc

type Transpiler x = StateT Store Err x

type family TranspilerResult a where
    TranspilerResult Program = Builder
    TranspilerResult Stmt = (Int, Builder)
    TranspilerResult Exp = (Int, Builder)

class Transp code where
    transpile :: code -> Transpiler (TranspilerResult code)

run :: String -> String -> Err Text
run name text = do
    ast <- pProgram . myLexer $ text
    code <- evalStateT (transpile ast) M.empty
    return $ toStrict $ toLazyText $ emit (SCClassHeader name) <> code

instance Transp Program where
    transpile (Prog _ stmts) = do
        results <- mapM transpile stmts
        locals <- gets M.size
        let stack = L.foldl' max 0 $ fst <$> results
            code = L.foldl' (<>) (fromString "") $ snd <$> results
        return $ emit SCDefaultConstructor <> emit (SCMainHeader stack locals) <> code <> emit SCMainFooter

instance Transp Stmt where
    transpile (SExp _ x) = do
        (stack, code) <- transpile x
        return (stack + 1, code <> emit IPrintLoadFunc <> emit ISwap <> emit IPrint)
    transpile (SAss _ ident x) = do
        (stack, code) <- transpile x
        loc <- getLoc ident (newLoc ident)
        return (stack, code <> emit (IStore loc))

instance Transp Exp where
    transpile (ExpAdd _ x y) = transpileBinOp OpAdd x y
    transpile (ExpMul _ x y) = transpileBinOp OpMul x y
    transpile (ExpSub _ x y) = transpileBinOp OpSub x y
    transpile (ExpDiv _ x y) = transpileBinOp OpDiv x y
    transpile (ExpLit _ value) = return (1, emit (IConst $ fromInteger value))
    transpile (ExpVar pos ident) = do
        loc <- getLoc ident (noLoc ident pos)
        return (1, emit (ILoad loc))

transpileBinOp :: BinOp -> Exp -> Exp -> Transpiler (Int, Builder)
transpileBinOp op left right = do
    (leftStack, leftCode) <- transpile left
    (rightStack, rightCode) <- transpile right
    let stack = max (1 + min leftStack rightStack) (max leftStack rightStack)
        opCode = emit (IBinOp op)
    if leftStack > rightStack
        then return (stack, leftCode <> rightCode <> opCode)
        else
            if commutative op
                then return (stack, rightCode <> leftCode <> opCode)
                else return (stack, rightCode <> leftCode <> emit ISwap <> opCode)

getLoc :: Ident -> Transpiler Loc -> Transpiler Loc
getLoc ident onFail = do
    maybeLoc <- gets (M.lookup ident)
    maybe onFail return maybeLoc

newLoc :: Ident -> Transpiler Loc
newLoc ident = do
    loc <- gets M.size
    modify (M.insert ident loc)
    return loc

noLoc :: Ident -> BNFC'Position -> Transpiler Loc
noLoc ident pos = fail $ "Uninitialized variable '" ++ show ident ++ "' at " ++ show pos
