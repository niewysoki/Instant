{-# LANGUAGE FlexibleInstances #-}

module Instant.Jvm.Transpiler where

import Control.Monad.State (StateT, evalStateT, gets, modify)
import Data.Foldable (foldl')
import qualified Data.Map as M
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TLB
import Instant.Common (Emit (emit), withIndent)
import Instant.Grammar.AbsInstant (BNFC'Position, Exp, Exp' (..), Ident, Program, Program' (Prog), Stmt, Stmt' (..))
import Instant.Grammar.ErrM (Err)
import Instant.Jvm.Instructions (BinOp (..), Instruction (..), Loc, commutative)

type Store = M.Map Ident Loc

type Transpiler x = StateT Store Err x

class Transp x where
    transpile :: x -> Transpiler (Int, TLB.Builder)

transpileProgramToJasmin :: String -> Program -> Err Text
transpileProgramToJasmin name prog = do
    x <- evalStateT (transpile prog) M.empty
    return $ toStrict $ TLB.toLazyText $ prefix <> snd x
  where
    prefix :: TLB.Builder
    prefix =
        TLB.fromString $
            unlines
                [ ".class public " ++ name
                , ".super java/lang/Object"
                , ".method public <init>()V"
                , withIndent ".limit stack 1"
                , withIndent ".limit locals 1"
                , withIndent "aload_0"
                , withIndent "invokespecial java/lang/Object/<init>()V"
                , withIndent "return"
                , ".end method"
                , ""
                ]

instance Transp Program where
    transpile (Prog _ stmts) = do
        results <- mapM transpile stmts
        locals <- gets M.size
        let stack = foldl' max 0 $ map fst results
        let code = foldl' (<>) (TLB.fromString "") $ map snd results
        return (0, mainHeader stack locals <> code <> mainFooter)
      where
        mainFooter :: TLB.Builder
        mainFooter = TLB.fromString $ withIndent "return\n" ++ ".end method\n"

        mainHeader :: Int -> Int -> TLB.Builder
        mainHeader stack locals =
            TLB.fromString $
                unlines
                    [ ".method public static main([Ljava/lang/String;)V"
                    , withIndent ".limit stack " ++ show (max stack 1)
                    , withIndent ".limit locals " ++ show (max locals 1)
                    , ""
                    ]

instance Transp Stmt where
    transpile (SExp _ x) = do
        (stack, code) <- transpile x
        return (stack, code <> emit IPrint)
    transpile (SAss _ ident x) = do
        (stack, code) <- transpile x
        loc <- getLoc ident (newLoc ident)
        return (stack, code <> emit (IStore loc))

instance Transp Exp where
    transpile (ExpAdd pos x y) = transpileBinOp OpAdd pos x y
    transpile (ExpMul pos x y) = transpileBinOp OpMul pos x y
    transpile (ExpSub pos x y) = transpileBinOp OpSub pos x y
    transpile (ExpDiv pos x y) = transpileBinOp OpDiv pos x y
    transpile (ExpLit _ value) = return (1, emit (IConst $ fromInteger value))
    transpile (ExpVar pos ident) = do
        loc <- getLoc ident (noLoc ident pos)
        return (1, emit (ILoad loc))

transpileBinOp :: BinOp -> BNFC'Position -> Exp -> Exp -> Transpiler (Int, TLB.Builder)
transpileBinOp op _ left right = do
    (leftStack, leftCode) <- transpile left
    (rightStack, rightCode) <- transpile right
    let stack = max (1 + min leftStack rightStack) (max leftStack rightStack)
    let opCode = emit (IBinOp op)
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
