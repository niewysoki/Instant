module Instant.Llvm.Transpiler where

import Control.Monad.State (StateT, gets)
import qualified Data.Map as M
import Data.Text.Lazy.Builder (Builder, fromString)
import Instant.Grammar.AbsInstant (Ident)
import Instant.Grammar.ErrM (Err)
import Instant.Llvm.Instructions (CallParam (Const, Reg), Register)

type Registers = M.Map Ident Register

type Transpiler x = StateT Registers Err x

data Computation = Comp {code :: Builder, param :: CallParam}

getReg :: Ident -> Transpiler Computation -> Transpiler Computation
getReg ident onFail = do
    maybeRegister <- gets (M.lookup ident)
    case maybeRegister of
        Nothing -> onFail
        Just reg -> return Comp{code = fromString "", param = Reg reg}