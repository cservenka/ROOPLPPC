module VectorAnalyzer where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import AST

type Size = Integer

data VAState = VAState
  { vectors :: [TypeName]
  , vectorElementSize :: [(TypeName, Size)]
  } deriving (Show, Eq)

newtype VectorAnalyzer a = VectorAnalyzer
  { runVA :: StateT VAState (Except String) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState
             , VAState
             , MonadError String
             )

initialState :: VAState
initialState = VAState {vectors = [], vectorElementSize = []}

setVectors :: VectorDeclaration -> VectorAnalyzer ()
setVectors v@(GVDecl _ t _) = modify $ \s -> s {vectors = t : vectors s}

vaProgram :: Program -> VectorAnalyzer Program
vaProgram (GProg p) = do
  mapM_ setVectors p
  mapM_ setVectorElementSize p
  return GProg

vectorAnalysis :: Program -> Except String (Program, VAState)
vectorAnalysis p = runStateT (runVA $ vaProgram p) initialState
