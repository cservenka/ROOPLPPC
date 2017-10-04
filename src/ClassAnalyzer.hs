{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module ClassAnalyzer
  ( classAnalysis
  , printCAState
  , CAState(..)
  ) where

import Data.List
import Data.Maybe

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Text.Pretty.Simple (pPrint)

import AST

type Size = Integer

-- |The Class Analyzer State consists of a list of classes, sizes, methods
-- |and a main class
data CAState = CAState
  { classes :: [(TypeName, ClassDeclaration)]
  , classSize :: [(TypeName, Size)]
  , classMethods :: [(TypeName, [MethodDeclaration])]
  , mainClass :: Maybe TypeName
  } deriving (Show, Eq)

-- |The Class Analyzer monad
newtype ClassAnalyzer a = ClassAnalyzer
  { runCA :: StateT CAState (Except String) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState CAState
             , MonadError String
             )

-- |Returns a class from the Class Analyzer State if passed typename matches
getClass :: TypeName -> ClassAnalyzer ClassDeclaration
getClass n =
  gets classes >>= \cs ->
    case lookup n cs of
      (Just c) -> return c
      Nothing -> throwError $ "ICE: Unknown class " ++ n

-- |Sets the main class in the Class Analyzer State
setMainClass :: ClassDeclaration -> ClassAnalyzer ()
setMainClass (GCDecl n _ ms) = when ("main" `elem` ms') (gets mainClass >>= set)
  where
    ms' = map (\(GMDecl n' _ _) -> n') ms
    set (Just m) =
      throwError $
      "Method main already defined in class " ++
      m ++ " but redefined in class " ++ n
    set Nothing = modify $ \s -> s {mainClass = Just n}

-- |Initializes the Class Analyzer State with empty lists and Nothing for the mainClass
initialState :: CAState
initialState =
  CAState {classes = [], classSize = [], classMethods = [], mainClass = Nothing}

-- |Adds classes to the state
setClasses :: ClassDeclaration -> ClassAnalyzer ()
setClasses c@(GCDecl n _ _) = modify $ \s -> s {classes = (n, c) : classes s}

-- |Returns the nearest 2^n as size for given class
getClassSize :: ClassDeclaration -> ClassAnalyzer Size
getClassSize (GCDecl _ fs _) =
  return $ 2 ^ (ceiling :: Double -> Integer) (logBase 2 (1 + genericLength fs))

-- |Set class size in state
setClassSize :: ClassDeclaration -> ClassAnalyzer ()
setClassSize c@(GCDecl n _ _) =
  getClassSize c >>= \sz -> modify $ \s -> s {classSize = (n, sz) : classSize s}

-- FIXME: Inheritance
-- |Returns class methods of a passed class
resolveClassMethods :: ClassDeclaration -> ClassAnalyzer [MethodDeclaration]
resolveClassMethods (GCDecl _ _ ms) = return ms

-- |Adds the methods of a class in the Class Analyzer State
setClassMethods :: ClassDeclaration -> ClassAnalyzer ()
setClassMethods c@(GCDecl n _ _) =
  resolveClassMethods c >>= \cm ->
    modify $ \s -> s {classMethods = (n, cm) : classMethods s}

-- |Class Analyzes a program
caProgram :: Program -> ClassAnalyzer Program
caProgram (GProg p) = do
  mapM_ setClasses p
  mapM_ setClassSize p
  mapM_ setClassMethods p
  mapM_ setMainClass p
  mc <- gets mainClass
  when (isNothing mc) (throwError "No main method defined")
  return $ GProg rootClasses
  where
    rootClasses = filter noBase p
    noBase GCDecl {} = True

-- |Performs Class Analysis on the program
classAnalysis :: Program -> Except String (Program, CAState)
classAnalysis p = runStateT (runCA $ caProgram p) initialState

-- |Pretty prints the Class Analyzer State
printCAState :: (Program, CAState) -> IO ()
printCAState (_, s) = pPrint s
