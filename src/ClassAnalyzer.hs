{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module ClassAnalyzer (classAnalysis, printCAState, CAState(..)) where

import Data.Maybe
import Data.List

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Text.Show.Pretty

import AST

type Size = Integer

data CAState =
    CAState {
        classes :: [(TypeName, ClassDeclaration)],
        classSize :: [(TypeName, Size)],
        classMethods :: [(TypeName, [MethodDeclaration])],
        mainClass :: Maybe TypeName
    } deriving (Show, Eq)

newtype ClassAnalyzer a = ClassAnalyzer { runCA :: StateT CAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState CAState, MonadError String)

getClass :: TypeName -> ClassAnalyzer ClassDeclaration
getClass n = gets classes >>= \cs ->
    case lookup n cs of
        (Just c) -> return c
        Nothing -> throwError $ "ICE: Unknown class " ++ n

setMainClass :: ClassDeclaration -> ClassAnalyzer ()
setMainClass (GCDecl n _ ms) = when ("main" `elem` ms') (gets mainClass >>= set)
    where ms' = map (\(GMDecl n' _ _) -> n') ms
          set (Just m) = throwError $ "Method main already defined in class " ++ m ++ " but redefined in class " ++ n
          set Nothing = modify $ \s -> s { mainClass = Just n }

initialState :: CAState
initialState =
    CAState {
        classes = [],
        classSize = [],
        classMethods = [],
        mainClass = Nothing }

setClasses :: ClassDeclaration -> ClassAnalyzer ()
setClasses c@(GCDecl n _ _) = modify $ \s -> s { classes = (n, c) : classes s }

getClassSize :: ClassDeclaration -> ClassAnalyzer Size
getClassSize (GCDecl _ fs _) = return $ 2 ^ ceiling(logBase 2 (1 + genericLength fs))

setClassSize :: ClassDeclaration -> ClassAnalyzer ()
setClassSize c@(GCDecl n _ _) = getClassSize c >>= \sz ->
    modify $ \s -> s { classSize = (n, sz) : classSize s }

-- TODO: Inheritance
resolveClassMethods :: ClassDeclaration -> ClassAnalyzer [MethodDeclaration]
resolveClassMethods (GCDecl _ _ ms) = return ms

setClassMethods :: ClassDeclaration -> ClassAnalyzer ()
setClassMethods c@(GCDecl n _ _) = resolveClassMethods c >>= \cm ->
    modify $ \s -> s { classMethods = (n, cm) : classMethods s }

caProgram :: Program -> ClassAnalyzer Program
caProgram (GProg p) =
    do mapM_ setClasses p
       mapM_ setClassSize p
       mapM_ setClassMethods p
       mapM_ setMainClass p
       mc <- gets mainClass
       when (isNothing mc) (throwError "No main method defined")
       return $ GProg rootClasses
    where rootClasses = filter noBase p
          noBase GCDecl{} = True

classAnalysis :: Program -> Except String (Program, CAState)
classAnalysis p = runStateT (runCA $ caProgram p) initialState

printCAState :: (Program, CAState) -> String
printCAState (_, s) = show s