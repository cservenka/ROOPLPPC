{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module ScopeAnalyzer
  ( scopeAnalysis
  , printSAState
  , SAState(..)
  ) where

import Data.Maybe
import Data.List

import Control.Monad.State
import Control.Monad.Except

import Text.Pretty.Simple (pPrint)

import AST
import ClassAnalyzer

data SAState =
    SAState {
        symbolIndex :: SIdentifier,
        symbolTable :: SymbolTable,
        scopeStack :: [Scope],
        virtualTables :: [(TypeName, [SIdentifier])],
        caState :: CAState,
        mainMethod :: SIdentifier
    } deriving (Show, Eq)

newtype ScopeAnalyzer a = ScopeAnalyzer { runSA :: StateT SAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState SAState, MonadError String)

initialState :: CAState -> SAState
initialState s = SAState { symbolIndex = 0, symbolTable = [], scopeStack = [], virtualTables = [], caState = s, mainMethod = 0 }

-- Add an empty scope to the scope stack
enterScope :: ScopeAnalyzer ()
enterScope = modify $ \s -> s { scopeStack = [] : scopeStack s }

-- Leaves the current scope by removing it from the scope stack
leaveScope :: ScopeAnalyzer ()
leaveScope = modify $ \s -> s { scopeStack = drop 1 $ scopeStack s }

-- Returns the top scope at the scope stack
topScope :: ScopeAnalyzer Scope
topScope = gets scopeStack >>= \ss ->
    case ss of
        (s:_) -> return s
        [] -> throwError "ICE: Empty scope stack"

-- Add a symbol to the current scope
addToScope :: (Identifier, SIdentifier) -> ScopeAnalyzer ()
addToScope b =
    do ts <- topScope
       modify $ \s -> s { scopeStack = (b : ts) : drop 1 (scopeStack s) }

-- Remove a symbol from the current scope
removeFromScope :: (Identifier, SIdentifier) -> ScopeAnalyzer ()
removeFromScope b =
    do ts <- topScope
       modify $ \s -> s { scopeStack = filter (not . (==) b) ts : drop 1 (scopeStack s) }

-- Inserts an identifier, symbol pair into the symbol table and current scope
saInsert :: Symbol -> Identifier -> ScopeAnalyzer SIdentifier
saInsert sym n =
    do ts <- topScope
       when (isJust $ lookup n ts) (throwError $ "Redeclaration of symbol: " ++ n)
       i <- gets symbolIndex
       modify $ \s -> s { symbolTable = (i, sym) : symbolTable s, symbolIndex = 1 + i }
       addToScope (n, i)
       return i

-- Removed an identifier, symbol pair from the symbol table and current scope
saRemove :: Symbol -> Identifier -> ScopeAnalyzer SIdentifier
saRemove sym n =
    do ts <- topScope
       when (isNothing $ lookup n ts) (throwError $ "Removal of unsued symbol: " ++ n)
       i <- gets symbolIndex
       modify $ \s -> s { symbolTable = filter (not . (==) (i, sym)) (symbolTable s), symbolIndex = i - 1 }
       removeFromScope (n, i)
       return i

-- Looks up an identifier in the current scope
saLookup :: Identifier -> ScopeAnalyzer SIdentifier
saLookup n = gets scopeStack >>= \ss ->
    case listToMaybe $ mapMaybe (lookup n) ss of
        Nothing -> throwError $ "Undeclared symbol: " ++ n
        Just i -> return i

-- Analyses Expressions
saExpression :: Expression -> ScopeAnalyzer SExpression
saExpression (Constant v) = pure $ Constant v
saExpression (Variable n) = Variable <$> saLookup n
saExpression Nil = pure Nil
saExpression (Binary binop e1 e2) =
    Binary binop
    <$> saExpression e1
    <*> saExpression e2

-- Analyses Statements
saStatement :: Statement -> ScopeAnalyzer SStatement
saStatement s =
    case s of
        (Assign n modop e) ->
            when (elem n $ var e) (throwError "Irreversible variable assignment")
            >> Assign
            <$> saLookup n
            <*> pure modop
            <*> saExpression e

        (ObjectCall o m args) ->
            when (args /= nub args || o `elem` args) (throwError $ "Irreversible invocation of method " ++ m)
            >> ObjectCall
            <$> saLookup o
            <*> pure m
            <*> mapM saLookup args

        (ObjectUncall o m args) ->
            when (args /= nub args || o `elem` args) (throwError $ "Irreversible invocation of method " ++ m)
            >> ObjectUncall
            <$> saLookup o
            <*> pure m
            <*> mapM saLookup args

        (ObjectConstruction tp n) ->
            do n' <- saInsert (LocalVariable (ObjectType tp) n) n
               return $ ObjectConstruction tp n'

        (ObjectDestruction tp n) ->
            do n' <- saRemove (LocalVariable (ObjectType tp) n) n
               return $ ObjectDestruction tp n'

    where var (Variable n) = [n]
          var (Binary _ e1 e2) = var e1 ++ var e2
          var _ = []

          isCF ClassField{} = True
          isCF _ = False

          rlookup = flip lookup

          localCall :: MethodName -> [Identifier] -> ScopeAnalyzer [SIdentifier]
          localCall m args =
            do when (args /= nub args) (throwError $ "Irreversible invocation of method " ++ m)
               args' <- mapM saLookup args
               st <- gets symbolTable
               when (any isCF $ mapMaybe (rlookup st) args') (throwError $ "Irreversible invocation of method " ++ m)
               return args'

setMainMethod :: SIdentifier -> ScopeAnalyzer ()
setMainMethod i = modify $ \s -> s { mainMethod = i }

-- Analyses Methods
saMethod :: (TypeName, MethodDeclaration) -> ScopeAnalyzer (TypeName, SMethodDeclaration)
saMethod (t, GMDecl m ps body) =
    do m' <- saLookup m
       when (m == "main") (setMainMethod m')
       enterScope
       ps' <- mapM insertMethodParameter ps
       body' <- mapM saStatement body
       leaveScope
       return (t, GMDecl m' ps' body')
    where insertMethodParameter (GDecl tp n) = GDecl tp <$> saInsert (MethodParameter tp n) n

getMethodName :: SIdentifier -> ScopeAnalyzer (SIdentifier, MethodName)
getMethodName i = gets symbolTable >>= \st ->
    case lookup i st of
        (Just (Method _ m)) -> return (i, m)
        _ -> throwError $ "ICE: Invalid method index " ++ show i

prefixVtable :: [(SIdentifier, MethodName)] -> (SIdentifier, MethodName) -> [(SIdentifier, MethodName)]
prefixVtable [] m' = [m']
prefixVtable (m:ms) m' = if comp m m' then m':ms else m : prefixVtable ms m'
    where comp (_, n) (_, n') = n == n'

-- TODO: Inheritance
saClass :: Offset -> [SIdentifier] -> ClassDeclaration -> ScopeAnalyzer [(TypeName, SMethodDeclaration)]
saClass offset pids (GCDecl c fs ms) =
    do enterScope
       mapM_ insertClassField $ zip [offset..] fs
       m1 <- mapM getMethodName pids
       m2 <- mapM insertMethod ms
       let m3 = map fst $ foldl prefixVtable m1 m2
           offset' = genericLength fs + offset
       modify $ \s -> s { virtualTables = (c, m3) : virtualTables s }
--        sc <- getSubClasses c
--        ms' <- concat <$> mapM (saClass offset' m3) sc
       ms'' <- mapM saMethod $ zip (repeat c) ms
       leaveScope
--        return $ ms' ++ ms''
       return ms''
    where insertClassField (o, GDecl tp n) = saInsert (ClassField tp n c o) n
          insertMethod (GMDecl n ps _) = saInsert (Method (map getType ps) n) n >>= getMethodName
          getType (GDecl tp _) = tp

-- Analyses Programs
saProgram :: Program -> ScopeAnalyzer SProgram
saProgram (GProg cs) = concat <$> mapM (saClass 1 []) cs

scopeAnalysis :: (Program, CAState) -> Except String (SProgram, SAState)
scopeAnalysis (p, s) = runStateT (runSA $ saProgram p) $ initialState s

printSAState :: (SProgram, SAState) -> IO ()
printSAState (_, s) = pPrint s
