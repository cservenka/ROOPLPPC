{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module ScopeAnalyzer
  ( scopeAnalysis
  , printSAState
  , SAState(..)
  ) where

import Data.Maybe
import Data.List
import Data.Typeable

import Control.Monad.State
import Control.Monad.Except

import Debug.Trace (trace, traceShow)

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

-- | Add an empty scope to the scope stack
enterScope :: ScopeAnalyzer ()
enterScope = modify $ \s -> s { scopeStack = [] : scopeStack s }

-- | Leaves the current scope by removing it from the scope stack
leaveScope :: ScopeAnalyzer ()
leaveScope = modify $ \s -> s { scopeStack = drop 1 $ scopeStack s }

-- | Returns the top scope at the scope stack
topScope :: ScopeAnalyzer Scope
topScope = gets scopeStack >>= \ss ->
    case ss of
        (s:_) -> return s
        [] -> throwError "ICE: Empty scope stack"

-- | Add a symbol to the current scope
addToScope :: (Identifier, SIdentifier) -> ScopeAnalyzer ()
addToScope b =
    do ts <- topScope
       modify $ \s -> s { scopeStack = (b : ts) : drop 1 (scopeStack s) }

-- | Inserts an identifier and symbol pair into the symbol table and current scope
saInsert :: Symbol -> Identifier -> ScopeAnalyzer SIdentifier
saInsert sym n =
    do ts <- topScope
       when (isJust $ lookup n ts) (throwError $ "Redeclaration of symbol: " ++ n)
       i <- gets symbolIndex
       modify $ \s -> s { symbolTable = (i, sym) : symbolTable s, symbolIndex = 1 + i }
       addToScope (n, i)
       return i     

-- | Looks up an identifier in the scope
saLookup :: Identifier -> ScopeAnalyzer SIdentifier
saLookup n = gets scopeStack >>= \ss ->
    case listToMaybe $ mapMaybe (lookup n) ss of
        Nothing -> throwError $ "Undeclared symbol: " ++ n
        Just i -> return i                                      

-- | Scope Analyses Expressions
saExpression :: Expression -> ScopeAnalyzer SExpression
saExpression (Constant v) = pure $ Constant v
saExpression (Variable n) = Variable <$> saLookup n
saExpression Nil = pure Nil
saExpression (ArrayElement (n, e)) = 
    do n' <- saLookup n
       e' <- saExpression e
       return $ ArrayElement (n', e')
saExpression (Binary binop e1 e2) =
    Binary binop
    <$> saExpression e1
    <*> saExpression e2

-- | Scope Analyses Statements
saStatement :: Statement -> ScopeAnalyzer SStatement
saStatement s =
    case s of
        (Assign n modop e) ->
            when (elem n $ var e) (throwError "Irreversible variable assignment")
            >> Assign
            <$> saLookup n
            <*> pure modop
            <*> saExpression e

        (AssignArrElem (n, e1) modop e2) ->
            when (elem (n, e1) $ varArr e2) (throwError "Irreversible variable assignment")
            >> AssignArrElem
            <$> saArrayCell n e1
            <*> pure modop
            <*> saExpression e2
        
        (Swap (n1, e1) (n2, e2)) ->
            Swap
            <$> maybeArrayCell n1 e1
            <*> maybeArrayCell n2 e2
    
        (Conditional e1 s1 s2 e2) ->
            Conditional
            <$> saExpression e1
            <*> mapM saStatement s1
            <*> mapM saStatement s2
            <*> saExpression e2
    
        (Loop e1 s1 s2 e2) ->
            Loop
            <$> saExpression e1
            <*> mapM saStatement s1
            <*> mapM saStatement s2
            <*> saExpression e2    
 
        (LocalBlock t n e1 stmt e2) ->
            do e1' <- saExpression e1
               enterScope
               n' <- saInsert (LocalVariable t n) n
               stmt' <- mapM saStatement stmt
               leaveScope
               e2' <- saExpression e2
               return $ LocalBlock t n' e1' stmt' e2'
    
        (LocalCall m args) ->
            LocalCall
            <$> saLookup m
            <*> localCall m args
    
        (LocalUncall m args) ->
            LocalUncall
            <$> saLookup m
            <*> localCall m args
                        
        (ObjectCall (o, e) m args) ->
            do when (args /= nub args || (o, e) `elem` args) (throwError $ "Irreversible invocation of method " ++ m)
               >> ObjectCall
               <$> maybeArrayCell o e
               <*> pure m
               <*> saArgs args

        (ObjectUncall (o, e) m args) ->
            when (args /= nub args || (o, e) `elem` args) (throwError $ "Irreversible invocation of method " ++ m)
            >> ObjectUncall
            <$> maybeArrayCell o e
            <*> pure m
            <*> saArgs args

        (ObjectConstruction tp (n, e)) ->
            ObjectConstruction
            <$> pure tp
            <*> maybeArrayCell n e
        
        (ObjectDestruction tp (n, e)) ->
            ObjectDestruction
            <$> pure tp
            <*> maybeArrayCell n e

        (ObjectBlock tp n stmt) ->
            do enterScope
               n' <- saInsert (LocalVariable (ObjectType tp) n) n
               stmt' <- mapM saStatement stmt
               leaveScope
               return $ ObjectBlock tp n' stmt'

        Skip -> pure Skip
        
        (CopyReference tp (n, e1) (m, e2)) ->
            CopyReference
            <$> pure tp
            <*> maybeArrayCell n e1
            <*> maybeArrayCell m e2
        
        (UnCopyReference tp (n, e1) (m, e2)) ->
            UnCopyReference
            <$> pure tp
            <*> maybeArrayCell n e1
            <*> maybeArrayCell m e2

        (ArrayConstruction (tp, e) n) ->
            do n' <- saLookup n
               e' <- saExpression e
               return $ ArrayConstruction (tp, e') n'
        
        (ArrayDestruction (tp, e) n) -> 
            do n' <- saLookup n
               e' <- saExpression e
               return $ ArrayDestruction (tp, e') n'       
                
    where var (Variable n) = [n]
          var (Binary _ e1 e2) = var e1 ++ var e2
          var _ = []

          varArr (ArrayElement (n, e)) = [(n, e)]
          varArr _ = []

          isCF ClassField{} = True
          isCF _ = False

          rlookup = flip lookup

          localCall :: MethodName -> [(Identifier, Maybe Expression)] -> ScopeAnalyzer [(SIdentifier, Maybe SExpression)]
          localCall m args =
            do when (args /= nub args) (throwError $ "Irreversible invocation of method " ++ m)
               args' <- saArgs args
               st <- gets symbolTable
               when (any isCF $ mapMaybe (rlookup st . fst) args') (throwError $ "Irreversible invocation of method " ++ m)
               return args'

          saArgs :: [(Identifier, Maybe Expression)] -> ScopeAnalyzer [(SIdentifier, Maybe SExpression)]
          saArgs args = 
            do (ns, es) <- pure $ unzip args
               ns' <- mapM saLookup ns
               es' <- mapM (mapM saExpression) es
               return $ zip ns' es'

          maybeArrayCell :: Identifier -> Maybe Expression -> ScopeAnalyzer (SIdentifier, Maybe SExpression)
          maybeArrayCell n e =
            do n' <- saLookup n
               e' <- mapM saExpression e
               return (n', e')
               
          saArrayCell :: Identifier -> Expression -> ScopeAnalyzer (SIdentifier, SExpression)
          saArrayCell n e =
            do n' <- saLookup n
               e' <- saExpression e
               return (n', e')     

-- | Set the main method in the Scope Analyzer state
setMainMethod :: SIdentifier -> ScopeAnalyzer ()
setMainMethod i = modify $ \s -> s { mainMethod = i }

-- | Scope Analyses Methods
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

-- | Returns subclasses for a given type name    
getSubClasses :: TypeName -> ScopeAnalyzer [ClassDeclaration]
getSubClasses n =
    do cs <- gets $ classes . caState
       sc <- gets $ subClasses . caState
       case lookup n sc of
           Nothing -> throwError $ "ICE: Unknown class " ++ n
           (Just sc') -> return $ mapMaybe (rlookup cs) sc'
    where rlookup = flip lookup    

-- | Returns method name at given index
getMethodName :: SIdentifier -> ScopeAnalyzer (SIdentifier, MethodName)
getMethodName i = gets symbolTable >>= \st ->
    case lookup i st of
        (Just (Method _ m)) -> return (i, m)
        _ -> throwError $ "ICE: Invalid method index " ++ show i

-- | Prefixes the virtual table
prefixVtable :: [(SIdentifier, MethodName)] -> (SIdentifier, MethodName) -> [(SIdentifier, MethodName)]
prefixVtable [] m' = [m']
prefixVtable (m:ms) m' = if comp m m' then m':ms else m : prefixVtable ms m'
    where comp (_, n) (_, n') = n == n'

-- | Scope Analyses a passed class
-- TODO: Fix offset for MAIN class
saClass :: Offset -> [SIdentifier] -> ClassDeclaration -> ScopeAnalyzer [(TypeName, SMethodDeclaration)]
saClass offset pids (GCDecl c _ fs ms) =
    do enterScope
       mapM_ insertClassField $ zip [offset..] fs
       m1 <- mapM getMethodName pids
       m2 <- mapM insertMethod ms
       let m3 = map fst $ foldl prefixVtable m1 m2
           offset' = genericLength fs + offset
       modify $ \s -> s { virtualTables = (c, m3) : virtualTables s }
       sc <- getSubClasses c
       ms' <- concat <$> mapM (saClass offset' m3) sc
       ms'' <- mapM saMethod $ zip (repeat c) ms
       leaveScope
       return $ ms' ++ ms''
    where insertClassField (o, GDecl tp n) = saInsert (ClassField tp n c o) n
          insertMethod (GMDecl n ps _) = saInsert (Method (map getType ps) n) n >>= getMethodName
          getType (GDecl tp _) = tp

-- | Analyses Programs
saProgram :: Program -> ScopeAnalyzer SProgram
saProgram (GProg cs) = concat <$> mapM (saClass 2 []) cs

-- | Performs scope analysis on the entire program 
scopeAnalysis :: (Program, CAState) -> Except String (SProgram, SAState)
scopeAnalysis (p, s) = runStateT (runSA $ saProgram p) $ initialState s

-- | Pretty prints the current Scope Analysis State Monad
printSAState :: (Show a, MonadIO m) => (t, a) -> m ()
printSAState (_, s) = pPrint s
