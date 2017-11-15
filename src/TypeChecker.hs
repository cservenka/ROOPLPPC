{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker (typeCheck) where

import Data.List

import Control.Monad.Reader
import Control.Monad.Except

import Debug.Trace (trace, traceShow)

import AST
import ClassAnalyzer
import ScopeAnalyzer

newtype TypeChecker a = TypeChecker { runTC :: ReaderT SAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadReader SAState, MonadError String)

getType :: SIdentifier -> TypeChecker DataType
getType i = asks symbolTable >>= \st ->
    case lookup i st of
        (Just (LocalVariable t _)) -> return t
        (Just (ClassField t _ _ _)) -> return t
        (Just (MethodParameter t _)) -> return t
        _ -> throwError $ "ICE: Invalid index " ++ show i

getParameterTypes :: SIdentifier -> TypeChecker [DataType]
getParameterTypes i = asks symbolTable >>= \st ->
    case lookup i st of
        (Just (Method ps _)) -> return ps
        _ -> throwError $ "ICE: Invalid index " ++ show i

expectType :: DataType -> DataType -> TypeChecker ()
expectType t1 t2 = unless (t1 == t2) (throwError $ "Expected type: " ++ show t1 ++ "\nActual type: " ++ show t2)

getClassMethods :: TypeName -> TypeChecker [MethodDeclaration]
getClassMethods n = asks (classMethods . caState) >>= \cm ->
    case lookup n cm of
        Nothing -> throwError $ "ICE: Unknown class " ++ n
        (Just ms) -> return ms

getDynamicParameterTypes :: TypeName -> MethodName -> TypeChecker [DataType]
getDynamicParameterTypes n m = getClassMethods n >>= \ms ->
    case find (\(GMDecl m' _ _) -> m == m') ms of
        Nothing -> throwError $ "Class " ++ n ++ " does not support method " ++ m
        (Just (GMDecl _ ps _)) -> return $ map (\(GDecl tp _) -> tp) ps

checkCall :: [SIdentifier] -> [DataType] -> TypeChecker ()
checkCall args ps = when (la /= lp) (throwError err) >> mapM getType args >>= \as -> mapM_ checkArgument (zip as ps)
    where la = length args
          lp = length ps
          err = "Passed " ++ show la ++ " argument(s) to method expecting " ++ show lp ++ " argument(s)"

checkArgument :: (DataType, DataType) -> TypeChecker ()
checkArgument (ObjectType ca, ObjectType cp) = asks (superClasses . caState) >>= \sc ->
    unless (ca == cp || maybe False (elem cp) (lookup ca sc)) (throwError $ "Class " ++ ca ++ " not a subtype of class " ++ cp)
checkArgument (ta, tp) = expectType tp ta

tcExpression :: SExpression -> TypeChecker DataType
tcExpression (Constant _) = pure IntegerType
tcExpression (Variable n) = getType n
tcExpression Nil = pure NilType
tcExpression (Binary binop e1 e2)
    | binop == Eq || binop == Neq =
        do t1 <- tcExpression e1
           t2 <- tcExpression e2
           expectType t1 t2
           pure IntegerType
    | otherwise =
        do t1 <- tcExpression e1
           t2 <- tcExpression e2
           expectType t1 IntegerType
           expectType t2 IntegerType
           pure IntegerType

tcStatement :: SStatement -> TypeChecker ()
tcStatement s =
    case s of
        (Assign n _ e) ->
            getType n
            >>= expectType IntegerType
            >> tcExpression e
            >>= expectType IntegerType

        (Swap n1 n2) ->
            do t1 <- getType n1
               t2 <- getType n2
               expectType t1 t2

        (Conditional e1 s1 s2 e2) ->
            tcExpression e1
            >>= expectType IntegerType
            >> mapM_ tcStatement s1
            >> mapM_ tcStatement s2
            >> tcExpression e2
            >>= expectType IntegerType

        (Loop e1 s1 s2 e2) ->
            tcExpression e1
            >>= expectType IntegerType
            >> mapM_ tcStatement s1
            >> mapM_ tcStatement s2
            >> tcExpression e2
            >>= expectType IntegerType

        (ObjectBlock _ _ stmt) ->
            mapM_ tcStatement stmt

        (LocalBlock tp n e1 stmt e2) ->
            getType n
            >>= case tp of
                  "int" -> expectType IntegerType
                  _     -> expectType (ObjectType tp)
            >> tcExpression e1
            >>= case tp of
                "int" -> expectType IntegerType
                _ -> expectType NilType
            >> mapM_ tcStatement stmt
            >> tcExpression e2
            >>= case tp of
                "int" -> expectType IntegerType
                _ -> expectType NilType

        (LocalCall m args) ->
            getParameterTypes m
            >>= checkCall args

        (LocalUncall m args) ->
            getParameterTypes m
            >>= checkCall args

        (ObjectCall o m args) ->
            do t <- getType o
               case t of
                   (ObjectType tn) -> getDynamicParameterTypes tn m >>= checkCall args
                   _ -> throwError $ "Non-object type " ++ show t ++ " does not support method invocation"

        (ObjectUncall o m args) ->
            do t <- getType o
               case t of
                   (ObjectType tn) -> getDynamicParameterTypes tn m >>= checkCall args
                   _ -> throwError $ "Non-object type " ++ show t ++ " does not support method invocation"

        Skip -> pure ()

        (ObjectConstruction _ _) -> pure ()

        (ObjectDestruction tp n) ->
            do t <- getType n
               when (t /= ObjectType tp) (throwError $ "Passed type " ++ show tp ++ " does not match the argument types: " ++ show t)
               case t of 
                (ObjectType _) -> expectType t (ObjectType tp)
                _ -> throwError $ "Expected type: " ++ show (ObjectType tp) ++ " Actual type: " ++ show t

        CopyReference _ n _ ->
            do t <- getType n
               case t of
                (ObjectType _) -> pure ()
                _ -> throwError $ "Non-object type " ++ show t ++ " does not support reference copying" 

        UnCopyReference tp n m ->
            do t1 <- getType n
               t2 <- getType m
               when (t1 /= ObjectType tp && t2 /= CopyType tp) (throwError $ "Passed type " ++ show tp ++ " does not match the arguments types: " ++ show t1 ++ ", " ++ show t2)
               case (t1, t2) of
                (ObjectType _, CopyType _) -> expectType t1 t2 
                _ -> throwError $ "Expected variables with types (" ++ show (ObjectType tp) ++ ", " ++ show (CopyType tp) ++ "). Actual Types given: (" ++ show t1 ++ ", " ++ show t2 ++ ")"
               
getMethodName :: SIdentifier -> TypeChecker Identifier
getMethodName i = asks symbolTable >>= \st ->
    case lookup i st of
        (Just (Method _ n)) -> return n
        _ -> throwError $ "ICE: Invalid index " ++ show i

tcMethod :: (TypeName, SMethodDeclaration) -> TypeChecker ()
tcMethod (_, GMDecl _ [] body) = mapM_ tcStatement body
tcMethod (_, GMDecl i (_:_) body) = getMethodName i >>= \n ->
    when (n == "main") (throwError "Method main has invalid signature")
    >> mapM_ tcStatement body

tcProgram :: SProgram -> TypeChecker (SProgram, SAState)
tcProgram p = (,) p <$> (mapM_ tcMethod p >> ask)

typeCheck :: (SProgram, SAState) -> Except String (SProgram, SAState)
typeCheck (p, s) = runReaderT (runTC $ tcProgram p) s