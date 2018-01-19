{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TypeChecker (typeCheck) where

import Data.List
import Data.Maybe

import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception

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

getArrayType :: DataType -> DataType
getArrayType tp = case tp of
                    IntegerArrayType -> IntegerType
                    ObjectArrayType t -> ObjectType t       

checkCall :: [(SIdentifier, Maybe SExpression)] -> [DataType] -> TypeChecker ()
checkCall args ps = 
    when (la /= lp) (throwError err)
    >> mapM (mapM tcExpression . snd) args 
    >> mapM (getType . fst) args
    >>= \as -> mapM_ checkArgument (zip as ps)
    where la = length args
          lp = length ps
          err = "Passed " ++ show la ++ " argument(s) to method expecting " ++ show lp ++ " argument(s)"

checkArgument :: (DataType, DataType) -> TypeChecker ()
checkArgument (ObjectType ca, ObjectType cp) = asks (superClasses . caState) >>= \sc ->
    unless (ca == cp || maybe False (elem cp) (lookup ca sc)) (throwError $ "Class " ++ ca ++ " not a subtype of class " ++ cp)
checkArgument (ObjectType ca, ObjectArrayType cp) = asks (superClasses . caState) >>= \sc ->
    unless (ca == cp || maybe False (elem cp) (lookup ca sc)) (throwError $ "Class " ++ ca ++ " not a subtype of class " ++ cp)
checkArgument (ObjectArrayType ca, ObjectType cp) = asks (superClasses . caState) >>= \sc ->
    unless (ca == cp || maybe False (elem cp) (lookup ca sc)) (throwError $ "Class " ++ ca ++ " not a subtype of class " ++ cp)
checkArgument (IntegerArrayType, tp) = expectType (getArrayType IntegerArrayType) tp
checkArgument (ta, IntegerArrayType) = expectType (getArrayType IntegerArrayType) ta       
checkArgument (ta, tp) = expectType tp ta

tcExpression :: SExpression -> TypeChecker DataType
tcExpression (Constant _) = pure IntegerType
tcExpression (Variable n) = getType n
tcExpression Nil = pure NilType
tcExpression (ArrayElement (n, e)) =
    do t <- getType n
       expectType ArrayType t
       e' <- tcExpression e
       expectType IntegerType e'
       return $ getArrayType t
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

        (AssignArrElem (n, e1) _ e2) ->
            getType n
            >>= expectType IntegerArrayType
            >> tcExpression e1
            >>= expectType IntegerType
            >> tcExpression e2
            >>= expectType IntegerType

        (Swap (n1, e1) (n2, e2)) ->
            do t1 <- getType n1
               t2 <- getType n2 
               if isNothing e1 /= isNothing e2 
                 then catchError (checkArgument (t1, t2)) (\_ -> checkArgument (t2, t1))
                 else expectType (if isNothing e1 then t1 else getArrayType t1) (if isNothing e2 then t2 else getArrayType t2)

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

        (LocalBlock t n e1 stmt e2) ->
            getType n
            >> tcExpression e1
            >>= expectType (if t == IntegerType then IntegerType else NilType)
            >> mapM_ tcStatement stmt
            >> tcExpression e2
            >>= expectType (if t == IntegerType then IntegerType else NilType)

        (LocalCall m args) ->
            getParameterTypes m
            >>= checkCall args

        (LocalUncall m args) ->
            getParameterTypes m
            >>= checkCall args

        (ObjectCall (o, e) m args) ->
            do t <- getType o
               e' <- mapM tcExpression e
               case t of
                   (ObjectType tn) -> getDynamicParameterTypes tn m >>= checkCall args
                   (ObjectArrayType tn) -> 
                    case e' of
                        Nothing -> throwError $ "Non-object type " ++ show t ++ " does not support method invocation"
                        _ -> getDynamicParameterTypes tn m >>= checkCall args
                   _ -> throwError $ "Non-object type " ++ show t ++ " does not support method invocation"

        (ObjectUncall (o, e) m args) ->
            do t <- getType o
               e' <- mapM tcExpression e
               case t of
                   (ObjectType tn) -> getDynamicParameterTypes tn m >>= checkCall args
                   (ObjectArrayType tn) -> 
                    case e' of
                        Nothing -> throwError $ "Non-object type " ++ show t ++ " does not support method invocation"
                        _ -> getDynamicParameterTypes tn m >>= checkCall args
                   _ -> throwError $ "Non-object type " ++ show t ++ " does not support method invocation"

        Skip -> pure ()

        (ObjectConstruction tp (n, e)) ->
            do t <- getType n
               e' <- mapM tcExpression e
               case e' of 
                 Nothing -> expectType t (ObjectType tp)
                 _       -> checkArgument (ObjectType tp, t)
        
        (ObjectDestruction tp (n, e)) ->
            do t <- getType n
               _ <- mapM tcExpression e
               case t of 
                (ObjectType _) -> expectType t (ObjectType tp)
                (ObjectArrayType _) -> checkArgument (ObjectType tp, t)
                _ -> throwError $ "Expected type: " ++ show (ObjectType tp) ++ " Actual type: " ++ show t
        
        -- Allow copying with a copy type        
        CopyReference _ (n, e1) (m, e2) ->
            do t1 <- getType n
               t2 <- getType m
               e1' <- mapM tcExpression e1
               e2' <- mapM tcExpression e2
               when (t1 == IntegerType || t2 == IntegerType) (throwError "Integer types does not support reference copying")
               if isNothing e1 /= isNothing e2 
                 then catchError (checkArgument (t1, t2)) (\_ -> checkArgument (t2, t1))
                 else expectType (if isNothing e1 then t1 else getArrayType t1) (if isNothing e2 then t2 else getArrayType t2)
        
        -- Allow uncopying with two identical copies
        UnCopyReference _ (n, e1) (m, e2) ->
            do t1 <- getType n
               t2 <- getType m
               e1' <- mapM tcExpression e1
               e2' <- mapM tcExpression e2
               when (t1 == IntegerType || t2 == IntegerType) (throwError "Integer types does not support reference copying")
               if isNothing e1 /= isNothing e2 
                 then catchError (checkArgument (t1, t2)) (\_ -> checkArgument (t2, t1))
                 else expectType (if isNothing e1 then t1 else getArrayType t1) (if isNothing e2 then t2 else getArrayType t2)

           
        (ArrayConstruction (tp, e) n) -> 
            do t <- getType n
               _ <- tcExpression e
               case tp of
                 "int" -> expectType t IntegerArrayType
                 _     -> expectType t (ObjectArrayType tp) 

        (ArrayDestruction (tp, e) n) -> 
            do t <- getType n
               _ <- tcExpression e
               case tp of
                 "int" -> expectType t IntegerArrayType
                 _     -> checkArgument (ObjectArrayType tp, t)         
               
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