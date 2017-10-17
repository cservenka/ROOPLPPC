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

-- | The Class Analyzer State consists of a list of classes, sizes, methods
-- | and a main class
data CAState =
  CAState {
      classes :: [(TypeName, ClassDeclaration)],
      subClasses :: [(TypeName, [TypeName])],
      superClasses :: [(TypeName, [TypeName])],
      classSize :: [(TypeName, Size)],
      classMethods :: [(TypeName, [MethodDeclaration])],
      mainClass :: Maybe TypeName
  } deriving (Show, Eq)

-- | The Class Analyzer monad
newtype ClassAnalyzer a = ClassAnalyzer { runCA :: StateT CAState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState CAState, MonadError String)

-- | Initializes the Class Analyzer State with empty lists and Nothing for the mainClass
initialState :: CAState
initialState =
  CAState {
      classes = [],
      subClasses = [],
      superClasses = [],
      classSize = [],
      classMethods = [],
      mainClass = Nothing 
  }             

-- | Returns a class from the Class Analyzer State if passed typename matches
getClass :: TypeName -> ClassAnalyzer ClassDeclaration
getClass n =
  gets classes >>= \cs ->
    case lookup n cs of
      (Just c) -> return c
      Nothing -> throwError $ "ICE: Unknown class " ++ n

-- | Returns the base class inherited from
getBaseClass :: TypeName -> ClassAnalyzer (Maybe TypeName)
getBaseClass n = getClass n >>= getBase
    where getBase (GCDecl _ b _ _) = return b

-- | Throws error if class is defined multiple times    
checkDuplicateClasses :: ClassDeclaration -> ClassAnalyzer ()
checkDuplicateClasses (GCDecl n _ _ _) = gets classes >>= \cs ->
    when (count cs > 1) (throwError $ "Multiple definitions of class " ++ n)
    where count = length . filter ((== n) . fst)

-- | Ensures legal inheritance 
checkBaseClass :: ClassDeclaration -> ClassAnalyzer ()
checkBaseClass (GCDecl _ Nothing _ _) = return ()
checkBaseClass (GCDecl n (Just b) _ _) =
    do when (n == b) (throwError $ "Class " ++ n ++ " cannot inherit from itself")
       cs <- gets classes
       when (isNothing $ lookup b cs) (throwError $ "Class " ++ n ++ " cannot inherit from unknown class " ++ b)

-- | Checks duplicated field declarations       
checkDuplicateFields :: ClassDeclaration -> ClassAnalyzer ()
checkDuplicateFields (GCDecl n _ fs _) = mapM_ checkField fs
    where count v = length . filter (\(GDecl _ v') -> v' == v) $ fs
          checkField (GDecl _ v) = when (count v > 1) (throwError $ "Multiple declarations of field " ++ v ++ " in class " ++ n)

-- | Checks duplicated method declaration in classes           
checkDuplicateMethods :: ClassDeclaration -> ClassAnalyzer ()
checkDuplicateMethods (GCDecl n _ _ ms) = mapM_ checkMethod ms'
    where ms' = map (\(GMDecl n' _ _) -> n') ms
          count m = length . filter (== m) $ ms'
          checkMethod m = when (count m > 1) (throwError $ "Multiple definitions of method " ++ m ++ " in class " ++ n)

-- | Checks cyclic inheritance           
checkCyclicInheritance :: ClassDeclaration -> ClassAnalyzer ()
checkCyclicInheritance (GCDecl _ Nothing _ _) = return ()
checkCyclicInheritance (GCDecl n b _ _) = checkInheritance b [n]
    where checkInheritance Nothing _ = return ()
          checkInheritance (Just b') visited =
              do when (b' `elem` visited) (throwError $ "Cyclic inheritance involving class " ++ n)
                 next <- getBaseClass b'
                 checkInheritance next (b' : visited)      

-- | Sets the main class in the Class Analyzer State
setMainClass :: ClassDeclaration -> ClassAnalyzer ()
setMainClass (GCDecl n _ _ ms) = when ("main" `elem` ms') (gets mainClass >>= set)
  where
    ms' = map (\(GMDecl n' _ _) -> n') ms
    set (Just m) =
      throwError $
      "Method main already defined in class " ++
      m ++ " but redefined in class " ++ n
    set Nothing = modify $ \s -> s {mainClass = Just n}

-- | Adds classes to the state
setClasses :: ClassDeclaration -> ClassAnalyzer ()
setClasses c@(GCDecl n _ _ _) = modify $ \s -> s {classes = (n, c) : classes s}

-- | Add subclasses to the state
setSubClasses :: ClassDeclaration -> ClassAnalyzer ()
setSubClasses (GCDecl n b _ _) = modify (\s -> s { subClasses = (n, []) : subClasses s }) >> addSubClass n b

-- | Adds a subclass to the list of subclasses 
addSubClass :: TypeName -> Maybe TypeName -> ClassAnalyzer ()
addSubClass _ Nothing = return ()
addSubClass n (Just b) = gets subClasses >>= \sc ->
    case lookup b sc of
        Nothing -> modify $ \s -> s { subClasses = (b, [n]) : sc }
        (Just sc') -> modify $ \s -> s { subClasses = (b, n : sc') : delete (b, sc') sc }

-- | Sets super classes in the state        
setSuperClasses :: ClassDeclaration -> ClassAnalyzer ()
setSuperClasses (GCDecl n _ _ _) = gets subClasses >>= \sc ->
    modify $ \s -> s { superClasses = (n, map fst $ filter (\(_, sub) -> n `elem` sub) sc) : superClasses s }

-- | Returns the nearest 2^n as size for given class
getClassSize :: ClassDeclaration -> ClassAnalyzer Size
getClassSize (GCDecl _ Nothing fs _) = 
    return $ 2 ^ (ceiling :: Double -> Integer) (logBase 2 (2 + genericLength fs))
getClassSize (GCDecl _ (Just b) fs _) = 
    getClass b >>= getClassSize >>= \sz -> 
        return $ 2 ^ (ceiling :: Double -> Integer) (logBase 2 (fromIntegral $ sz + genericLength fs))

-- | Set class size in state
setClassSize :: ClassDeclaration -> ClassAnalyzer ()
setClassSize c@(GCDecl n _ _ _) =
  getClassSize c >>= \sz -> modify $ \s -> s {classSize = (n, sz) : classSize s}

-- | Returns class methods of a passed class
resolveClassMethods :: ClassDeclaration -> ClassAnalyzer [MethodDeclaration]
resolveClassMethods (GCDecl _ Nothing _ ms) = return ms
resolveClassMethods (GCDecl n (Just b) _ ms) = getClass b >>= resolveClassMethods >>= combine
    where checkSignature (GMDecl m ps _, GMDecl m' ps' _) = when (m == m' && ps /= ps') (throwError $ "Method " ++ m ++ " in class " ++ n ++ " has invalid method signature")
          compareName (GMDecl m _ _) (GMDecl m' _ _) = m == m'
          combine ms' = mapM_ checkSignature ((,) <$> ms <*> ms') >> return (unionBy compareName ms ms')

-- | Adds the methods of a class in the Class Analyzer State
setClassMethods :: ClassDeclaration -> ClassAnalyzer ()
setClassMethods c@(GCDecl n _ _ _) = resolveClassMethods c >>= \cm ->
    modify $ \s -> s { classMethods = (n, cm) : classMethods s }

-- | Class Analyzes a program
caProgram :: Program -> ClassAnalyzer Program
caProgram (GProg p) = do
  mapM_ setClasses p
  mapM_ setSubClasses p
  mapM_ setSuperClasses p
  mapM_ setClassSize p
  mapM_ setClassMethods p
  mapM_ checkDuplicateClasses p
  mapM_ checkDuplicateFields p
  mapM_ checkDuplicateMethods p
  mapM_ checkBaseClass p
  mapM_ checkCyclicInheritance p
  mapM_ setMainClass p
  mc <- gets mainClass
  when (isNothing mc) (throwError "No main method defined")
  return $ GProg rootClasses
  where
    rootClasses = filter noBase p
    noBase (GCDecl _ Nothing _ _) = True
    noBase _ = False
 
-- | Performs Class Analysis on the program
classAnalysis :: Program -> Except String (Program, CAState)
classAnalysis p = runStateT (runCA $ caProgram p) initialState

-- | Pretty prints the Class Analyzer State
printCAState :: (Program, CAState) -> IO ()
printCAState (_, s) = pPrint s
