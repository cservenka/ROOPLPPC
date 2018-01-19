module AST where

import Text.Show.Pretty

{-- AST Primitives --}
type TypeName = String

type MethodName = String

data DataType = IntegerType
              | ObjectType TypeName
              | CopyType TypeName
              | ObjectArrayType TypeName
              | IntegerArrayType
              | ArrayType
              | ArrayElementType
              | NilType
  deriving (Show)

-- Types
instance Eq DataType where
  IntegerType == IntegerType = True
  IntegerArrayType == IntegerArrayType = True
  NilType == NilType = True
  NilType == (ObjectType _) = True
  (ObjectType _) == NilType = True
  (ObjectType t1) == (ObjectType t2) = t1 == t2
  (CopyType t1) == (CopyType t2) = t1 == t2
  (ObjectArrayType t1) == (ObjectArrayType t2) = t1 == t2
  (CopyType t1) == (ObjectType t2) = t1 == t2
  (ObjectType t1) == (CopyType t2) = t1 == t2
  ArrayType == (ObjectArrayType _) = True
  (ObjectArrayType _) == ArrayType = True
  ArrayType == IntegerArrayType = True
  IntegerArrayType == ArrayType = True
  _ == _ = False

-- Binary Operators
data BinOp = Add
           | Sub
           | Xor
           | Mul
           | Div
           | Mod
           | BitAnd
           | BitOr
           | And
           | Or
           | Lt
           | Gt
           | Eq
           | Neq
           | Lte
           | Gte 
  deriving (Show, Eq, Enum)

data ModOp = ModAdd
           | ModSub
           | ModXor
  deriving (Show, Eq, Enum)

{-- Generic AST Definitions --}
--Expressions
data GExpr v = Constant Integer
             | Variable v
             | ArrayElement (v, GExpr v)
             | Nil
             | Binary BinOp (GExpr v) (GExpr v)
  deriving (Show, Eq)

--Statements
data GStmt m v = Assign v ModOp (GExpr v)
               | AssignArrElem (v, GExpr v) ModOp (GExpr v)
               | Swap (v, Maybe (GExpr v)) (v, Maybe (GExpr v))
               | Conditional (GExpr v) [GStmt m v] [GStmt m v] (GExpr v)
               | Loop (GExpr v) [GStmt m v] [GStmt m v] (GExpr v)
               | ObjectBlock TypeName v [GStmt m v]
               | LocalBlock DataType v (GExpr v) [GStmt m v] (GExpr v)
               | LocalCall m [(v, Maybe (GExpr v))]
               | LocalUncall m [(v, Maybe (GExpr v))]
               | ObjectCall (v, Maybe (GExpr v)) MethodName [(v, Maybe (GExpr v))]
               | ObjectUncall (v, Maybe (GExpr v)) MethodName [(v, Maybe (GExpr v))]
               | ObjectConstruction TypeName (v, Maybe (GExpr v))
               | ObjectDestruction TypeName (v, Maybe (GExpr v))
               | CopyReference DataType (v, Maybe (GExpr v)) (v, Maybe (GExpr v))
               | UnCopyReference DataType (v, Maybe (GExpr v)) (v, Maybe (GExpr v))
               | ArrayConstruction (TypeName, GExpr v) v 
               | ArrayDestruction (TypeName, GExpr v) v 
               | Skip
  deriving (Show, Eq)

--Field/Parameter declarations
data GDecl v = GDecl DataType v
  deriving (Show, Eq)

--Method: Name, parameters, body
data GMDecl m v = GMDecl m [GDecl v] [GStmt m v]
  deriving (Show, Eq)

--Class: Name, fields, methods
data GCDecl m v = GCDecl TypeName (Maybe TypeName) [GDecl v] [GMDecl m v]
  deriving (Show, Eq)

--Program
newtype GProg m v = GProg [GCDecl m v]
  deriving (Show, Eq)

{-- Specific AST Definitions --}
--Plain AST
type Identifier = String

type Expression = GExpr Identifier

type Statement = GStmt MethodName Identifier

type VariableDeclaration = GDecl Identifier

type MethodDeclaration = GMDecl MethodName Identifier

type ClassDeclaration = GCDecl MethodName Identifier

type Program = GProg MethodName Identifier

--Scoped AST
type SIdentifier = Integer

type SExpression = GExpr SIdentifier

type SStatement = GStmt SIdentifier SIdentifier

type SVariableDeclaration = GDecl SIdentifier

type SMethodDeclaration = GMDecl SIdentifier SIdentifier

type SProgram = [(TypeName, GMDecl SIdentifier SIdentifier)]

{-- Other Definitions --}
type Offset = Integer

data Symbol = LocalVariable DataType Identifier
            | ClassField DataType Identifier TypeName Offset
            | MethodParameter DataType Identifier
            | Method [DataType] MethodName
  deriving (Show, Eq)

type SymbolTable = [(SIdentifier, Symbol)]

type Scope = [(Identifier, SIdentifier)]

printAST :: (Show t) => t -> String
printAST = ppShow
