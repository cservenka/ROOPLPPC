module AST where

import Text.Show.Pretty

{-- AST Primitives --}
type TypeName = String

type MethodName = String

data DataType
  = IntegerType
  | ObjectType TypeName
  | NilType
  deriving (Show)

-- Types
instance Eq DataType where
  IntegerType == IntegerType = True
  NilType == NilType = True
  NilType == (ObjectType _) = True
  (ObjectType _) == NilType = True
  (ObjectType t1) == (ObjectType t2) = t1 == t2
  _ == _ = False

-- Binary Operators
data BinOp =
  Add
  deriving (Show, Eq, Enum)

data ModOp =
  ModAdd
  deriving (Show, Eq, Enum)

{-- Generic AST Definitions --}
--Expressions
data GExpr v
  = Constant Integer
  | Variable v
  | Nil
  | Binary BinOp
           (GExpr v)
           (GExpr v)
  deriving (Show, Eq)

--Statements
data GStmt m v
  = Assign v
           ModOp
           (GExpr v)
  | ObjectCall v
               MethodName
               [v]
  | ObjectUncall v
                 MethodName
                 [v]
  | ObjectConstruction TypeName
                       v
  | ObjectDestruction TypeName
                      v
  deriving (Show, Eq)

--Field/Parameter declarations
data GDecl v =
  GDecl DataType
        v
  deriving (Show, Eq)

--Method: Name, parameters, body
data GMDecl m v =
  GMDecl m
         [GDecl v]
         [GStmt m v]
  deriving (Show, Eq)

--Class: Name, fields, methods
data GCDecl m v =
  GCDecl TypeName
         [GDecl v]
         [GMDecl m v]
  deriving (Show, Eq)

--Program
data GProg m v =
  GProg [GCDecl m v]
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

data Symbol
  = LocalVariable DataType
                  Identifier
  | ClassField DataType
               Identifier
               TypeName
               Offset
  | MethodParameter DataType
                    Identifier
  | Method [DataType]
           MethodName
  deriving (Show, Eq)

type SymbolTable = [(SIdentifier, Symbol)]

type Scope = [(Identifier, SIdentifier)]

printAST :: (Show t) => t -> String
printAST = ppShow
