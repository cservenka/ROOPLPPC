{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module PISA where

import Data.List (intercalate)
import Control.Arrow

import AST (TypeName, MethodName)

type Label = String

data Register = Reg Integer
    deriving (Eq)

{-- Generic PISA Definitions --}

data GInstr i = ADD Register Register
              | ADDI Register i
              | ANDX Register Register Register
              | ANDIX Register Register i
              | NORX Register Register Register
              | NEG Register
              | ORX Register Register Register
              | ORIX Register Register i
              | RL Register i
              | RLV Register Register
              | RR Register i
              | RRV Register Register
              | SLLX Register Register i
              | SLLVX Register Register Register
              | SRAX Register Register i
              | SRAVX Register Register Register
              | SRLX Register Register i
              | SRLVX Register Register Register
              | SUB Register Register
              | XOR Register Register
              | XORI Register i
              | BEQ Register Register Label
              | BGEZ Register Label
              | BGTZ Register Label
              | BLEZ Register Label
              | BLTZ Register Label
              | BNE Register Register Label
              | BRA Label
              | EXCH Register Register
              | SWAPBR Register
              | RBRA Label
              | START
              | FINISH
              | DATA i
              | SUBI Register i --Pseudo
    deriving (Eq)

data GProg i = GProg [(Maybe Label, GInstr i)]

{-- Macro PISA Definitions --}

data Macro = Immediate Integer
           | AddressMacro Label
           | SizeMacro TypeName
           | OffsetMacro TypeName MethodName
           | ProgramSize
           | FreeListsSize
           | StackOffset
           | InitialMemoryBlockSize
    deriving (Show, Eq)

type MInstruction = GInstr Macro
type MProgram = GProg Macro

invertInstructions :: [(Maybe Label, MInstruction)] -> [(Maybe Label, MInstruction)]
invertInstructions = reverse . map (second invertInstruction . first (fmap (++ "_i")))
    where invertInstruction (ADD r1 r2) = SUB r1 r2
          invertInstruction (SUB r1 r2) = ADD r1 r2
          invertInstruction (ADDI r i) = SUBI r i
          invertInstruction (SUBI r i) = ADDI r i
          invertInstruction (RL r i) = RR r i
          invertInstruction (RLV r1 r2) = RRV r1 r2
          invertInstruction (RR r i) = RL r i
          invertInstruction (RRV r1 r2) = RLV r1 r2
          invertInstruction (BEQ r1 r2 l) = BEQ r1 r2 $ l ++ "_i"
          invertInstruction (BGEZ r l) = BGEZ r $ l ++ "_i"
          invertInstruction (BGTZ r l) = BGTZ r $ l ++ "_i"
          invertInstruction (BLEZ r l) = BLEZ r $ l ++ "_i"
          invertInstruction (BLTZ r l) = BLTZ r $ l ++ "_i"
          invertInstruction (BNE r1 r2 l) = BNE r1 r2 $ l ++ "_i"
          invertInstruction (BRA l) = BRA $ l ++ "_i"
          invertInstruction (RBRA l) = RBRA $ l ++ "_i"
          invertInstruction inst = inst

{-- Output PISA Definitions --}

type Instruction = GInstr Integer
type Program = GProg Integer

instance Show Register where
    show (Reg r) = "$" ++ show r

instance Show Instruction where
    show (ADD r1 r2) = unwords ["ADD   ", show r1, show r2]
    show (ADDI r i) = unwords ["ADDI  ", show r, show i]
    show (ANDX r1 r2 r3) = unwords ["ANDX  ", show r1, show r2, show r3]
    show (ANDIX r1 r2 i) = unwords ["ANDIX ", show r1, show r2, show i]
    show (NORX r1 r2 r3) = unwords ["NORX  ", show r1, show r2, show r3]
    show (NEG r) = unwords ["NEG   ", show r]
    show (ORX r1 r2 r3) = unwords ["ORX   ", show r1, show r2, show r3]
    show (ORIX r1 r2 i) = unwords ["ORIX  ", show r1, show r2, show i]
    show (RL r i) = unwords ["RL    ", show r, show i]
    show (RLV r1 r2) = unwords ["RLV   ", show r1, show r2]
    show (RR r i) = unwords ["RR    ", show r, show i]
    show (RRV r1 r2) = unwords ["RRV   ", show r1, show r2]
    show (SLLX r1 r2 i) = unwords ["SLLX  ", show r1, show r2, show i]
    show (SLLVX r1 r2 r3) = unwords ["SLLVX ", show r1, show r2, show r3]
    show (SRAX r1 r2 i) = unwords ["SRAX  ", show r1, show r2, show i]
    show (SRAVX r1 r2 r3) = unwords ["SRAVX ", show r1, show r2, show r3]
    show (SRLX r1 r2 i) = unwords ["SRLX  ", show r1, show r2, show i]
    show (SRLVX r1 r2 r3) = unwords ["SRLVX ", show r1, show r2, show r3]
    show (SUB r1 r2) = unwords ["SUB   ", show r1, show r2]
    show (XOR r1 r2) = unwords ["XOR   ", show r1, show r2]
    show (XORI r i) = unwords ["XORI  ", show r, show i]
    show (BEQ r1 r2 l) = unwords ["BEQ   ", show r1, show r2, l]
    show (BGEZ r l) = unwords ["BGEZ  ", show r, l]
    show (BGTZ r l) = unwords ["BGTZ  ", show r, l]
    show (BLEZ r l) = unwords ["BLEZ  ", show r, l]
    show (BLTZ r l) = unwords ["BLTZ  ", show r, l]
    show (BNE r1 r2 l) = unwords ["BNE   ", show r1, show r2, l]
    show (BRA l) = unwords ["BRA   ", l]
    show (EXCH r1 r2) = unwords ["EXCH  ", show r1, show r2]
    show (SWAPBR r) = unwords ["SWAPBR", show r]
    show (RBRA l) = unwords ["RBRA  ", l]
    show START = "START "
    show FINISH = "FINISH"
    show (DATA i) = unwords ["DATA  ", show i]
    show (SUBI r i) = unwords ["ADDI  ", show r, show $ -i] --Expand pseudo

showProgram :: Program -> String
showProgram (GProg p) = ";; pendulum pal file\n" ++ intercalate "\n" (map showLine p)
    where showLine (Nothing, i) = spaces 25 ++ show i
          showLine (Just l, i) = l ++ ":" ++ spaces (24 - length l) ++ show i
          spaces :: (Int -> String)
          spaces n = [1..n] >> " "

writeProgram :: Program -> IO ()
writeProgram p = writeFile "../test/Example.pal" $ showProgram p