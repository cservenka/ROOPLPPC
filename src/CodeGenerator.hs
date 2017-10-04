{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module CodeGenerator( 
    generatePISA,
    showPISAProgram
) where

import Data.List

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State

import Debug.Trace (trace, traceShow)

import Text.Pretty.Simple (pPrint)

import AST
import ClassAnalyzer
import PISA
import ScopeAnalyzer

{-# ANN module "HLint: ignore Reduce duplication" #-}

data CGState =
    CGState {
        labelIndex :: SIdentifier,
        registerIndex :: Integer,
        labelTable :: [(SIdentifier, Label)],
        registerStack :: [(SIdentifier, Register)],
        saState :: SAState
    } deriving (Show, Eq)

newtype CodeGenerator a = CodeGenerator { runCG :: StateT CGState (Except String) a }
    deriving (Functor, Applicative, Monad, MonadState CGState, MonadError String)


initialState :: SAState -> CGState
initialState s = CGState { labelIndex = 0, registerIndex = 6, labelTable = [], registerStack = [], saState = s }

-- | Register containing 0
registerZero :: Register
registerZero = Reg 0

-- | Register containing Stack pointer
registerSP :: Register
registerSP = Reg 1

-- | Register R0
registerRO :: Register
registerRO = Reg 2

-- | Register holding 'this'
registerThis :: Register
registerThis = Reg 3

-- | Register containing Free list pointers
registerFLPs :: Register
registerFLPs = Reg 4

-- | Register containing Heap pointer
registerHP :: Register
registerHP = Reg 5

pushRegister :: SIdentifier -> CodeGenerator Register
pushRegister i =
    do ri <- gets registerIndex
       modify $ \s -> s { registerIndex = 1 + ri, registerStack = (i, Reg ri) : registerStack s }
       return $ Reg ri

popRegister :: CodeGenerator ()
popRegister = modify $ \s -> s { registerIndex = (-1) + registerIndex s, registerStack = drop 1 $ registerStack s }

tempRegister :: CodeGenerator Register
tempRegister =
    do ri <- gets registerIndex
       modify $ \s -> s { registerIndex = 1 + ri }
       return $ Reg ri

popTempRegister :: CodeGenerator ()
popTempRegister = modify $ \s -> s { registerIndex = (-1) + registerIndex s }

lookupRegister :: SIdentifier -> CodeGenerator Register
lookupRegister i = gets registerStack >>= \rs ->
    case lookup i rs of
        Nothing -> throwError $ "ICE: No register reserved for index " ++ show i
        (Just r) -> traceShow rs return r

-- | Returns the method name of a valid method identifier
getMethodName :: SIdentifier -> CodeGenerator MethodName
getMethodName i = gets (symbolTable . saState) >>= \st ->
    case lookup i st of
        (Just (Method _ n)) -> return n
        _ -> throwError $ "ICE: Invalid method index " ++ show i

-- | Inserts a unique method label in the label table for a given method identifier
insertMethodLabel :: SIdentifier -> CodeGenerator ()
insertMethodLabel m =
    do n <- getMethodName m
       i <- gets labelIndex
       modify $ \s -> s { labelIndex = 1 + i, labelTable = (m, "l_" ++ n ++ "_" ++ show i) : labelTable s }

-- | Returns the Method label for a method identifier
getMethodLabel :: SIdentifier -> CodeGenerator Label
getMethodLabel m = gets labelTable >>= \lt ->
    case lookup m lt of
        (Just l) -> return l
        Nothing -> insertMethodLabel m >> getMethodLabel m

-- | Returns a unique label by appending the label index to a passed label type
getUniqueLabel :: Label -> CodeGenerator Label
getUniqueLabel l =
    do i <- gets labelIndex
       modify $ \s -> s { labelIndex = 1 + i }
       return $ l ++ "_" ++ show i

-- | Returns the address to the variable of a given identifier
loadVariableAddress :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
loadVariableAddress n = gets (symbolTable . saState) >>= \st ->
    case lookup n st of
        (Just (ClassField _ _ _ o)) -> tempRegister >>= \r -> return (r, [(Nothing, ADD r registerThis), (Nothing, ADDI r $ Immediate o)], popTempRegister)
        (Just (LocalVariable _ _)) -> lookupRegister n >>= \r -> return (r, [], return ())
        (Just (MethodParameter _ _)) -> lookupRegister n >>= \r -> return (r, [], return ())
        _ -> throwError $ "ICE: Invalid variable index " ++ show n

-- | Returns the value of a variable of given identifier
loadVariableValue :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
loadVariableValue n =
    do (ra, la, ua) <- loadVariableAddress n
       rv <- tempRegister
       return (rv, la ++ [(Nothing, EXCH rv ra)], popTempRegister >> ua)

-- | Returns pointer to free list at given index      
loadFreeListAddress :: Register -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())        
loadFreeListAddress index =  
    do rfl <- tempRegister
       let getAddress = [(Nothing, XOR rfl registerFLPs),
                         (Nothing, ADD rfl index)]
       return (rfl, getAddress, popTempRegister)

       
loadFirstFreeListBlockAddress :: Register -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ()) 
loadFirstFreeListBlockAddress index =
    do (r_free_list_address, la, ua) <- loadFreeListAddress index
       r_block_address <- tempRegister
       return (r_block_address, la ++ [(Nothing, EXCH r_block_address r_free_list_address)], popTempRegister >> ua) 

-- | Code generation for binary operators
cgBinOp :: BinOp -> Register -> Register -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
cgBinOp Add r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, XOR rt r1), (Nothing, ADD rt r2)], popTempRegister)
cgBinOp Sub r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, XOR rt r1), (Nothing, SUB rt r2)], popTempRegister)
cgBinOp Xor r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, XOR rt r1), (Nothing, XOR rt r2)], popTempRegister)
cgBinOp BitAnd r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, ANDX rt r1 r2)], popTempRegister)
cgBinOp BitOr r1 r2 = tempRegister >>= \rt -> return (rt, [(Nothing, ORX rt r1 r2)], popTempRegister)
cgBinOp Lt r1 r2 =
    do rt <- tempRegister
       rc <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Nothing, XOR rt r1),
                  (Nothing, SUB rt r2),
                  (Just l_top, BGEZ rt l_bot),
                  (Nothing, XORI rc $ Immediate 1),
                  (Just l_bot, BGEZ rt l_top)]
       return (rc, cmp, popTempRegister >> popTempRegister)
cgBinOp Gt r1 r2 =
    do rt <- tempRegister
       rc <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Nothing, XOR rt r1),
                  (Nothing, SUB rt r2),
                  (Just l_top, BLEZ rt l_bot),
                  (Nothing, XORI rc $ Immediate 1),
                  (Just l_bot, BLEZ rt l_top)]
       return (rc, cmp, popTempRegister >> popTempRegister)
cgBinOp Eq r1 r2 =
    do rt <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Just l_top, BNE r1 r2 l_bot),
                  (Nothing, XORI rt $ Immediate 1),
                  (Just l_bot, BNE r1 r2 l_top)]
       return (rt, cmp, popTempRegister)
cgBinOp Neq r1 r2 =
    do rt <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Just l_top, BEQ r1 r2 l_bot),
                  (Nothing, XORI rt $ Immediate 1),
                  (Just l_bot, BEQ r1 r2 l_top)]
       return (rt, cmp, popTempRegister)
cgBinOp Lte r1 r2 =
    do rt <- tempRegister
       rc <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Nothing, XOR rt r1),
                  (Nothing, SUB rt r2),
                  (Just l_top, BGTZ rt l_bot),
                  (Nothing, XORI rc $ Immediate 1),
                  (Just l_bot, BGTZ rt l_top)]
       return (rc, cmp, popTempRegister >> popTempRegister)
cgBinOp Gte r1 r2 =
    do rt <- tempRegister
       rc <- tempRegister
       l_top <- getUniqueLabel "cmp_top"
       l_bot <- getUniqueLabel "cmp_bot"
       let cmp = [(Nothing, XOR rt r1),
                  (Nothing, SUB rt r2),
                  (Just l_top, BLTZ rt l_bot),
                  (Nothing, XORI rc $ Immediate 1),
                  (Just l_bot, BLTZ rt l_top)]
       return (rc, cmp, popTempRegister >> popTempRegister)
cgBinOp _ _ _ = throwError "ICE: Binary operator not implemented"

-- | Code generation for expressions
cgExpression :: SExpression -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
cgExpression (Constant 0) = return (registerZero, [], return ())
cgExpression (Constant n) = tempRegister >>= \rt -> return (rt, [(Nothing, XORI rt $ Immediate n)], popTempRegister)
cgExpression (Variable i) = loadVariableValue i
cgExpression Nil = return (registerZero, [], return ())
cgExpression (Binary op e1 e2) =
    do (r1, l1, u1) <- cgExpression e1
       (r2, l2, u2) <- cgExpression e2
       (ro, lo, uo) <- cgBinOp op r1 r2
       return (ro, l1 ++ l2 ++ lo, uo >> u2 >> u1)

-- | Code generation for binary expressions
cgBinaryExpression :: SExpression -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
cgBinaryExpression e =
    do (re, le, ue) <- cgExpression e
       rt <- tempRegister
       l_top <- getUniqueLabel "f_top"
       l_bot <- getUniqueLabel "f_bot"
       let flatten = [(Just l_top, BEQ re registerZero l_bot),
                      (Nothing, XORI rt $ Immediate 1),
                      (Just l_bot, BEQ re registerZero l_top)]
       return (rt, le ++ flatten, popTempRegister >> ue)

-- | Code generation for assignments
cgAssign :: SIdentifier -> ModOp -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgAssign n modop e =
    do (rt, lt, ut) <- loadVariableValue n
       (re, le, ue) <- cgExpression e
       ue >> ut
       return $ lt ++ le ++ [(Nothing, cgModOp modop rt re)] ++ invertInstructions (lt ++ le)
    where cgModOp ModAdd = ADD
          cgModOp ModSub = SUB 
          cgModOp ModXor = XOR

loadForSwap :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
loadForSwap n = gets (symbolTable . saState) >>= \st ->
    case lookup n st of
        (Just ClassField {}) -> loadVariableValue n
        (Just (LocalVariable IntegerType _)) -> loadVariableValue n
        (Just (LocalVariable (ObjectType _) _)) -> loadVariableAddress n
        (Just (MethodParameter IntegerType _)) -> loadVariableValue n
        (Just (MethodParameter (ObjectType _) _)) -> loadVariableAddress n
        _ -> throwError $ "ICE: Invalid variable index " ++ show n

cgSwap :: SIdentifier -> SIdentifier -> CodeGenerator [(Maybe Label, MInstruction)]
cgSwap n1 n2 = if n1 == n2 then return [] else
    do (r1, l1, u1) <- loadForSwap n1
       (r2, l2, u2) <- loadForSwap n2
       u2 >> u1
       let swap = [(Nothing, XOR r1 r2), (Nothing, XOR r2 r1), (Nothing, XOR r1 r2)]
       return $ l1 ++ l2 ++ swap ++ invertInstructions (l1 ++ l2)

-- | Code generation for conditionals
cgConditional :: SExpression -> [SStatement] -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgConditional e1 s1 s2 e2 =
    do l_test <- getUniqueLabel "test"
       l_assert_t <- getUniqueLabel "assert_true"
       l_test_f <- getUniqueLabel "test_false"
       l_assert <- getUniqueLabel "assert"
       rt <- tempRegister
       (re1, le1, ue1) <- cgBinaryExpression e1
       ue1
       s1' <- concat <$> mapM cgStatement s1
       s2' <- concat <$> mapM cgStatement s2
       (re2, le2, ue2) <- cgBinaryExpression e2
       ue2 >> popTempRegister --rt
       return $ le1 ++ [(Nothing, XOR rt re1)] ++ invertInstructions le1 ++
                [(Just l_test, BEQ rt registerZero l_test_f), (Nothing, XORI rt $ Immediate 1)] ++
                s1' ++ [(Nothing, XORI rt $ Immediate 1), (Just l_assert_t, BRA l_assert), (Just l_test_f, BRA l_test)] ++
                s2' ++ [(Just l_assert, BNE rt registerZero l_assert_t)] ++
                le2 ++ [(Nothing, XOR rt re2)] ++ invertInstructions le2

-- | Code generation for loops
cgLoop :: SExpression -> [SStatement] -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgLoop e1 s1 s2 e2 =
    do l_entry <- getUniqueLabel "entry"
       l_test <- getUniqueLabel "test"
       l_assert <- getUniqueLabel "assert"
       l_exit <- getUniqueLabel "exit"
       rt <- tempRegister
       (re1, le1, ue1) <- cgBinaryExpression e1
       ue1
       s1' <- concat <$> mapM cgStatement s1
       s2' <- concat <$> mapM cgStatement s2
       (re2, le2, ue2) <- cgBinaryExpression e2
       ue2 >> popTempRegister --rt
       return $ [(Nothing, XORI rt $ Immediate 1), (Just l_entry, BEQ rt registerZero l_assert)] ++
                le1 ++ [(Nothing, XOR rt re1)] ++ invertInstructions le1 ++
                s1' ++ le2 ++ [(Nothing, XOR rt re2)] ++ invertInstructions le2 ++
                [(Just l_test, BNE rt registerZero l_exit)] ++ s2' ++
                [(Just l_assert, BRA l_entry), (Just l_exit, BRA l_test), (Nothing, XORI rt $ Immediate 1)]

-- | Code generation for object blocks (Stack direction fixed)
cgObjectBlock :: TypeName -> SIdentifier -> [SStatement] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectBlock tp n stmt =
    do rn <- pushRegister n
       rv <- tempRegister
       popTempRegister --rv
       stmt' <- concat <$> mapM cgStatement stmt
       popRegister --rn
       let create = [(Nothing, XOR rn registerSP),
                     (Nothing, XORI rv $ AddressMacro $ "l_" ++ tp ++ "_vt"),
                     (Nothing, EXCH rv registerSP),
                     (Nothing, SUBI registerSP $ SizeMacro tp)]
       return $ create ++ stmt' ++ invertInstructions create

-- | Code generation for local blocks (Stack direction fixed)
cgLocalBlock :: SIdentifier -> SExpression -> [SStatement] -> SExpression -> CodeGenerator [(Maybe Label, MInstruction)]
cgLocalBlock n e1 stmt e2 =
    do rn <- pushRegister n
       (re1, le1, ue1) <- cgExpression e1
       rt1 <- tempRegister
       popTempRegister >> ue1
       stmt' <- concat <$> mapM cgStatement stmt
       (re2, le2, ue2) <- cgExpression e2
       rt2 <- tempRegister
       popTempRegister >> ue2
       popRegister --rn
       let create re rt = [(Nothing, XOR rn registerSP),
                           (Nothing, XOR rt re),
                           (Nothing, EXCH rt registerSP),
                           (Nothing, SUBI registerSP $ Immediate 1)]
           load = le1 ++ create re1 rt1 ++ invertInstructions le1
           clear = le2 ++ invertInstructions (create re2 rt2) ++ invertInstructions le2
       return $ load ++ stmt' ++ clear

-- | Code generation for calls (Stack direction fixed)
cgCall :: [SIdentifier] -> [(Maybe Label, MInstruction)] -> Register -> CodeGenerator [(Maybe Label, MInstruction)]
cgCall args jump this =
    do (ra, la, ua) <- unzip3 <$> mapM loadVariableAddress args
       sequence_ ua
       rs <- gets registerStack
       let rr = (registerThis : map snd rs) \\ (this : ra)
           store = concatMap push $ rr ++ ra ++ [this]
       return $ concat la ++ store ++ jump ++ invertInstructions store ++ invertInstructions (concat la)
    where push r = [(Nothing, EXCH r registerSP), (Nothing, SUBI registerSP $ Immediate 1)]

cgLocalCall :: SIdentifier -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgLocalCall m args = getMethodLabel m >>= \l_m -> cgCall args [(Nothing, BRA l_m)] registerThis

cgLocalUncall :: SIdentifier -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgLocalUncall m args = getMethodLabel m >>= \l_m -> cgCall args [(Nothing, RBRA l_m)] registerThis

-- | Returns the type associated with a given identifier
getType :: SIdentifier -> CodeGenerator TypeName
getType i = gets (symbolTable . saState) >>= \st ->
    case lookup i st of
        (Just (LocalVariable (ObjectType tp) _)) -> return tp
        (Just (ClassField (ObjectType tp) _ _ _)) -> return tp
        (Just (MethodParameter (ObjectType tp) _)) -> return tp
        _ -> throwError $ "ICE: Invalid object variable index " ++ show i

-- | Load address for method (??)
loadMethodAddress :: (SIdentifier, Register) -> MethodName -> CodeGenerator (Register, [(Maybe Label, MInstruction)])
loadMethodAddress (o, ro) m =
    do rv <- tempRegister
       rt <- tempRegister
       rtgt <- tempRegister
       popTempRegister >> popTempRegister >> popTempRegister
       offsetMacro <- OffsetMacro <$> getType o <*> pure m
       let load = [(Nothing, EXCH rv ro),
                   (Nothing, ADDI rv offsetMacro),
                   (Nothing, EXCH rt rv),
                   (Nothing, XOR rtgt rt),
                   (Nothing, EXCH rt rv),
                   (Nothing, SUBI rv offsetMacro),
                   (Nothing, EXCH rv ro)]
       return (rtgt, load)

-- | Load address or value needed for calls
loadForCall :: SIdentifier -> CodeGenerator (Register, [(Maybe Label, MInstruction)], CodeGenerator ())
loadForCall n = gets (symbolTable . saState) >>= \st ->
    case lookup n st of
        (Just ClassField {}) -> loadVariableValue n
        (Just _) -> loadVariableAddress n
        _ -> throwError $ "ICE: Invalid variable index " ++ show n

-- | Code generation for object calls
cgObjectCall :: SIdentifier -> MethodName -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectCall o m args =
    do (ro, lo, uo) <- trace "IN OBJECT CALL" loadForCall o
       rt <- tempRegister
       (rtgt, loadAddress) <- loadMethodAddress (o, rt) m
       l_jmp <- getUniqueLabel "l_jmp"
       let jp = [(Nothing, SUBI rtgt $ AddressMacro l_jmp),
                 (Just l_jmp, SWAPBR rtgt),
                 (Nothing, NEG rtgt),
                 (Nothing, ADDI rtgt $ AddressMacro l_jmp)]
       call <- cgCall args jp rt
       popTempRegister >> uo
       let load = lo ++ [(Nothing, XOR rt ro)] ++ loadAddress ++ invertInstructions lo
       return $ load ++ call ++ invertInstructions load

-- | Code generation for object uncalls
cgObjectUncall :: SIdentifier -> MethodName -> [SIdentifier] -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectUncall o m args =
    do (ro, lo, uo) <- loadForCall o
       rt <- tempRegister
       (rtgt, loadAddress) <- loadMethodAddress (o, rt) m
       l_jmp <- getUniqueLabel "l_jmp"
       l_rjmp_top <- getUniqueLabel "l_rjmp_top"
       l_rjmp_bot <- getUniqueLabel "l_rjmp_bot"
       let jp = [(Nothing, SUBI rtgt $ AddressMacro l_jmp),
                 (Just l_rjmp_top, RBRA l_rjmp_bot),
                 (Just l_jmp, SWAPBR rtgt),
                 (Nothing, NEG rtgt),
                 (Just l_rjmp_bot, BRA l_rjmp_top),
                 (Nothing, ADDI rtgt $ AddressMacro l_jmp)]
       call <- cgCall args jp rt
       popTempRegister >> uo
       let load = lo ++ [(Nothing, XOR rt ro)] ++ loadAddress ++ invertInstructions lo
       return $ load ++ call ++ invertInstructions load


cgMalloc :: Register -> Register -> Register -> Register -> Label -> TypeName -> CodeGenerator [(Maybe Label, MInstruction)]
cgMalloc r_p r_object_size r_counter r_csize l_m_entry tp =
    do l_o_test <- getUniqueLabel "o_test"
       l_o_assert_t <- getUniqueLabel "o_assert_true"
       l_o_test_f <- getUniqueLabel "o_test_false"
       l_o_assert <- getUniqueLabel "o_assert"
       l_i_test <- getUniqueLabel "i_test"
       l_i_assert_t <- getUniqueLabel "i_assert_true"
       l_i_test_f <- getUniqueLabel "i_test_false"
       l_i_assert <- getUniqueLabel "i_assert"
       l_m_top <- getUniqueLabel "malloc_top"
       l_m_bot <- getUniqueLabel "malloc_bot"
    
       -- Temp registers needed for malloc
       rt <- tempRegister
       rt2 <- tempRegister
       r_tmp <- tempRegister

       -- Expressions and sub routines
       (r_e1_outer, l_e1_outer, u_e1_outer) <- cgBinOp Lt r_csize r_object_size
       (r_e2_outer, l_e2_outer, u_e2_outer) <- cgBinOp Lt r_csize r_object_size
       (r_fl, l_fl, u_fl) <- loadFreeListAddress r_counter
       (r_block, l_block, u_block) <- loadFirstFreeListBlockAddress r_counter
       (r_e1_inner, l_e1_inner, u_e1_inner) <- cgBinOp Neq r_block registerZero
       (r_e2_i, l_e2_i, u_e2_i) <- cgBinOp Neq r_p r_tmp

       -- Update state
       u_e2_i >> u_e1_inner >> u_block >> u_fl >> u_e2_outer >> u_e1_outer

       popTempRegister -- r_tmp
       popTempRegister -- rt2
       popTempRegister -- rt

       let malloc = [(Just l_m_top, BRA l_m_bot),               -- 
                     (Nothing, ADDI registerSP $ Immediate 1),  
                     (Nothing, EXCH registerRO registerSP),-- Pop return offset from stack
                     (Just l_m_entry, SWAPBR registerRO),  -- Malloc1 entry/exit point
                     (Nothing, NEG registerRO),            -- Restore return offset
                     (Nothing, EXCH registerRO registerSP),-- Push return offset to stack
                     (Nothing, SUBI registerSP $ Immediate 1)]
                    ++ l_e1_outer                               -- Set r_e1 -> c_size < obj_size
                    ++ [(Nothing, XOR rt r_e1_outer)]           -- r_t = r_e1_o 
                    ++ invertInstructions l_e1_outer ++         -- Clear r_e1_o
                    [(Just l_o_test, BEQ rt registerZero l_o_test_f),
                     (Nothing, XORI rt $ Immediate 1),          -- S1_outer start
                     (Nothing, ADDI r_counter $ Immediate 1),   -- counter++
                     (Nothing, RL r_csize $ Immediate 1),       -- call double(csize)
                     (Nothing, BRA l_m_entry),                  -- call malloc1()
                     (Nothing, RR r_csize $ Immediate 1),       -- uncall double(csize)
                     (Nothing, XORI rt $ Immediate 1),          -- S1_outer end
                     (Just l_o_assert_t, BRA l_o_assert),
                     (Just l_o_test_f, BRA l_o_test)]
                     -- S2_outer start
                     ++ l_fl                                    -- Set r_fl -> address of free_list[counter]
                     ++ l_block                                 -- Set r_block -> value of free_list[counter]
                     ++ l_e1_inner                              -- Set r_e1_i -> r_block != 0
                     ++ [(Nothing, XOR rt2 r_e1_inner)]         -- Set rt2 -> r_e1_i         
                     ++ invertInstructions l_e1_inner           -- Clear r_e1_i
                     ++ invertInstructions l_block
                     ++ invertInstructions l_fl ++
                    [(Just l_i_test, BEQ rt2 r_e1_inner l_i_test_f),
                     (Nothing, XORI rt2 $ Immediate 1)]         -- S1_inner start
                    ++ l_fl
                    ++ l_block ++
                    [--(Nothing, EXCH r_block r_fl),              -- Get address of block stored in current free list
                     (Nothing, ADD r_p r_block),                -- Set address of p to said block
                     (Nothing, SUB r_block r_p),                -- Clear r_block
                     (Nothing, EXCH r_tmp r_p),                 -- Load address of next block
                     (Nothing, EXCH r_tmp r_fl),                -- Set address of next block as head of current free list
                     (Nothing, XORI r_tmp $ AddressMacro $ "l_" ++ tp ++ "_vt"), 
                     (Nothing, EXCH r_tmp r_p),                 -- Store address of vtable in newly alloc'd obj
                     (Nothing, XORI rt2 $ Immediate 1),         -- S1_inner end
                     (Just l_i_assert_t, BRA l_i_assert),
                     (Just l_i_test_f, BRA l_i_test),
                     (Nothing, ADDI r_counter $ Immediate 1)]   -- S2_inner start
                    ++
                    [(Nothing, RL r_csize $ Immediate 1),       -- call double(csize)
                     (Nothing, BRA l_m_entry),                  -- call malloc1()
                     (Nothing, RR r_csize $ Immediate 1),       -- uncall double(csize)
                     (Nothing, SUBI r_counter $ Immediate 1)]   -- counter -= 1
                    ++ l_block
                     ++ l_fl ++ 
                    [(Nothing, XOR r_tmp r_p),                  -- Copy current address of p
                     (Nothing, EXCH r_tmp r_fl),                -- Store address in current free list
                     (Nothing, ADD r_p r_csize),                -- Set p to other half of the block we're splitting
                     (Just l_i_assert, BNE rt2 registerZero l_i_assert_t),
                     (Nothing, XOR r_tmp r_p),
                     (Nothing, SUB r_tmp r_csize)]    
                     ++ l_e2_i                              -- set r_i_2 -> p - csize != free_list[counter]
                     ++ [(Nothing, XOR rt2 r_e2_i)]          -- Set rt2 -> r_i_2
                     ++ invertInstructions l_e2_i           -- Clear r_i_2
                     ++ invertInstructions l_block
                     ++ invertInstructions l_fl ++
                     [(Nothing, ADD r_tmp r_csize),
                      (Nothing, XOR r_tmp r_p),             -- S2_outer end 
                      (Just l_o_assert, BNE rt registerZero l_o_assert_t)]         
                     ++ l_e2_outer                            -- Set r_e1 -> c_size < obj_size
                     ++ [(Nothing, XOR rt r_e2_outer)]        -- r_t = r_e1_o 
                     ++ invertInstructions l_e2_outer         -- Clear r_e1_o
                     ++ [(Just l_m_bot, BRA l_m_top)]         -- Go to top
       return malloc

-- | Code generation for object construction
cgObjectConstruction :: TypeName -> SIdentifier -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectConstruction tp n =
    do r_p <- pushRegister n
       r_object_size <- tempRegister
       r_counter <- tempRegister
       r_csize <- tempRegister
       l_m_entry <- getUniqueLabel "malloc_entry"

       popTempRegister >> popTempRegister >> popTempRegister -- r_csize, r_counter, r_object_size
       let malloc = [(Nothing, ADDI r_csize $ Immediate 2),
                   (Nothing, XOR r_counter registerZero),
                   (Nothing, ADDI r_object_size $ SizeMacro tp)]
       malloc1 <- cgMalloc r_p r_object_size r_counter r_csize l_m_entry tp       
       return $ malloc ++ [(Nothing, BRA l_m_entry)] ++ malloc1 ++ invertInstructions malloc

-- | Code generation for object destruction
-- TODO: Implement
cgObjectDestruction :: TypeName -> SIdentifier -> CodeGenerator [(Maybe Label, MInstruction)]
cgObjectDestruction tp n =
    do rv <- tempRegister
       popTempRegister --rv
       let create = [(Nothing, XOR rv registerSP),
                     (Nothing, XORI rv $ AddressMacro $ "l_" ++ tp ++ "_vt"),
                     (Nothing, EXCH rv registerSP),
                     (Nothing, ADDI registerSP $ SizeMacro tp)]
       return $ create ++ invertInstructions create

-- | Code generation for statements
cgStatement :: SStatement -> CodeGenerator [(Maybe Label, MInstruction)]
cgStatement (Assign n modop e) = cgAssign n modop e
cgStatement (Swap n1 n2) = cgSwap n1 n2
cgStatement (Conditional e1 s1 s2 e2) = cgConditional e1 s1 s2 e2
cgStatement (Loop e1 s1 s2 e2) = cgLoop e1 s1 s2 e2
cgStatement (ObjectBlock tp n stmt) = cgObjectBlock tp n stmt
cgStatement (LocalBlock n e1 stmt e2) = cgLocalBlock n e1 stmt e2
cgStatement (LocalCall m args) = cgLocalCall m args
cgStatement (LocalUncall m args) = cgLocalUncall m args
cgStatement (ObjectCall o m args) = cgObjectCall o m args
cgStatement (ObjectUncall o m args) = cgObjectUncall o m args
cgStatement (ObjectConstruction tp n) = cgObjectConstruction tp n
cgStatement (ObjectDestruction tp n)  = cgObjectDestruction tp n
cgStatement Skip = return []

-- | Code generation for methods (Stack direction fixed)
cgMethod :: (TypeName, SMethodDeclaration) -> CodeGenerator [(Maybe Label, MInstruction)]
cgMethod (_, GMDecl m ps body) =
    do l <- getMethodLabel m
       rs <- addParameters
       body' <- concat <$> mapM cgStatement body
       clearParameters
       let lt = l ++ "_top"
           lb = l ++ "_bot"
           mp = [(Just lt, BRA lb),
                 (Nothing, ADDI registerSP $ Immediate 1),
                 (Nothing, EXCH registerRO registerSP)]
                 ++ concatMap pushParameter rs ++
                [(Nothing, EXCH registerThis registerSP),
                 (Nothing, SUBI registerSP $ Immediate 1),
                 (Just l, SWAPBR registerRO),
                 (Nothing, NEG registerRO),
                 (Nothing, ADDI registerSP $ Immediate 1),
                 (Nothing, EXCH registerThis registerSP)]
                 ++ invertInstructions (concatMap pushParameter rs) ++
                [(Nothing, EXCH registerRO registerSP),
                 (Nothing, SUBI registerSP $ Immediate 1)]
       return $ mp ++ body' ++ [(Just lb, BRA lt)]
    where addParameters = mapM (pushRegister . (\(GDecl _ p) -> p)) ps
          clearParameters = replicateM_ (length ps) popRegister
          pushParameter r = [(Nothing, EXCH r registerSP), (Nothing, SUBI registerSP $ Immediate 1)]

-- | Code generation for virtual tables
cgVirtualTables :: CodeGenerator [(Maybe Label, MInstruction)]
cgVirtualTables = concat <$> (gets (virtualTables . saState) >>= mapM vtInstructions)
    where vtInstructions (n, ms) = zip (vtLabel n) <$> mapM vtData ms
          vtData m = DATA . AddressMacro <$> getMethodLabel m
          vtLabel n = (Just $ "l_" ++ n ++ "_vt") : repeat Nothing

-- | Returns the main class label
getMainLabel :: CodeGenerator Label
getMainLabel = gets (mainMethod . saState) >>= getMethodLabel

-- | Fetches the main class from the class analysis state
getMainClass :: CodeGenerator TypeName
getMainClass = gets (mainClass . caState . saState) >>= \mc ->
    case mc of
        (Just tp) -> return tp
        Nothing -> throwError "ICE: No main method defined"

-- | Fetches the field of a given type name
-- FIXME: Inheritance
getFields :: TypeName -> CodeGenerator [VariableDeclaration]
getFields tp =
    do cs <- gets (classes . caState . saState)
       case lookup tp cs of
           (Just (GCDecl _ fs _)) -> return fs
           Nothing -> throwError $ "ICE: Unknown class " ++ tp

-- | Code generation for output (Stack direction fixed)
cgOutput :: TypeName -> CodeGenerator ([(Maybe Label, MInstruction)], [(Maybe Label, MInstruction)])
cgOutput tp =
    do mfs <- getFields tp
       co <- concat <$> mapM cgCopyOutput (zip [1..] $ reverse mfs)
       return (map cgStatic mfs, co)
    where cgStatic (GDecl _ n) = (Just $ "l_r_" ++ n, DATA $ Immediate 0)
          cgCopyOutput(o, GDecl _ n) =
              do rt <- tempRegister
                 ra <- tempRegister
                 popTempRegister >> popTempRegister
                 let copy = [ADDI registerSP $ Immediate o,
                             EXCH rt registerSP,
                             XORI ra $ AddressMacro $ "l_r_" ++ n,
                             EXCH rt ra,
                             XORI ra $ AddressMacro $ "l_r_" ++ n,
                             SUBI registerSP $ Immediate o]
                 return $ zip (repeat Nothing) copy

-- | Generates code for the program entry point (Stack direction fixed)
cgProgram :: SProgram -> CodeGenerator PISA.MProgram
cgProgram p =
    do vt <- cgVirtualTables
       rv <- tempRegister -- V table register
       rb <- tempRegister -- Memory block register
       popTempRegister >> popTempRegister
       ms <- concat <$> mapM cgMethod p
       l_main <- getMainLabel
       mtp <- getMainClass
       (out, co) <- cgOutput mtp
       let mvt = "l_" ++ mtp ++ "_vt"
           mn = [(Just "start", BRA "top"),
                 (Nothing, START),
                 (Nothing, ADDI registerFLPs ProgramSize),    -- Init free list pointer list
                 (Nothing, XOR registerHP registerFLPs),      -- Init heap pointer
                 (Nothing, ADDI registerHP FreeListsSize),    -- Init space for FLPs list TODO: Check for off by one her
                 (Nothing, XOR rb registerHP),                -- Store address of initial memory block in rb
                 (Nothing, ADDI registerFLPs FreeListsSize),  -- Index to end of free lists
                 (Nothing, SUBI registerFLPs $ Immediate 1),  -- Index to last element of free lists
                 (Nothing, EXCH rb registerFLPs),             -- Store address of first block in last element of free lists
                 (Nothing, ADDI registerFLPs $ Immediate 1),  -- Index to end of free lists
                 (Nothing, SUBI registerFLPs FreeListsSize),  -- Index to beginning of free lists
                 (Nothing, ADDI registerSP StackOffset),      -- Init stack pointer
                 (Nothing, XOR registerThis registerSP),      -- Store address of main object
                 (Nothing, XORI rv $ AddressMacro mvt),       -- Store address of vtable in rv
                 (Nothing, EXCH rv registerSP),               -- Add address of vtable to stack
                 (Nothing, SUBI registerSP $ SizeMacro mtp),  -- Allocate space for object on stack
                 (Nothing, EXCH registerThis registerSP),     -- Push 'this' to stack
                 (Nothing, SUBI registerSP $ Immediate 1),    -- Push 'this' to stack
                 (Nothing, BRA l_main),                       -- Execute main
                 (Nothing, ADDI registerSP $ Immediate 1),    -- Pop 'this'
                 (Nothing, EXCH registerThis registerSP)]     -- Pop 'this'
                  ++ co ++
                [(Nothing, ADDI registerSP $ SizeMacro mtp),  -- Deallocate space for program
                 (Nothing, EXCH rv registerSP),               -- Pop vtable address
                 (Nothing, XORI rv $ AddressMacro mvt),       -- Clear rv
                 (Nothing, XOR registerThis registerSP),      -- Clear 'this'
                 (Nothing, SUBI registerSP StackOffset),      -- Clear stack pointer
                 (Nothing, ADDI registerFLPs FreeListsSize),  -- Index to end of free lists
                 (Nothing, SUBI registerFLPs $ Immediate 4),  -- Index to last element of free lists
                 (Nothing, EXCH rb registerFLPs),             -- Remove address of first block in last element of free lists
                 (Nothing, ADDI registerFLPs $ Immediate 4),  -- Index to end of free lists
                 (Nothing, SUBI registerFLPs FreeListsSize),  -- Index to beginning of free lists
                 (Nothing, XOR rb registerHP),                -- clear rb
                 (Nothing, SUBI registerHP FreeListsSize),    -- Reset Heap pointer
                 (Nothing, XOR registerHP registerFLPs),      -- Reset Heap pointer
                 (Nothing, SUBI registerFLPs ProgramSize),    -- Reset Free lists
                 (Just "finish", FINISH)]
       return $ PISA.GProg $ [(Just "top", BRA "start")] ++ out ++ vt ++ ms ++ mn


-- | Generates code for a program
generatePISA :: (SProgram, SAState) -> Except String (PISA.MProgram, SAState)
generatePISA (p, s) = second saState <$> runStateT (runCG $ cgProgram p) (initialState s)

showPISAProgram :: (Show a, MonadIO m) => (t, a) -> m ()
showPISAProgram (_, s) = pPrint s
