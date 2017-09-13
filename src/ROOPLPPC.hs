import Control.Monad.Except
import System.IO

import AST
import PISA
import Parser
import ClassAnalyzer
-- import VectorAnalyzer
import ScopeAnalyzer
import CodeGenerator
import MacroExpander

type Error = String

main :: IO ()
main =
    do handle <- openFile "../test/Example.rplpp" ReadMode
       input <- hGetContents handle
--        either (hPutStrLn stderr) printCAState (compileProgram input)
--        either (hPutStrLn stderr) printSAState (compileProgram input)
--        either (hPutStrLn stderr) showPISAProgram (compileProgram input)
--        either (hPutStrLn stderr) (putStr . showProgram) (compileProgram input)
       either (hPutStrLn stderr) (writeProgram) (compileProgram input)

-- compileProgram :: String -> Either Error (AST.Program, CAState)
-- compileProgram :: String -> Either String (SProgram, SAState)
-- compileProgram :: String -> Either String (PISA.MProgram, SAState)
compileProgram :: String -> Either Error PISA.Program
compileProgram s =
    runExcept $
    parseString s
    >>= classAnalysis
--     >>= vectorAnalysis
    >>= scopeAnalysis
    >>= generatePISA
    >>= expandMacros
