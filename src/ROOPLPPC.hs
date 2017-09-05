import Control.Monad.Except
import System.IO

import AST
import PISA
import Parser
import ClassAnalyzer
-- import VectorAnalyzer
import ScopeAnalyzer
import CodeGenerator

type Error = String

main :: IO ()
main =
    do handle <- openFile "../test/Example.rplpp" ReadMode
       input <- hGetContents handle
--        either (hPutStrLn stderr) (putStr . printCAState) (compileProgram input)
       either (hPutStrLn stderr) printSAState (compileProgram input)

-- compileProgram :: String -> Either Error (AST.Program, CAState)
compileProgram :: String -> Either String (SProgram, SAState)
compileProgram s =
    runExcept $
    parseString s
    >>= classAnalysis
--     >>= vectorAnalysis
    >>= scopeAnalysis
--     >>= generatePISA
