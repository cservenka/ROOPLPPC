import Control.Monad.Except
import System.IO
import System.Environment

import PISA
import Parser
import ClassAnalyzer
import ScopeAnalyzer
import TypeChecker
import CodeGenerator
import MacroExpander

import Data.List.Split

type Error = String

main :: IO ()
main =
    do args <- getArgs
       when (null args) (error "Supply input filename.\nUsage: ROOPLPPC input.rplpp output.pal\n")
       when (length args > 2) (error "Too many arguments.\nUsage: ROOPLPPC input.rplpp output.pal\n")
       handle <- openFile (head args) ReadMode
       input <- hGetContents handle
       let output =  if length args == 2 then last args else head (splitOn "." (head args)) ++ ".pal"
       either (hPutStrLn stderr) (writeProgram output) $ compileProgram input

compileProgram :: String -> Either Error PISA.Program
compileProgram s =
    runExcept $
    parseString s
    >>= classAnalysis
    >>= scopeAnalysis
    >>= typeCheck
    >>= generatePISA
    >>= expandMacros
