import Control.Monad.Except
import System.IO

import AST
import Parser
import ClassAnalyzer

type Error = String

main :: IO ()
main =
    do handle <- openFile "../test/Example.rplpp" ReadMode
       input <- hGetContents handle
       either (hPutStrLn stderr) (putStr . printCAState) (compileProgram input)

compileProgram :: String -> Either Error (Program, CAState)
compileProgram s =
    runExcept $
    parseString s
    >>= classAnalysis

