{-# LANGUAGE BangPatterns #-}
import Control.Monad.Except
import System.IO

import AST
import Parser
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as C

type Error = String

main = do
        handle <- openFile "../test/Example.rplpp" ReadMode
        contents <- hGetContents handle
        case parseString $ contents of
            Left err -> print err
            Right xs -> putStr $ printAST xs
        hClose handle

--
-- main :: IO ()
-- main =
--     do input <- Str.readFile "example.rpl"
--        case parseString $ C.unpack input of
--            Left err -> print err
--            Right xs -> putStr $ printAST xs