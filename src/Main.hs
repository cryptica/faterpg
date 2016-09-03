module Main where

import           System.Environment          (getArgs)
import           System.Exit                 (exitWith, ExitCode(..) )
import qualified Data.Yaml             as Y
import qualified Data.ByteString.Char8 as BS
import           Control.Monad               (when)

import           FateCore
import           FateYAML

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ putStrLn "Error: no input file given" >> exitWith (ExitFailure 1)
    let filename = head args
    content <- BS.readFile filename
    case Y.decodeEither content :: Either String FateChar of
        Left err -> putStrLn ("Error parsing the character: " ++ err) >> exitWith (ExitFailure 1)
        Right parsedChar -> do
            let fateChar = normalizeSkillGroups parsedChar
            let content = Y.encode fateChar
            BS.putStrLn content
