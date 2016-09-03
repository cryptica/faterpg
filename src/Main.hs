{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment          (getArgs)
import qualified Data.Yaml             as Y
import qualified Data.ByteString.Char8 as BS
import           Control.Monad               (when)
import           Data.Maybe                  (fromMaybe)
import           System.Exit                 (exitWith, ExitCode(..) )

--import GHC.Generics
--import Data.Aeson

data Aspect = Aspect { aspecttype :: Maybe String, aspectname :: String, aspectdescription :: Maybe String } deriving (Show)

data Skill = Skill { skilllevel :: Int, skillname :: String } deriving (Show)

data Extra = Extra { extraname :: String, extradescription :: Maybe String } deriving (Show)

data Stunt = Stunt { stuntname :: String, stuntdescription :: Maybe String } deriving (Show)

data FateChar =
        FateChar {
              charsystem :: Maybe String
            , charname :: String
            , chardescription :: Maybe String
            , charrefreshrate :: Int
            , charaspects :: [Aspect]
--            , charskills :: [Skill]
--            , charextras :: [Extra]
--            , charstunts :: [Stunt]
        } deriving (Show)

instance Y.FromJSON Aspect where
    parseJSON (Y.Object m) = Aspect <$>
        m Y..:? "type" <*>
        m Y..:  "name" <*>
        m Y..:? "description"
    parseJSON x = fail ("not an object: " ++ show x)

instance Y.FromJSON Extra where
    parseJSON (Y.Object m) = Extra <$>
        m Y..:  "name" <*>
        m Y..:? "description"
    parseJSON x = fail ("not an object: " ++ show x)

instance Y.FromJSON Stunt where
    parseJSON (Y.Object m) = Stunt <$>
        m Y..:  "name" <*>
        m Y..:? "description"
    parseJSON x = fail ("not an object: " ++ show x)

instance Y.FromJSON FateChar where
    parseJSON (Y.Object m) = FateChar <$>
        m Y..:? "system" <*>
        m Y..:  "name" <*>
        m Y..:? "description" <*>
        m Y..:  "refresh rate" <*>
        m Y..:  "aspects"
    parseJSON x = fail ("not an object: " ++ show x)

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ putStrLn "Need to specify character file" >> exitWith (ExitFailure 1)
    let filename = head args
    content <- BS.readFile filename
    let parsedContent = Y.decode content :: Maybe FateChar
    print parsedContent
