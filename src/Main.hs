{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Environment          (getArgs)
import qualified Data.Text.Read        as TR
import qualified Data.Yaml             as Y
import qualified Data.Yaml.Parser      as YP
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as HM
import           Control.Monad               (when)
import           Data.Maybe                  (fromMaybe)
import           System.Exit                 (exitWith, ExitCode(..) )

data Aspect = Aspect { aspecttype :: Maybe String, aspectname :: String, aspectdescription :: Maybe String } deriving (Show)

data Skill = Skill { skilllevel :: Int, skillname :: String } deriving (Show)

data SkillGroup = SkillGroup { skillgrouplevel :: Int, skillgroupskills :: [String] } deriving (Show)

data Extra = Extra { extraname :: String, extradescription :: Maybe String } deriving (Show)

data Stunt = Stunt { stuntname :: String, stuntdescription :: Maybe String } deriving (Show)

data FateChar =
        FateChar {
              charsystem :: Maybe String
            , charname :: String
            , chardescription :: Maybe String
            , charrefreshrate :: Int
            , charaspects :: [Aspect]
            , charskillgroups :: [SkillGroup]
            , charextras :: [Extra]
            , charstunts :: [Stunt]
        } deriving (Show)

instance Y.FromJSON Aspect where
    parseJSON (Y.Object m) = Aspect <$>
        m Y..:? "type" <*>
        m Y..:  "name" <*>
        m Y..:? "description"
    parseJSON x = fail ("not an object: " ++ show x)

instance Y.FromJSON SkillGroup where
    parseJSON (Y.Object m) =
        if HM.size m /= 1 then fail "Exactly one skill level must be given per skill level"
        else do
            let [(lt, st)] = HM.toList m
            case TR.signed TR.decimal lt of
                Left err -> fail err
                Right (level, _) -> do
                    skills <- Y.parseJSON st
                    return $ SkillGroup level skills
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
        m Y..:  "aspects" <*>
        m Y..:  "skills" <*>
        m Y..:  "extras" <*>
        m Y..:  "stunts"
    parseJSON x = fail ("not an object: " ++ show x)

main :: IO ()
main = do
    args <- getArgs
    when (length args < 1) $ putStrLn "Error: no input file given" >> exitWith (ExitFailure 1)
    let filename = head args
    content <- BS.readFile filename
    case Y.decodeEither content :: Either String FateChar of
        Left err -> putStrLn ("Error parsing the character: " ++ err) >> exitWith (ExitFailure 1)
        Right parsedContent ->
            print parsedContent
