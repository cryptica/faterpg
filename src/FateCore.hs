{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module FateCore where

import           GHC.Generics
import           Data.Function               (on)
import           Data.Ord                    (comparing, Down(..))
import           Data.List                   (sortBy, groupBy)

data Aspect = Aspect { aspecttype :: Maybe String, aspectname :: String, aspectdescription :: Maybe String } deriving (Show,Generic)

data Skill = Skill { skillname :: String, skilllevel :: Int } deriving (Show,Generic)

data SkillGroup = SkillGroup { skillgrouplevel :: Int, skillgroupskills :: [String] } deriving (Show,Generic)

data Extra = Extra { extraname :: String, extradescription :: Maybe String } deriving (Show,Generic)

data Stunt = Stunt { stuntname :: String, stuntdescription :: Maybe String } deriving (Show,Generic)

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
        } deriving (Show,Generic)

normalizeSkillGroups :: FateChar -> FateChar
normalizeSkillGroups char =
        let skills = concatMap skillGroupToSkills $ charskillgroups char
            skillgroups = skillsToSkillGroups $ skills
        in  FateChar {
              charsystem = charsystem char
            , charname = charname char
            , chardescription = chardescription char
            , charrefreshrate = charrefreshrate char
            , charaspects = charaspects char
            , charskillgroups = skillgroups
            , charextras = charextras char
            , charstunts = charstunts char
        }
    where
        skillGroupToSkills group =
            map (\s -> Skill { skillname = s, skilllevel = skillgrouplevel group}) $ skillgroupskills group
        skillsToSkillGroups skills =
            let skillsSorted = sortBy (comparing (Down . skilllevel)) skills
                skillsGrouped = groupBy ((==) `on` skilllevel) skillsSorted
            in  map (\s -> SkillGroup { skillgrouplevel = skilllevel $ head s, skillgroupskills = map skillname s }) skillsGrouped
