{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

module FateYAML where

import           GHC.Generics
import qualified Data.Text             as T
import qualified Data.Text.Read        as TR
import qualified Data.Yaml             as Y
import qualified Data.Yaml.Parser      as YP
import qualified Data.Aeson.Types      as AT
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as HM

import           FateCore

instance Y.FromJSON Aspect where
    parseJSON (Y.String s) = return $ Aspect
        { aspecttype = Nothing
        , aspectname = T.unpack s
        , aspectdescription = Nothing
        }
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
                    skills <- case st of
                        Y.Array _ -> Y.parseJSON st
                        _ -> (:[]) <$> (Y.parseJSON st)
                    return $ SkillGroup level skills
    parseJSON x = fail ("not an object: " ++ show x)

instance Y.FromJSON Extra where
    parseJSON (Y.String s) = return $ Extra
        { extraname = T.unpack s
        , extradescription = Nothing
        }
    parseJSON (Y.Object m) = Extra <$>
        m Y..:  "name" <*>
        m Y..:? "description"
    parseJSON x = fail ("not an object: " ++ show x)

instance Y.FromJSON Stunt where
    parseJSON (Y.String s) = return $ Stunt
        { stuntname = T.unpack s
        , stuntdescription = Nothing
        }
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

instance Y.ToJSON Aspect where
    toJSON     = AT.genericToJSON encodingOptions
    toEncoding = AT.genericToEncoding encodingOptions

signedSkillLevel :: Int -> T.Text
signedSkillLevel l = T.pack ((if l > 0 then "+" else "") ++ show l)

instance Y.ToJSON SkillGroup where
    toJSON (SkillGroup level [skill]) = Y.object [signedSkillLevel level Y..= skill]
    toJSON (SkillGroup level skills) = Y.object [signedSkillLevel level Y..= skills]
    toEncoding (SkillGroup level [skill]) = AT.pairs (signedSkillLevel level Y..= skill)
    toEncoding (SkillGroup level skills) = AT.pairs (signedSkillLevel level Y..= skills)

instance Y.ToJSON Extra where
    toJSON     = AT.genericToJSON encodingOptions
    toEncoding = AT.genericToEncoding encodingOptions

instance Y.ToJSON Stunt where
    toJSON     = AT.genericToJSON encodingOptions
    toEncoding = AT.genericToEncoding encodingOptions

instance Y.ToJSON FateChar where
    toJSON     = AT.genericToJSON encodingOptions
    toEncoding = AT.genericToEncoding encodingOptions

encodingOptions :: AT.Options
encodingOptions = AT.defaultOptions
        { AT.fieldLabelModifier      = fieldLabelMod
        , AT.omitNothingFields       = True
        }
    where fieldLabelMod ('a':'s':'p':'e':'c':'t':s) = s
          fieldLabelMod ('s':'k':'i':'l':'l':'g':'r':'o':'u':'p':s) = s
          fieldLabelMod ('s':'k':'i':'l':'l':s) = s
          fieldLabelMod ('e':'x':'t':'r':'a':s) = s
          fieldLabelMod ('s':'t':'u':'n':'t':s) = s
          fieldLabelMod ('y':'a':'m':'l':s) = s
          fieldLabelMod s = s

