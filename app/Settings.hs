{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Settings
  ( readSettings

  , Settings (..)
  , sHosts
  , sTitle

  , Host (..)
  , hUser
  , hkeyFile
  ) where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Control.Exception.Safe (throwString)
import           Control.Lens (makeLenses)
import qualified System.Directory as Dir
import qualified System.Environment as Env


data Settings = Settings
  { _sHosts :: !(Map Text Host)
  , _sTitle :: !(Maybe Text)
  } deriving (Show, Generic)

data Host = Host
  { _hUser :: !(Maybe Text)
  , _hkeyFile :: !(Maybe FilePath)
  } deriving (Show, Generic)



readSettings :: IO (Maybe Settings)
readSettings = do
  settingsPath <- Dir.getXdgDirectory Dir.XdgConfig "awssy/awssy.js"

  Dir.doesFileExist settingsPath >>= \case
    True -> Just <$> readJs settingsPath
    False -> pure Nothing

  where
    readJs p = do
      b <- BS.readFile p
      case Ae.eitherDecode $ BSL.fromStrict b of
        Left e -> throwString $ "Error reading settings: " <> p <> "\n" <> e
        Right r' -> do
          r <- expandValues r'
          case Ae.fromJSON r of
            Ae.Error e -> throwString $ "Error reading settings values: " <> p <> "\n" <> e
            Ae.Success s -> pure s


expandText :: Text -> IO Text
expandText s = do
  let ss = Txt.splitOn "${" s
  let start = take 1 ss
  rest <- for (drop 1 ss) $ \p ->
        if Txt.elem '}' p
          then do
            let (h, t) = Txt.breakOn "}" p
            e <- Env.lookupEnv (Txt.unpack h) >>= \case
                   Just v -> pure v
                   Nothing -> throwString $ "Unknown env var: " <> Txt.unpack h
            pure [Txt.pack e, Txt.drop 1 t]
          else
            pure [p]
  pure . Txt.concat $ start <> concat rest


expandValues :: Ae.Value -> IO Ae.Value
expandValues v =
  case v of
    Ae.String s -> Ae.String <$> expandText s
    Ae.Array as -> Ae.Array <$> traverse expandValues as
    Ae.Object o -> Ae.Object <$> traverse expandValues o
    Ae.Bool _ -> pure v
    Ae.Null -> pure v
    Ae.Number _ -> pure v


instance Ae.FromJSON Settings where
  parseJSON = Ae.genericParseJSON (recOptionsSnake 2)
instance Ae.ToJSON Settings where
  toJSON = Ae.genericToJSON (recOptionsSnake 2)

instance Ae.FromJSON Host where
  parseJSON = Ae.genericParseJSON (recOptionsSnake 2)
instance Ae.ToJSON Host where
  toJSON = Ae.genericToJSON (recOptionsSnake 2)


recOptionsSnake :: Int -> Ae.Options
recOptionsSnake n =
  recOptions
    { Ae.fieldLabelModifier = renSnake n
    , Ae.sumEncoding = Ae.ObjectWithSingleField
    , Ae.constructorTagModifier = renSnake 0
    }

recOptions :: Ae.Options
recOptions =
  Ae.defaultOptions
    { Ae.allNullaryToStringTag = False
    }

renSnake :: Int -> [Char] -> [Char]
renSnake d = Ae.camelTo2 '_' . drop d


makeLenses ''Settings
makeLenses ''Host
