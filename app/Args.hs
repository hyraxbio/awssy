{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Args ( runArgs
            , version
            ) where

import           Protolude
import qualified Data.Map.Strict as Map
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.ByteString as BS
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as A
import           System.FilePath ((</>))
import qualified System.Directory as Dir

data Opts = Opts { key :: Text
                 , save :: Bool
                 }
            deriving (A.Data, Typeable)

version :: Text
version = "0.1.3.11"

runArgs :: (FilePath -> IO ()) -> IO ()
runArgs run = do
  -- If the user did not specify any arguments, pretend as "--help" was given
  opts <- A.cmdArgs mkArgs
  pem <- getPem opts
  run pem

getPem :: Opts -> IO FilePath
getPem opts
  | Txt.null (key opts) = loadSettings
  | save opts = saveSettings (key opts)
  | otherwise = pure (Txt.unpack $ key opts) 

loadSettings :: IO FilePath
loadSettings = do
  let def = "~/.ssh/id_rsa"
  p <- getSettingsFilePath

  Dir.doesFileExist p >>= \case
    True -> do
      cfgLines1 <- Txt.lines . TxtE.decodeUtf8 <$> BS.readFile p
      let cfgLines2 = Txt.strip <$> cfgLines1
      let cfgLines3 = filter (not . Txt.isPrefixOf "#") cfgLines2
      let cfg1 = Txt.breakOn "=" <$> cfgLines3
      let cfg2 = filter (not . Txt.null . snd) cfg1
      let cfg3 = (\(k, v) -> (Txt.strip k, Txt.strip . Txt.drop 1 $ v)) <$> cfg2
      let cfg = Map.fromList cfg3
      pure . Txt.unpack $ Map.findWithDefault def "key" cfg
    False ->
      pure $ Txt.unpack def

saveSettings :: Text -> IO FilePath
saveSettings pem = do
  p <- getSettingsFilePath
  BS.writeFile p $ TxtE.encodeUtf8 ("key=" <> pem)
  pure $ Txt.unpack pem

getSettingsFilePath :: IO FilePath
getSettingsFilePath = do
  p <- getSettingsRootPath
  pure $ p </> "awssy.conf"

getSettingsRootPath :: IO FilePath
getSettingsRootPath = do
  p <- Dir.getXdgDirectory Dir.XdgConfig "awssy"
  Dir.createDirectoryIfMissing True p
  pure p
  

mkArgs :: Opts
mkArgs =
  let
    opts = Opts { key  = ""    &= A.name "k" &= A.typFile &= A.help "ssh pem file" 
                , save = False &= A.name "s"              &= A.help "save key file setting as default" 
                }


    _PROGRAM_NAME = "awssy"
    _PROGRAM_VERSION = Txt.unpack version
    _PROGRAM_INFO = _PROGRAM_NAME <> " version " <> _PROGRAM_VERSION
    _PROGRAM_ABOUT = "awssy: aws terminal GUI"
    _COPYRIGHT = "(C) 2018 HyraxBio"
  in
    opts
    &= A.versionArg [A.explicit, A.name "version", A.summary _PROGRAM_INFO]
    &= A.summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= A.help _PROGRAM_ABOUT
    &= A.helpArg [A.explicit, A.name "help", A.name "h"]
    &= A.program _PROGRAM_NAME
