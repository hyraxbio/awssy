{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Args ( runArgs
            , version
            , Args (..)
            , aKeyFile
            , aAllowCache
            , aUser
            , aRegion
            ) where

import           Protolude
import qualified Data.Text as Txt
import           Control.Lens.TH (makeLenses)
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as A
import qualified System.Directory as Dir
import qualified Network.AWS.Types as AWS

data Opts = Opts { key :: !Text
                 , cache :: !Bool
                 , user :: !Text
                 , region :: !Text
                 } deriving (A.Data, Typeable)

data Args = Args { _aKeyFile :: !FilePath
                 , _aAllowCache :: !Bool
                 , _aUser :: !Text
                 , _aRegion :: !AWS.Region
                 } deriving (Show)

makeLenses ''Args

version :: Text
version = "0.2.1.19"

runArgs :: (Args -> IO ()) -> IO ()
runArgs run = do
  opts <- A.cmdArgs mkArgs
  runExceptT (parseOpts opts) >>= \case
    Left e -> putText e
    Right args -> run args


parseOpts :: Opts -> ExceptT Text IO Args
parseOpts opts = do
  awsRegion <-
    case readMaybe (Txt.unpack $ region opts) :: Maybe AWS.Region of
      Just r -> pure r
      Nothing -> throwE "Invalid region, please use one of: Beijing, Frankfurt, GovCloud, GovCloudFIPS, Ireland, London, Montreal, Mumbai, NorthCalifornia, NorthVirginia, Ohio, Oregon, SaoPaulo, Seoul, Singapore, Sydney, Tokyo"

  keyFile <- do
    let path = if Txt.null (key opts)
               then "~/.ssh/id_rsa"
               else key opts

    liftIO (Dir.doesFileExist $ Txt.unpack path) >>= \case
      True -> pure path
      False -> throwE $ "Unable to find key file: " <> path


  let userName =
        if Txt.null (user opts)
        then "ec2-user"
        else user opts
  
  pure Args { _aKeyFile = Txt.unpack keyFile
            , _aAllowCache = cache opts
            , _aUser = userName
            , _aRegion = awsRegion
            }

mkArgs :: Opts
mkArgs =
  let
    opts = Opts { key    = ""         &= A.name "k" &= A.help "ssh pem file" 
                , cache  = False      &= A.name "c" &= A.help "allow cached instance list"
                , user   = "ec2-user" &= A.name "u" &= A.help "AWS user"
                , region = ""         &= A.name "r" &= A.help "AWS region"
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
