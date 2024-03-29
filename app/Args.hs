{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Args
  ( runArgs
  , version
  , Args (..)
  , aKeyFile
  , aUser
  ) where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.Version as Ver
import           Control.Lens.TH (makeLenses)
import           System.Console.CmdArgs ((&=))
import qualified System.Console.CmdArgs as A
import qualified System.Directory as Dir

import qualified Paths_awssy as Paths

data Opts = Opts { key :: !Text
                 , user :: !Text
                 } deriving (A.Data, Typeable)

data Args = Args { _aKeyFile :: !FilePath
                 , _aUser :: !Text
                 } deriving (Show)

makeLenses ''Args

version :: Text
version = Txt.pack $ Ver.showVersion Paths.version

runArgs :: (Args -> IO ()) -> IO ()
runArgs run = do
  opts <- A.cmdArgs mkArgs
  runExceptT (parseOpts opts) >>= \case
    Left e -> putText e
    Right args -> run args


parseOpts :: Opts -> ExceptT Text IO Args
parseOpts opts = do
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
            , _aUser = userName
            }

mkArgs :: Opts
mkArgs =
  let
    opts = Opts { key    = ""         &= A.name "k" &= A.help "ssh default pem file"
                , user   = "ec2-user" &= A.name "u" &= A.help "AWS default user"
                }

    _PROGRAM_NAME = "awssy"
    _PROGRAM_VERSION = Txt.unpack version
    _PROGRAM_INFO = _PROGRAM_NAME <> " version " <> _PROGRAM_VERSION
    _PROGRAM_ABOUT = "awssy: aws terminal GUI"
    _COPYRIGHT = "(C) 2018-2022 HyraxBio"
  in
    opts
    &= A.versionArg [A.explicit, A.name "version", A.summary _PROGRAM_INFO]
    &= A.summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= A.help _PROGRAM_ABOUT
    &= A.helpArg [A.explicit, A.name "help", A.name "h"]
    &= A.program _PROGRAM_NAME
