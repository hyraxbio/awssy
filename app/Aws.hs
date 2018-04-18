{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Aws ( Ec2Instance (..)
           , fetchInstances
           , exec
           , execWait
           , exec'
           , execWait'
           ) where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BSL
import qualified System.IO as IO
import qualified System.Exit as Ex
import qualified System.Process as Proc
import           Control.Lens ((^.))
import           Control.Lens.TH (makeLenses)
import           Control.Exception.Safe (throwString)

data Ec2Instance = Ec2Instance { ec2Name :: !Text
                               , ec2ImageId :: !Text
                               , ec2InstanceId :: !Text
                               , ec2InstanceType :: !Text
                               , ec2LaunchTime :: !Text
                               , ec2SubnetId :: !Text
                               , ec2VpcId :: !Text
                               , ec2Architecture :: !Text
                               , ec2PublicDnsName :: !Text
                               , ec2PublicIpAddress :: !Text
                               , ec2Placement :: !Text
                               , ec2State :: !Text
                               , ec2SecurityGroup :: !(Maybe (Text, Text))
                               , ec2PortForwards :: ![(Int, Text, Int)]
                               } deriving (Show)


data Describe = Describe { _dReservations :: [Reservation]
                         } deriving (Generic)

data Reservation = Reservation { _rInstances :: [Instance]
                               } deriving (Generic)

data Instance = Instance { _iImageId :: !Text
                         , _iInstanceId :: !(Maybe Text)
                         , _iInstanceType :: !(Maybe Text)
                         , _iLaunchTime :: !(Maybe Text)
                         , _iSubnetId :: !(Maybe Text)
                         , _iVpcId :: !(Maybe Text)
                         , _iArchitecture :: !(Maybe Text)
                         , _iPublicDnsName :: !(Maybe Text)
                         , _iPublicIpAddress :: !(Maybe Text)
                         , _iPlacement :: !(Maybe Placement)
                         , _iState :: !(Maybe InstanceState)
                         , _iTags :: ![Tag]
                         , _iSecurityGroups :: ![SecurityGroup]
                         } deriving (Generic)

data Placement = Placement { _pAvailabilityZone :: !Text
                           } deriving (Generic)

data InstanceState = InstanceState { _sName :: !Text
                                   } deriving (Generic)

data Tag = Tag { _tKey :: !Text
               , _tValue :: !Text
               } deriving (Generic)

data SecurityGroup = SecurityGroup { _sGroupName :: !Text
                                   , _sGroupId :: !Text
                                   } deriving (Generic)

makeLenses ''Describe
makeLenses ''Reservation
makeLenses ''Instance
makeLenses ''Placement
makeLenses ''InstanceState
makeLenses ''Tag
makeLenses ''SecurityGroup

instance Ae.FromJSON Describe where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 False }

instance Ae.FromJSON Reservation where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 False }

instance Ae.FromJSON Instance where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 False }

instance Ae.FromJSON Placement where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 False }

instance Ae.FromJSON InstanceState where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 False }

instance Ae.FromJSON Tag where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 False }

instance Ae.FromJSON SecurityGroup where
  parseJSON = Ae.genericParseJSON Ae.defaultOptions { Ae.fieldLabelModifier = renField 2 False }
 

renField :: Int -> Bool -> [Char] -> [Char]
renField drp toLower =
  Txt.unpack . (if toLower then mkLower else identity) . Txt.drop drp . Txt.pack
  where
    mkLower t = Txt.toLower (Txt.take 1 t) <> Txt.drop 1 t

  
execWait :: FilePath -> Maybe FilePath -> Maybe [(Text, Text)] -> [Text] -> IO (ExitCode, Text, Text)
execWait bin cwd env args = do
  (outp, errp, phandle) <- exec bin cwd env args
  exitCode <- Proc.waitForProcess phandle
  stdout' <- IO.hGetContents outp
  stderr' <- IO.hGetContents errp
  pure (exitCode, Txt.pack stdout', Txt.pack stderr')
  

exec :: FilePath -> Maybe FilePath -> Maybe [(Text, Text)] -> [Text] -> IO (Handle, Handle, Proc.ProcessHandle)
exec bin cwd env args = do
  let p' = Proc.proc bin $ Txt.unpack <$> args
  let p = p' { Proc.env = (\(k, v) -> (Txt.unpack k, Txt.unpack v)) <<$>> env
             , Proc.std_out = Proc.CreatePipe
             , Proc.std_err = Proc.CreatePipe
             , Proc.cwd = cwd
             }

  (_, Just outp, Just errp, phandle) <- Proc.createProcess p
  pure (outp, errp, phandle)



execWait' :: FilePath -> Maybe FilePath -> Maybe [(Text, Text)] -> [Text] -> IO ExitCode
execWait' bin cwd env args = do
  phandle <- exec' bin cwd env args
  Proc.waitForProcess phandle
  

exec' :: FilePath -> Maybe FilePath -> Maybe [(Text, Text)] -> [Text] -> IO Proc.ProcessHandle
exec' bin cwd env args = do
  let p' = Proc.proc bin $ Txt.unpack <$> args
  let p = p' { Proc.env = (\(k, v) -> (Txt.unpack k, Txt.unpack v)) <<$>> env
             , Proc.cwd = cwd
             }

  (_, _, _, phandle) <- Proc.createProcess p
  pure phandle



fetchInstances :: IO [Ec2Instance]
fetchInstances = do
  (x, o, err) <- execWait "sh" Nothing Nothing ["-c", "r=$(aws ec2 describe-instances); echo $r"]
  let j = BSL.fromStrict . TxtE.encodeUtf8 $ o

  case x of
    Ex.ExitSuccess ->
      case Ae.eitherDecode j :: Either [Char] Describe of
        Left e -> throwString e
        Right r -> do
          let e = concat $ fromReservation <$> (r ^. dReservations)
          pure $ sortOn (Txt.toUpper . ec2Name) e
  
    _ -> throwString $ "aws failed" <> Txt.unpack err

  where
    fromReservation r = 
      fromInstance <$> (r ^. rInstances)

    fromInstance i =
      Ec2Instance { ec2ImageId = i ^. iImageId
                  , ec2InstanceId = fromMaybe "" $ i ^. iInstanceId
                  , ec2InstanceType = fromMaybe "" $ i ^. iInstanceType
                  , ec2LaunchTime = fromMaybe "" $ i ^. iLaunchTime
                  , ec2SubnetId = fromMaybe "" $ i ^. iSubnetId
                  , ec2VpcId = fromMaybe "" $ i ^. iVpcId
                  , ec2Architecture = fromMaybe "" $ i ^. iArchitecture 
                  , ec2PublicDnsName = fromMaybe "" $ i ^. iPublicDnsName
                  , ec2PublicIpAddress = fromMaybe "" $ i ^. iPublicIpAddress
                  , ec2Name = getTag "Name" $ i ^.iTags
                  , ec2Placement = maybe "" (^. pAvailabilityZone) $ i ^. iPlacement
                  , ec2State = maybe "" (^. sName) $ i ^. iState
                  , ec2SecurityGroup = getSecGroup $ i ^. iSecurityGroups
                  , ec2PortForwards = []
                  }

    getTag n ts =
      maybe "" (^. tValue) (headMay $ filter (\x -> x ^. tKey == n) ts)


    getSecGroup gs =
      let
        gs1 = filter (\g -> g ^. sGroupName /= "safety-first") gs
        g' = headMay gs1
      in
      case g' of
        Nothing -> Nothing
        Just g -> Just (g ^. sGroupName, g ^. sGroupId)
