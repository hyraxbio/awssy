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
           , parseInstances
           ) where

import           Protolude
import qualified Data.Text as Txt
import qualified Data.Text.IO as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Aeson as Ae
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified System.IO as IO
import qualified System.IO.Temp as Tmp
import qualified System.Exit as Ex
import qualified System.Process as Proc
import           Control.Lens ((<&>), (^.), (.~), (&), (^..), folded, set, view)
import           Control.Lens.TH (makeLenses)
--import qualified Network.AWS as AWS
import qualified Network.AWS.EC2 as EC2
import qualified Network.AWS.Data as AWS
import qualified Control.Monad.Trans.AWS as AWS

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
                               } deriving (Show, Generic, Ae.FromJSON, Ae.ToJSON)


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


parseInstances :: ByteString -> Either Text [Ec2Instance]
parseInstances b =
  case Ae.eitherDecode (BSL.fromStrict b) :: Either [Char] [Ec2Instance] of
    Left e -> Left $ Txt.pack e
    Right r -> Right r


fetchInstances :: IO (Either Text ([Ec2Instance], BSL.ByteString))
fetchInstances = do
  let region = AWS.Ireland
  lgr <- AWS.newLogger AWS.Debug stdout
  env <- AWS.newEnv AWS.Discover <&> set AWS.envRegion region -- . set AWS.envLogger lgr 

  instances' <- AWS.runResourceT . AWS.runAWST env $ do
    d' <- AWS.trying AWS._Error $ AWS.send EC2.describeInstances
    case d' of
      Left e -> pure . Left $ e
      Right d -> do
        let r = d ^. EC2.dirsReservations
        pure . Right . concat $ r ^.. folded . EC2.rInstances

  case instances' of
    Left e -> pure . Left . show $ e
    Right instances -> do
      let ec2is = mkInstance <$> instances
      pure . Right $ (ec2is, Ae.encode ec2is)

  where
    mkInstance i = 
        Ec2Instance { ec2ImageId = i ^. EC2.insImageId
                    , ec2InstanceId = i ^. EC2.insInstanceId
                    , ec2InstanceType = show $ i ^. EC2.insInstanceType
                    , ec2LaunchTime = show $ i ^. EC2.insLaunchTime --TODO format
                    , ec2SubnetId = fromMaybe "" $ i ^. EC2.insSubnetId
                    , ec2VpcId = fromMaybe "" $ i ^. EC2.insVPCId
                    , ec2Architecture = show $ i ^. EC2.insArchitecture 
                    , ec2PublicDnsName = fromMaybe "" $ i ^. EC2.insPublicDNSName
                    , ec2PublicIpAddress = fromMaybe "" $ i ^. EC2.insPublicIPAddress
                    , ec2Name = getTag "Name" $  i ^. EC2.insTags
                    , ec2Placement = fromMaybe "" $ i ^. EC2.insPlacement . EC2.pAvailabilityZone
                    , ec2State = Txt.drop 3 . show $ i ^. EC2.insState . EC2.isName
                    , ec2SecurityGroup = getSecGroup $ i ^. EC2.insSecurityGroups
                    , ec2PortForwards = []
                    }

    getTag n ts =
      case filter (\t -> t ^. EC2.tagKey == n) ts of
        (t:_) -> t ^. EC2.tagValue
        _ -> ""
        

    getSecGroup gs =
      let
        gs1 = filter (\g -> g ^. EC2.giGroupName /= Just "safety-first") gs <> gs
        gs2 = (\g -> (g ^. EC2.giGroupName, g ^. EC2.giGroupId)) <$> gs1
      in
      case gs2 of
        ((Just n, Just i) : _ ) -> Just (n, i)
        _ -> Nothing


test :: IO [EC2.Reservation]
test = do
  let region = AWS.Ireland
  lgr <- AWS.newLogger AWS.Debug stdout
  env <- AWS.newEnv AWS.Discover <&> set AWS.envRegion region -- . set AWS.envLogger lgr 

  AWS.runResourceT . AWS.runAWST env {- . AWS.within region -} $ do
    r <- AWS.send (EC2.describeInstances)
    liftIO . print $ r ^. EC2.dirsNextToken 
    pure $ r ^. EC2.dirsReservations

