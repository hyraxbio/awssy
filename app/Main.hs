{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Protolude hiding (bracket_, finally)
import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.AttrMap as BA
import qualified Brick.BChan as BCh
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import           Control.Exception.Safe (finally)
import           Control.Lens (makeLenses, traversed, filtered, _Just, at, ix, (^.), (?~), (^?), (^..), (%~), (.~))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Encode.Pretty as Ae
import           Data.Aeson.Lens (key, _Array, _Object, _String, _Number)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.UUID as UU
import qualified Data.UUID.V4 as UU
import qualified Data.Vector as Vec
import qualified Data.Version as Ver
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K
import qualified Network.HTTP.Req as R
import qualified System.Clipboard as Clp
import qualified System.Environment as Env
import qualified System.Exit as Ex
import qualified System.Process.Typed as Pt

import qualified BrickBedrock.Core as Bb
import qualified BrickBedrock.Defaults as Bbd
import qualified BrickBedrock.Model as Bb

import qualified Args
import qualified Settings as S
import qualified Paths_awssy as Paths

-- Make the code a bit easier to read, but I don't really like aliases...
type Event' = Bb.Event AwState AwPopup AwWindow AwName AwEvent
type UIState' = Bb.UIState AwState AwPopup AwWindow AwName AwEvent
type Window' = Bb.Window AwState AwPopup AwWindow AwName AwEvent
type Popup' = Bb.Popup AwState AwPopup AwWindow AwName AwEvent
type Name' = Bb.Name AwName
--type WindowReg' = Bb.WindowReg AwState AwPopup AwWindow AwName AwEvent
type PopupReg' = Bb.PopupReg AwState AwPopup AwWindow AwName AwEvent
type PendingAction' = Bb.PendingAction AwState AwPopup AwWindow AwName AwEvent

data AwState = AwState
  { _usInstances :: !(BL.List Name' Ae.Value)
  , _usFocus :: !(BF.FocusRing Name')
  , _usIp :: !Text
  , _usArgs :: !Args.Args
  , _usSettings :: !(Maybe S.Settings)
  }

data AwPopup

data AwWindow
  = WMain

data AwName
  = NameInstances
  deriving (Show, Eq, Ord)

data AwEvent
  = EvtRefresh

makeLenses ''AwState


main :: IO ()
main = Args.runArgs run


run :: Args.Args -> IO ()
run args = do
  settings <- S.readSettings

  let uiinit = Bbd.defaultInit
       { Bb._uioStartWindow = regWindowInstances
       , Bb._uioAppName = "Awssy"
       , Bb._uioAppVersion = Txt.pack $ Ver.showVersion Paths.version
       , Bb._uioUserAttrs = gAttrs
       , Bb._uioAppInit = loadApp
       , Bb._uioAppPreInit = startUpdateTimer
       , Bb._uioHelpPopupReg = popupRegHelp
       , Bb._uioHandleUserEvents = handleEvents
       }

  let ust = AwState
       { _usInstances = BL.list (nm NameInstances) Vec.empty 1
       , _usFocus = BF.focusRing [nm NameInstances]
       , _usIp = ""
       , _usArgs = args
       , _usSettings = settings
       }

  Bb.runTui uiinit ust

  where
    startUpdateTimer st = do
      void . liftIO . forkIO $
        forever $ do
          threadDelay (5 * 60 * 1_000_000)  -- 5 min
          BCh.writeBChan (st ^. Bb.uiChan) $ evt EvtRefresh

      pure st

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
regWindowInstances :: Window'
regWindowInstances =
  Bb.WUser WMain "EC2 Instances" . Just $
    Bb.WindowReg
      { Bb._wrDraw = const drawInstances
      , Bb._wrEventHandler = const keyHandlerInstances
      }


keyHandlerInstances :: UIState' -> B.BrickEvent Name' Event' -> B.EventM Name' (B.Next UIState')
keyHandlerInstances st ev =
  case ev of
    (B.VtyEvent ve@(V.EvKey k ms)) ->
      case (k, ms) of
        (K.KChar 'c', []) -> do
          case snd <$> BL.listSelectedElement (st ^. Bb.uiSt . usInstances) of
            Nothing -> B.continue st
            Just vs -> do
              let s = Ae.encodePretty vs
              liftIO . Clp.setClipboardString . BS8.unpack . BSL.toStrict $ s
              B.continue st

        (K.KChar 'i', []) -> do
          case snd <$> BL.listSelectedElement (st ^. Bb.uiSt . usInstances) of
            Nothing -> B.continue st
            Just vs -> do
              let s = Txt.pack . BS8.unpack . BSL.toStrict . Ae.encodePretty $ vs
              B.continue $ st & Bb.uiPopup ?~ Bb.popTextForText s
                              & Bb.uiPopText .~ BE.editorText Bb.NamePopTextEdit Nothing s

        (K.KChar 'r', [V.MCtrl]) -> syncRefreshInstances st
        (K.KFun 5, []) -> syncRefreshInstances st

        (K.KChar 's', []) -> do
          st2 <- liftIO $ startShell st
          B.continue st2

        (K.KEnter, []) -> do
          st2 <- liftIO $ startSsh st
          B.continue st2

        _ -> do
          r <- BL.handleListEventVi BL.handleListEvent ve $ st ^. Bb.uiSt . usInstances
          B.continue $ st & Bb.uiSt . usInstances .~ r

    _ -> B.continue st


drawInstances :: UIState' -> B.Widget Name'
drawInstances st =
   B.hLimit 50 (block "Instances" instancesList) <+> B.vLimit 40 (block "Detail" detailBlock)

  where
    instancesList =
      BL.renderList (\_ e -> instanceName e) (BF.focusGetCurrent (st ^. Bb.uiSt . usFocus) == Just (nm NameInstances)) (st ^. Bb.uiSt . usInstances)

    instanceName e =
      let
        instName = fromMaybe "?" $ e ^? key "__InstanceName" . _String
        stsName = fromMaybe "?" $ e ^? key "State" . key "Name" . _String
        status =
          case stsName of
            "pending"       -> "P "
            "running"       -> "+ "
            "shutting-down" -> "D "
            "stopped"       -> "- "
            "stopping"      -> "S "
            "terminated"    -> "x "
            _               -> "? "
      in
      B.withAttr (BA.attrName . Txt.unpack $ "status_" <> stsName) (B.txt status) <+> B.txt instName

    detailBlock =
      let
        inst = maybe Ae.Null snd (BL.listSelectedElement (st ^. Bb.uiSt . usInstances))
      in
      ( titleTxt "Name"
        <=>
        titleTxt "State"
        <=>
        titleTxt "Public DNS"
        <=>
        titleTxt "Public IP"
        <=>
        titleTxt "Instance type"
        <=>
        titleTxt "Zone"
        <=>
        titleTxt "Launch time"
        <=>
        titleTxt "Instance Id"
        <=>
        titleTxt "CPU Cores"
        <=>
        titleTxt "Threads per core"
      )
      <+>
      ( (titleTxt ": " <+> dullTxt (fromMaybe "" (inst ^? key "__InstanceName" . _String)))
        <=>
        (titleTxt ": " <+> dullTxt (fromMaybe "" (inst ^? key "State" . key "Name" . _String)))
        <=>
        (titleTxt ": " <+> dullTxt (fromMaybe "" (inst ^? key "PublicDnsName" . _String)))
        <=>
        (titleTxt ": " <+> dullTxt (fromMaybe "" (inst ^? key "PublicIpAddress" . _String)))
        <=>
        (titleTxt ": " <+> dullTxt (fromMaybe "" (inst ^? key "InstanceType" . _String)))
        <=>
        (titleTxt ": " <+> dullTxt (fromMaybe "" (inst ^? key "Placement" . key "AvailabilityZone" . _String)))
        <=>
        (titleTxt ": " <+> dullTxt (fromMaybe "" (inst ^? key "LaunchTime" . _String)))
        <=>
        (titleTxt ": " <+> dullTxt (fromMaybe "" (inst ^? key "InstanceId" . _String)))
        <=>
        (titleTxt ": " <+> dullTxt (maybe "" (show . round @_ @Int) (inst ^? key "CpuOptions" . key "CoreCount" . _Number)))
        <=>
        (titleTxt ": " <+> dullTxt (maybe "" (show . round @_ @Int) (inst ^? key "CpuOptions" . key "ThreadsPerCore" . _Number)))
      )
      <+>
      B.fill ' '

    titleTxt t =
      B.withAttr "titleText" $ txt t

    dullTxt t =
      B.withAttr "normalText" $ txt t

    txt t =
      B.txt $ if Txt.null t then " " else t

    block n f =
      vtitle n
      <=>
      B.withBorderStyle BBS.unicode (BB.border f)

    vtitle t =
      B.withAttr "infoTitle" $
      B.txt t
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Events
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
handleEvents :: AwEvent -> UIState' -> B.EventM Name' (B.Next UIState')
handleEvents v st =
  case v of
    EvtRefresh -> do
      liftIO $ do
        id <- UU.nextRandom
        Bb.addAsyncAction st . Bb.PendingAction id "instance.refresh.async" $ do
          is <- awsGetInstances
          liftIO $ Bb.sendStatusMessage st Bb.StsTrace "Instances refreshed (async)" Nothing
          pure (\stx -> B.continue $ updateInstances stx is)

      B.continue st
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Load and update
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
loadApp :: UIState' -> PendingAction'
loadApp st =
  let
    id = UU.fromWords 2500037091 603082674 2988119275 3335668075
    name = "init"
  in
  Bb.PendingAction id name $ do
    is <- awsGetInstances
    ip <- getIp
    Bb.sendStatusMessage st Bb.StsInfo ("Loaded instances: " <> show (length is)) Nothing
    pure (\stx -> B.continue $ stx & Bb.uiSt . usInstances .~ BL.list (nm NameInstances) (Vec.fromList is) 1
                                   & Bb.uiSt . usIp .~ ip
         )


updateInstances :: UIState' -> [Ae.Value] -> UIState'
updateInstances st1 is = do
  let
    oldInstId =
      case snd <$> BL.listSelectedElement (st1 ^. Bb.uiSt . usInstances) of
        Nothing -> Nothing
        Just vs -> vs ^? key "InstanceId" . _String

    lst1 = BL.list (nm NameInstances) (Vec.fromList is) 1
    lst2 = BL.listFindBy (\v -> v ^? key "InstanceId" . _String == oldInstId) lst1
    st2 = st1 & Bb.uiSt . usInstances .~ lst2

  st2


syncRefreshInstances :: UIState' -> B.EventM Name' (B.Next UIState')
syncRefreshInstances st = do
  id <- liftIO UU.nextRandom
  liftIO . Bb.addBlockingAction st . Bb.PendingAction id "instance.refresh.async" $ do
    is <- awsGetInstances
    liftIO $ Bb.sendStatusMessage st Bb.StsTrace "Instances refreshed (sync)" Nothing
    pure (\stx -> B.continue $ updateInstances stx is)

  B.continue st
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Help
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
popupRegHelp :: PopupReg'
popupRegHelp =
  Bb.PopupReg
    { Bb._prDraw = drawPopupHelp
    , Bb._prEventHandler = const Bb.nopKeyHandler
    }


drawPopupHelp :: Popup' -> UIState' -> B.Widget Name'
drawPopupHelp _pop _st =
  B.vBox
    [ B.withAttr "infoTitle" . B.txt $ "- Global -"
    , B.withAttr "titleText" (B.txt "  <control> q") <+> B.withAttr "" (B.txt " - Quit")
    , B.withAttr "titleText" (B.txt "  <control> ?") <+> B.withAttr "" (B.txt " - Help")
    , B.withAttr "titleText" (B.txt "  F1") <+> B.withAttr "" (B.txt " - Help")
    , B.withAttr "titleText" (B.txt "  <control> r") <+> B.withAttr "" (B.txt " - Refresh data")
    , B.withAttr "titleText" (B.txt "  F5") <+> B.withAttr "" (B.txt " - Refresh data")
    , B.withAttr "titleText" (B.txt "  F12") <+> B.withAttr "" (B.txt " - Error log")
    , B.withAttr "titleText" (B.txt "  ESC") <+> B.withAttr "" (B.txt " - Back")
    , B.vLimit 2 . B.hLimit 1 $ B.fill ' '
    , B.withAttr "infoTitle" . B.txt $ "- Instances -"
    , B.withAttr "titleText" (B.txt "  i") <+> B.withAttr "" (B.txt " - View instance JSON")
    , B.withAttr "titleText" (B.txt "  c") <+> B.withAttr "" (B.txt " - Copy instance JSON to clipboard")
    , B.vLimit 40 . B.hLimit 120 $ B.fill ' '
    ]
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AWS
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
awsGetInstances :: IO [Ae.Value]
awsGetInstances = do
  (js,_) <- Pt.readProcess_ "aws ec2 describe-instances --output json"
  let
    -- All instances that are not stopped
    instances1 = js ^.. key "Reservations" . _Array . traversed . key "Instances" . _Array . traversed -- . filtered (\i -> i ^? key "State" . key "Name" . _String /= Just "stopped")
    -- Add an "__InstanceName" property
    instances2 = instances1 & traversed . _Object %~ \i ->
      let keyName = fromMaybe Ae.Null . headMay $ i ^.. ix "Tags" . _Array . traversed . filtered (\t -> t ^? key "Key" . _String == Just "Name") in
      i & at "__InstanceName" .~ (keyName ^? key "Value")
    instances3 = sortOn (\v -> v ^? key "__InstanceName" . _String) instances2

  pure instances3
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Custom attributes
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gAttrs :: [(BA.AttrName, V.Attr)]
gAttrs =
  [ ("status_terminated"       , B.fg V.yellow)
  , ("status_running"          , B.fg V.brightGreen)
  , ("status_pending"          , B.fg V.green)
  , ("status_shutting-down"    , B.fg V.magenta)
  , ("status_stopped"          , B.fg V.brightRed)
  , ("status_stopping"         , B.fg V.magenta)
  , ("spinnerText"             , B.fg V.brightMagenta)
  ]
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Shell
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
startSsh :: UIState' -> IO UIState'
startSsh st =
  case snd <$> BL.listSelectedElement (st ^. Bb.uiSt . usInstances) of
    Nothing -> pure st
    Just vs -> do
      let name = vs ^. key "__InstanceName" . _String
      aid <- UU.nextRandom
      Bb.addBlockingAction st . Bb.PendingAction aid "ssh" $ do
        allowSshIngress st

        let echos =
             [ "clear"
             , "echo -e \"\\e]0;AWSSY: ssh" <> name <> "\\007\""
             , "echo ''"
             ]

        let ssh =
             Pt.proc
               "ssh"
               [ "-i", getKeyFile name (st ^. Bb.uiSt)
               , Txt.unpack $ getUser name (st ^. Bb.uiSt) <> "@" <> fromMaybe "" (vs ^? key "PublicIpAddress" . _String)
               ]

        pure (\stx -> B.suspendAndResume $ do
                -- reject ssh ingress rule after delay to connect. SG rules are stateful so existing connection will continue to work
                void . forkIO $ do
                  threadDelay $ 10 * 1_000_000
                  rejectSshIngress stx

                finally
                  (do
                    void $ Pt.runProcess . Pt.shell . Txt.unpack $ Txt.intercalate ";" echos
                    _ <- Pt.runProcess ssh
                    void $ Pt.runProcess. Pt.shell $ "echo -e \"\\e]0;AWSSY\\007\""
                    pure stx
                  )
                  (rejectSshIngress stx)
             )
      pure st


startShell :: UIState' -> IO UIState'
startShell st =
  case snd <$> BL.listSelectedElement (st ^. Bb.uiSt . usInstances) of
    Nothing -> pure st
    Just vs -> do
      let name = vs ^. key "__InstanceName" . _String

      aid <- UU.nextRandom
      Bb.addBlockingAction st . Bb.PendingAction aid "ssh" $ do
        allowSshIngress st
        let userName = getUser name (st ^. Bb.uiSt)
        let keyFile = getKeyFile name (st ^. Bb.uiSt)
        let echos =
             [ "clear"
             , "echo -e \"\\e]0;AWSSY - shell: " <> name <> "\\007\""
             , "echo ''"
             , "echo 'Shell for: ### " <> userName <> " ###'"
             , "echo '  AWS_H: " <> fromMaybe "" (vs ^? key "PublicIpAddress" . _String) <> "'"
             , "echo '  AWS_U: " <> userName <> "'"
             , "echo '  AWS_K: " <> Txt.pack keyFile <> "'"
             , "echo '  AWS_UH: " <> userName <> "@" <> fromMaybe "" (vs ^? key "PublicIpAddress" . _String) <> "'"
             , "echo '  e.g. scp -i $AWS_K ./README.md $AWS_UH:/home/$AWS_U/README.md'"
             , "echo ''"
             ]

        let env' = Map.fromList
             [ ("AWS_H", Txt.unpack $ fromMaybe "" (vs ^? key "PublicIpAddress" . _String))
             , ("AWS_U", Txt.unpack userName)
             , ("AWS_K", keyFile)
             , ("AWS_UH", Txt.unpack $ userName <> "@" <> fromMaybe "" (vs ^? key "PublicIpAddress" . _String))
             ]

        currentEnv <- Map.fromList <$> Env.getEnvironment
        let env = Map.toList $ Map.union env' currentEnv

        let sh = Map.findWithDefault "sh" "SHELL" currentEnv
        let shell = Pt.setEnv env $ Pt.proc sh []

        pure (\stx -> B.suspendAndResume $ do
                finally
                  (do
                     void $ Pt.runProcess . Pt.shell . Txt.unpack $ Txt.intercalate ";" echos
                     _ <- Pt.runProcess shell
                     void $ Pt.runProcess. Pt.shell $ "echo -e \"\\e]0;AWSSY\\007\""
                     pure stx
                  )
                  (rejectSshIngress stx)
             )
      pure st


rejectSshIngress :: UIState' -> IO ()
rejectSshIngress st = do
  let ip = st ^. Bb.uiSt . usIp
  case snd <$> BL.listSelectedElement (st ^. Bb.uiSt . usInstances) of
    Nothing -> pass
    Just vs -> do
      let name = vs ^. key "__InstanceName" . _String
      case lastMay $ vs ^.. key "SecurityGroups" . _Array . traversed . key "GroupId" . _String of
        Nothing -> pass
        Just sg -> do
          let args = Txt.unpack <$>
                  [ "ec2"
                  , "revoke-security-group-ingress"
                  , "--group-id"
                  , sg
                  , "--protocol"
                  , "tcp"
                  , "--port"
                  , "22"
                  , "--cidr"
                  , ip <> "/32"
                  ]

          (ex, sout, serr) <- Pt.readProcess . Pt.proc "aws" $ args

          if ex /= Ex.ExitSuccess
            then Bb.sendStatusMessage st Bb.StsError ("Reject ingress rule failed for " <> name <> ", group=" <> sg) (Just $ (show args <> "\n\n" <> (Txt.pack . BS8.unpack . BSL.toStrict $ sout)) <> "\n\n" <> (Txt.pack . BS8.unpack . BSL.toStrict $ serr))
            else Bb.sendStatusMessage st Bb.StsTrace ("Reject ingress rule succeeded for " <> name <> ", group=" <> sg) (Just $ (Txt.pack . BS8.unpack . BSL.toStrict $ sout) <> "\n\n" <> (Txt.pack . BS8.unpack . BSL.toStrict $ serr))



allowSshIngress :: UIState' -> IO ()
allowSshIngress st = do
  let ip = st ^. Bb.uiSt . usIp
  case snd <$> BL.listSelectedElement (st ^. Bb.uiSt . usInstances) of
    Nothing -> pass
    Just vs -> do
      let name = vs ^. key "__InstanceName" . _String
      case lastMay $ vs ^.. key "SecurityGroups" . _Array . traversed . key "GroupId" . _String of
        Nothing -> pass
        Just sg -> do
            let args = Txt.unpack <$>
                  [ "ec2"
                  , "authorize-security-group-ingress"
                  , "--group-id"
                  , sg
                  , "--protocol"
                  , "tcp"
                  , "--port"
                  , "22"
                  , "--cidr"
                  , ip <> "/32"
                  ]
            (ex, sout, serr) <- Pt.readProcess . Pt.proc "aws" $ args

            if ex /= Ex.ExitSuccess
              then Bb.sendStatusMessage st Bb.StsError ("Ingress rule failed for " <> name <> ", group=" <> sg) (Just $ (show args <> "\n\n" <> (Txt.pack . BS8.unpack . BSL.toStrict $ sout)) <> "\n\n" <> (Txt.pack . BS8.unpack . BSL.toStrict $ serr))
              else Bb.sendStatusMessage st Bb.StsTrace ("Ingress rule succeeded for " <> name <> ", group=" <> sg) (Just $ (Txt.pack . BS8.unpack . BSL.toStrict $ sout) <> "\n\n" <> (Txt.pack . BS8.unpack . BSL.toStrict $ serr))


getIp :: IO Text
getIp = do
  ip <- R.runReq R.defaultHttpConfig $ do
    r <- R.req
           R.GET
           (R.http "ifconfig.co")
           R.NoReqBody
           R.lbsResponse
           (R.header "Accept" "text/plain")

    pure $ R.responseBody r

  pure . Txt.strip . TxtE.decodeUtf8 . BSL.toStrict $ ip


getUser :: Text -> AwState -> Text
getUser hostName st =
  let def = st ^. usArgs . Args.aUser in
  case st ^? usSettings . _Just . S.sHosts . ix hostName of
    Just h -> fromMaybe def $ h ^. S.hUser
    _ ->  def


getKeyFile :: Text -> AwState -> FilePath
getKeyFile hostName st =
  let def = st ^. usArgs . Args.aKeyFile in
  case st ^? usSettings . _Just . S.sHosts . ix hostName of
    Just h -> fromMaybe def $ h ^. S.hkeyFile
    _ ->  def
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


evt :: AwEvent -> Event'
evt = Bb.EvtUser

nm :: AwName -> Name'
nm = Bb.NameUser
