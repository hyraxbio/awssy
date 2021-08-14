{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Protolude
import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.AttrMap as BA
import qualified Brick.Focus as BF
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.List as BL
import           Control.Lens (makeLenses, traversed, filtered, at, ix, (^.), (?~), (^?), (^..), (%~), (.~))
import qualified Data.Aeson as Ae
import qualified Data.Aeson.Encode.Pretty as Ae
import           Data.Aeson.Lens (key, _Array, _Object, _String, _Number)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Txt
import qualified Data.UUID as UU
import qualified Data.Vector as Vec
import qualified Data.Version as Ver
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K
import qualified System.Clipboard as Clp
import qualified System.Process.Typed as Pt

import qualified BrickBedrock.Core as Bb
import qualified BrickBedrock.Defaults as Bbd
import qualified BrickBedrock.Model as Bb

import qualified Args
import qualified Paths_awssy as Paths

-- Make the code a bit easier to read, but I don't really like aliases...
type Event' = Bb.Event AwState AwPopup AwWindow AwName AwEvent
type UIState' = Bb.UIState AwState AwPopup AwWindow AwName AwEvent
type Window' = Bb.Window AwState AwPopup AwWindow AwName AwEvent
type Popup' = Bb.Popup AwState AwPopup AwWindow AwName AwEvent
type Name' = Bb.Name AwName
type WindowReg' = Bb.WindowReg AwState AwPopup AwWindow AwName AwEvent
type PopupReg' = Bb.PopupReg AwState AwPopup AwWindow AwName AwEvent
type PendingAction' = Bb.PendingAction AwState AwPopup AwWindow AwName AwEvent

data AwState = AwState
  { _usInstances :: !(BL.List Name' Ae.Value)
  , _usFocus :: !(BF.FocusRing Name')
  }

data AwPopup
  = PopDemo

data AwWindow
  = WMain

data AwName
  = NameInstances
  deriving (Show, Eq, Ord)

data AwEvent

makeLenses ''AwState


main :: IO ()
main = Args.runArgs run


run :: Args.Args -> IO ()
run args = do
  let uiinit = Bbd.defaultInit
       { Bb._uioStartWindow = regWindowInstances
       , Bb._uioAppName = "Awssy"
       , Bb._uioAppVersion = Txt.pack $ Ver.showVersion Paths.version
       , Bb._uioUserAttrs = gAttrs
       , Bb._uioAppInit = loadApp
       , Bb._uioHelpPopupReg = popupRegHelp
       }

  let ust = AwState
       { _usInstances = BL.list (nm NameInstances) Vec.empty 1
       , _usFocus = BF.focusRing [nm NameInstances]
       }

  Bb.runTui uiinit ust


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

  --      (K.KEnter, []) -> do
  --        st2 <- liftIO $ loadWorkers st
  --        B.continue st2

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
        (titleTxt ": " <+> dullTxt (maybe "" (show . round) (inst ^? key "CpuOptions" . key "CoreCount" . _Number)))
        <=>
        (titleTxt ": " <+> dullTxt (maybe "" (show . round) (inst ^? key "CpuOptions" . key "ThreadsPerCore" . _Number)))
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
    Bb.sendStatusMessage st Bb.StsTrace ("Loaded instances: " <> show (length is)) Nothing
    pure ( \stx -> stx & Bb.uiSt . usInstances .~ BL.list (nm NameInstances) (Vec.fromList is) 1
         , []
         )
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
    -- , B.withAttr "titleText" (B.txt "  <control> r") <+> B.withAttr "" (B.txt " - Refresh data")
    -- , B.withAttr "titleText" (B.txt "  F5") <+> B.withAttr "" (B.txt " - Refresh data")
    , B.withAttr "titleText" (B.txt "  F12") <+> B.withAttr "" (B.txt " - Error log")
    , B.withAttr "titleText" (B.txt "  ESC") <+> B.withAttr "" (B.txt " - Back")
    , B.vLimit 2 . B.hLimit 1 $ B.fill ' '
    , B.withAttr "infoTitle" . B.txt $ "- Instances -"
    , B.withAttr "titleText" (B.txt "  i") <+> B.withAttr "" (B.txt " - View instance JSON")
    , B.withAttr "titleText" (B.txt "  c") <+> B.withAttr "" (B.txt " - Copy instance JSON to clipboard")
    -- , B.withAttr "titleText" (B.txt "  Enter") <+> B.withAttr "" (B.txt " - View job's workers")
    , B.vLimit 40 . B.hLimit 120 $ B.fill ' '
    ]
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- AWS
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
awsGetInstances :: IO [Ae.Value]
awsGetInstances = do
  (js,_) <- Pt.readProcess_ "aws ec2 describe-instances"
  let
    -- All instances that are not stopped
    instances' = js ^.. key "Reservations" . _Array . traversed . key "Instances" . _Array . traversed . filtered (\i -> i ^? key "State" . key "Name" . _String /= Just "stopped")
    -- Add an "__InstanceName" property
    instances = instances' & traversed . _Object %~ \i ->
      let keyName = fromMaybe Ae.Null . headMay $ i ^.. ix "Tags" . _Array . traversed . filtered (\t -> t ^? key "Key" . _String == Just "Name") in
      i & at "__InstanceName" .~ (keyName ^? key "Value")

  pure instances
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


nm :: AwName -> Name'
nm = Bb.NameUser
