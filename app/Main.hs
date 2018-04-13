{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Protolude
import           Control.Lens (_Just, (^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Vector as Vec
import qualified Data.Default.Class as Def
import qualified Data.ByteString.Lazy as BSL
import           System.FilePath ((</>))
import qualified System.Environment as Env
import qualified System.Directory as Dir
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.STM (atomically, readTVar, writeTVar, newTVar, modifyTVar', TVar)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception.Safe (bracket_)
import qualified Network.HTTP.Req as R
import           Network.HTTP.Req ((/:))


import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.BChan as BCh
import qualified Brick.Focus as BF
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K

import qualified Aws as A

version :: Text
version = "0.0.1.1"

data Event = EventUpdate

data Name = NameInstances
          | NameDetail
          | NameSshSettings
          deriving (Show, Eq, Ord)

data UIState = UIState { _uiFocus :: !(BF.FocusRing Name)
                       , _uiInstances :: !(BL.List Name A.Ec2Instance)
                       , _uiSelectedInstance :: !(Maybe A.Ec2Instance)
                       , _uiIp :: Text
                       , _uiPem :: Text
                       }

makeLenses ''UIState

app :: B.App UIState Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }


main :: IO ()
main = do
  r <- A.fetchInstances
  ip <- getIp

  args <- Env.getArgs
  let pem = fromMaybe "~/.ssh/hyraxbio.pem" $ headMay args

  chan <- BCh.newBChan 5

  -- Construct the initial state values
  let st' = UIState { _uiFocus = BF.focusRing [NameInstances, NameDetail, NameSshSettings]
                    , _uiInstances = BL.list NameInstances (Vec.fromList r) 1 -- Vec.empty 1
                    , _uiSelectedInstance = Nothing
                    , _uiIp = ip
                    , _uiPem = Txt.pack pem
                    }

  let st = st' & uiSelectedInstance .~ (snd <$> BL.listSelectedElement (st' ^. uiInstances))
          
  -- And run brick
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st


handleEvent :: UIState -> B.BrickEvent Name Event -> B.EventM Name (B.Next UIState)
handleEvent st ev =
  case ev of
    -- Handle keyboard events
    --   k is the key
    --   ms are the modifier keys
    (B.VtyEvent ve@(V.EvKey k ms)) ->
      case (k, ms) of
        -- Escape quits the app, no matter what control has focus
        (K.KEsc, []) -> B.halt st

        _ ->
          -- How to interpret the key press depends on which control is focused
          case BF.focusGetCurrent $ st ^. uiFocus of
            Just NameInstances ->
              case k of
                -- K.KChar '\t' ->
                --   B.continue $ st & uiFocus %~ BF.focusNext

                -- K.KBackTab ->
                --   B.continue $ st & uiFocus %~ BF.focusPrev

                K.KEnter ->
                  case st ^. uiSelectedInstance of
                    Nothing -> B.continue st
                    Just selected ->
                      case A.ec2SecurityGroup selected of
                        Nothing -> B.continue st
                        Just (_, sg) -> B.suspendAndResume $ startSsh (st ^. uiPem) (st ^. uiIp) sg selected $> st

                _ -> do
                  r <- BL.handleListEventVi BL.handleListEvent ve $ st ^. uiInstances
                  let selected = snd <$> BL.listSelectedElement r
  
                  B.continue $ st & uiInstances .~ r
                                  & uiSelectedInstance .~ selected

            _ -> B.continue st

      
    _ -> B.continue st



-- | Draw the UI
drawUI :: UIState -> [B.Widget Name]
drawUI st =
  [B.padAll 1 contentBlock] 

  where
    contentBlock =
      (B.hLimit 50 $ block "Instances" instancesList)
      <+>
      rightBlock
      <=>
      (B.txt $ "awssy " <> version)
      
    block n f =
      vtitle n
      <=>
      (B.withBorderStyle BBS.unicode $ BB.border f)

    rightBlock =
      (B.vLimit 20 $ block "Detail" detailBlock)

    detailBlock =
      ( B.txt "Name: "
        <=>
        B.txt "State: "
        <=>
        B.txt "Public DNS: "
        <=>
        B.txt "Public IP: "
        <=>
        B.txt "Instance type: "
        <=>
        B.txt "Zone: "
        <=>
        B.txt "Launch time: "
        <=>
        B.txt "Instance Id: "
        <=>
        B.txt "Security group: "
      )
      <+>
      ( (txt $ maybe "" A.ec2Name (st ^. uiSelectedInstance))
        <=>
        (txt $ maybe "" A.ec2State (st ^. uiSelectedInstance))
        <=>
        (txt $ maybe "" A.ec2PublicDnsName (st ^. uiSelectedInstance))
        <=>
        (txt $ maybe "" A.ec2PublicIpAddress (st ^. uiSelectedInstance))
        <=>
        (txt $ maybe "" A.ec2InstanceType (st ^. uiSelectedInstance))
        <=>
        (txt $ maybe "" A.ec2Placement (st ^. uiSelectedInstance))
        <=>
        (txt $ maybe "" A.ec2LaunchTime (st ^. uiSelectedInstance))
        <=>
        (txt $ maybe "" A.ec2InstanceId (st ^. uiSelectedInstance))
        <=>
        (txt $ maybe "" (maybe "" snd . A.ec2SecurityGroup) (st ^. uiSelectedInstance))
      )
      <+>
      B.fill ' '

    instancesList =
      BL.renderList (\_ e -> B.txt $ A.ec2Name e) False (st ^. uiInstances)

    txt t =
      B.txt $ if Txt.null t then " " else t
  
    htitle t =
      B.hLimit 20 $
      B.withAttr "infoTitle" $
      B.txt t
      
    vtitle t =
      B.withAttr "infoTitle" $
      B.txt t




theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BE.editAttr        , V.black `B.on` V.cyan)
                              , (BE.editFocusedAttr , V.black `B.on` V.yellow)
                              , (BL.listAttr        , V.white `B.on` V.blue)
                              , (BL.listSelectedAttr, V.blue `B.on` V.white)
                              , ("infoTitle"        , B.fg V.cyan)
                              ]

startSsh :: Text -> Text -> Text -> A.Ec2Instance -> IO ()
startSsh pem ip sg inst = 
  bracket_ auth deauth ssh

  where
    auth  =
      exec [ "echo '### connecting to " <> A.ec2Name inst <> " ###'"
           , "echo grant access to: " <> ip
           , "echo aws --region eu-west-1 ec2 authorize-security-group-ingress --group-id " <> sg <> " --protocol tcp --port 22 --cidr " <> ip <> "/24"
           , "aws --region eu-west-1 ec2 authorize-security-group-ingress --group-id " <> sg <> " --protocol tcp --port 22 --cidr " <> ip <> "/24"
           ]

    deauth =
      exec [ "echo remove access from: " <> ip
           , "echo aws --region eu-west-1 ec2 revoke-security-group-ingress --group-id " <> sg <> " --protocol tcp --port 22 --cidr " <> ip <> "/24"
           , "aws --region eu-west-1 ec2 revoke-security-group-ingress --group-id " <> sg <> " --protocol tcp --port 22 --cidr " <> ip <> "/24"
           ]

    ssh = 
      void $ exec [ "echo ssh -i " <> pem <> " ec2-user@" <> A.ec2PublicIpAddress inst <> " -L 8033:localhost:8033"
                  , "ssh -i " <> pem <> " ec2-user@" <> A.ec2PublicIpAddress inst <> " -L 8033:localhost:8033"
                  ]

    exec cmds = A.execWait' "sh" Nothing ["-c", Txt.intercalate "; " cmds]

getIp :: IO Text
getIp = do
  ip <- R.runReq Def.def $ do
    r <- R.req
           R.GET
           (R.http "ipv4bot.whatismyipaddress.com")
           R.NoReqBody
           R.lbsResponse
           mempty

    pure $ R.responseBody r

  pure . TxtE.decodeUtf8 . BSL.toStrict $ ip
