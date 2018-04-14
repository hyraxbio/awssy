{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Protolude
import           Control.Lens (_Just, (%~), (^.), (.~), (?~))
import           Control.Lens.TH (makeLenses)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as Lst
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.Aeson as Ae
import qualified Data.Vector as Vec
import qualified Data.Default.Class as Def
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified Network.HTTP.Req as R

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
version = "0.0.2.0"

data Event = EventUpdate [A.Ec2Instance]
           | EventStatus Text

data Name = NameInstances
          | NameDetail
          | NameForwardsList
          | NameForwardLocalPort
          | NameForwardRemotePort
          | NameForwardRemoteHost
          | NameButtonAdd
          deriving (Show, Eq, Ord)

data UIState = UIState { _uiFocus :: !(BF.FocusRing Name)
                       , _uiInstances :: !(BL.List Name A.Ec2Instance)
                       , _uiForwards :: !(BL.List Name (Int, Text, Int))
                       , _uiSelectedInstance :: !(Maybe A.Ec2Instance)
                       , _uiEditForwardLocalPort :: !(BE.Editor Text Name)
                       , _uiEditForwardRemoteHost :: !(BE.Editor Text Name)
                       , _uiEditForwardRemotePort :: !(BE.Editor Text Name)
                       , _uiIp :: Text
                       , _uiPem :: Text
                       , _uiAddError :: Text
                       , _uiStatus :: Text
                       , _uiFnUpdate :: IO ()
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
  r <- applySettings =<< pure [] --A.fetchInstances
  ip <- getIp

  args <- Env.getArgs
  let pem = fromMaybe "~/.ssh/hyraxbio.pem" $ headMay args

  chan <- BCh.newBChan 5

  let updateFromAws = do
        BCh.writeBChan chan $ EventStatus "fetching from aws..."
        is <- A.fetchInstances
        BCh.writeBChan chan $ EventUpdate is
        BCh.writeBChan chan $ EventStatus ""

  -- Construct the initial state values
  let st1 = UIState { _uiFocus = BF.focusRing [NameInstances, NameForwardsList, NameForwardLocalPort, NameForwardRemoteHost, NameForwardRemotePort, NameButtonAdd]
                    , _uiInstances = BL.list NameInstances (Vec.fromList r) 1
                    , _uiForwards = BL.list NameForwardsList Vec.empty 1
                    , _uiSelectedInstance = Nothing
                    , _uiEditForwardLocalPort = BE.editor NameForwardLocalPort (Just 1) ""
                    , _uiEditForwardRemotePort = BE.editor NameForwardRemotePort (Just 1) ""
                    , _uiEditForwardRemoteHost = BE.editor NameForwardRemoteHost (Just 1) "localhost"
                    , _uiIp = ip
                    , _uiPem = Txt.pack pem
                    , _uiAddError = ""
                    , _uiStatus = ""
                    , _uiFnUpdate = updateFromAws
                    }

  let st2 = st1 & uiSelectedInstance .~ (snd <$> BL.listSelectedElement (st1 ^. uiInstances))
  let st3 = st2 & uiForwards .~ BL.list NameForwardsList (Vec.fromList $ maybe [] (\(_, e) -> A.ec2PortForwards e) (BL.listSelectedElement $ st2 ^. uiInstances)) 1

  void . forkIO $ forever $ do
    updateFromAws
    threadDelay $ 1000000 * 60 * 5
          
  -- Run brick
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st3


handleEvent :: UIState -> B.BrickEvent Name Event -> B.EventM Name (B.Next UIState)
handleEvent st ev =
  case ev of
    -- Handle keyboard events
    --   k is the key
    --   ms are the modifier keys
    (B.VtyEvent ve@(V.EvKey k ms)) ->
      case (k, ms) of
        -- Escape quits the app, no matter what control has focus
        (K.KEsc, []) -> do
          liftIO $ saveSettings st
          B.halt st

        (K.KFun 5, []) -> do
          liftIO $ void . forkIO $ st ^. uiFnUpdate
          B.continue st

        _ ->
          -- How to interpret the key press depends on which control is focused
          case BF.focusGetCurrent $ st ^. uiFocus of
            Just NameInstances ->
              case k of
                K.KChar '\t' -> B.continue $ st & uiFocus %~ BF.focusNext
                K.KBackTab -> B.continue $ st & uiFocus %~ BF.focusPrev

                K.KEnter ->
                  case st ^. uiSelectedInstance of
                    Nothing -> B.continue st
                    Just selected ->
                      case A.ec2SecurityGroup selected of
                        Nothing -> B.continue st
                        Just (_, sg) -> B.suspendAndResume $  saveSettings st
                                                           >> startSsh (st ^. uiPem) (st ^. uiIp) sg selected
                                                           >> pure st


                K.KChar 's' ->
                  case st ^. uiSelectedInstance of
                    Nothing -> B.continue st
                    Just selected ->
                      case A.ec2SecurityGroup selected of
                        Nothing -> B.continue st
                        Just (_, sg) -> B.suspendAndResume $  saveSettings st
                                                           >> startShell (st ^. uiPem) (st ^. uiIp) sg selected
                                                           >> pure st

                _ -> do
                  r <- BL.handleListEventVi BL.handleListEvent ve $ st ^. uiInstances
                  let selected = snd <$> BL.listSelectedElement r
  
                  let st1 = st & uiInstances .~ r & uiSelectedInstance .~ selected
                  let st2 = st1 & uiForwards .~ BL.list NameForwardsList (Vec.fromList $ maybe [] (\(_, e) -> A.ec2PortForwards e) (BL.listSelectedElement $ st1 ^. uiInstances)) 1
                  B.continue . clearForwardsEdits $ st2

            Just NameButtonAdd ->
              case k of
                K.KChar '\t' -> B.continue . clearForwardsEdits $ st & uiFocus %~ BF.focusNext
                K.KBackTab -> B.continue $ st & uiFocus %~ BF.focusPrev

                K.KEnter -> do
                  let localPort = Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. uiEditForwardLocalPort
                  let remotePort = Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. uiEditForwardRemotePort
                  let remoteHost = Txt.strip . Txt.unlines $ BE.getEditContents $ st ^. uiEditForwardRemoteHost
                  case (readMaybe $ Txt.unpack localPort, readMaybe $ Txt.unpack remotePort) of
                    (Nothing, _) -> B.continue $ st & uiAddError .~ "Invalid local port"
                    (_, Nothing) -> B.continue $ st & uiAddError .~ "Invalid remote port"
                    (Just localPort', Just remotePort') ->
                      B.continue . clearForwardsEdits . updateForwards $ st & uiForwards %~ BL.listInsert 0 (localPort', remoteHost, remotePort')
                    
                _ -> B.continue st

            Just NameForwardsList ->
              case k of
                K.KChar '\t' -> B.continue $ st & uiFocus %~ BF.focusNext
                K.KBackTab -> B.continue . clearForwardsEdits $ st & uiFocus %~ BF.focusPrev
                K.KDel -> 
                  case st ^. (uiForwards . BL.listSelectedL) of
                    Nothing -> B.continue st
                    Just i -> B.continue . updateForwards $ st & uiForwards %~ BL.listRemove i

                _ -> do
                  r <- BL.handleListEventVi BL.handleListEvent ve $ st ^. uiForwards
                  B.continue $ st & uiForwards .~ r

            Just NameForwardLocalPort -> handleEditor uiEditForwardLocalPort uiEditForwardLocalPort k ve
            Just NameForwardRemotePort -> handleEditor uiEditForwardRemotePort uiEditForwardRemotePort k ve
            Just NameForwardRemoteHost -> handleEditor uiEditForwardRemoteHost uiEditForwardRemoteHost k ve

            _ -> B.continue st

    (B.AppEvent (EventStatus s)) -> 
      B.continue $ st & uiStatus .~ s

    (B.AppEvent (EventUpdate is')) -> do
      is <- liftIO $ applySettings is'
      B.continue $ applyUpdate st is
  
    _ -> B.continue st

  where
    handleEditor e e' k ve =
      case k of
        K.KChar '\t' -> B.continue $ st & uiFocus %~ BF.focusNext
        K.KBackTab -> B.continue $ st & uiFocus %~ BF.focusPrev

        _ -> do
          r <- BE.handleEditorEvent ve $ st ^. e
          B.continue $ st & e' .~ r

    updateForwards st' =
      let forwards = Vec.toList $ st' ^. (uiForwards . BL.listElementsL) in
      st' & uiInstances .~ BL.listModify (\e -> e { A.ec2PortForwards = forwards }) (st' ^. uiInstances)
          & uiSelectedInstance . _Just %~ (\e -> e { A.ec2PortForwards = forwards })
      
    clearForwardsEdits st' =
      st' & uiAddError .~ ""
          & uiEditForwardLocalPort .~ BE.editor NameForwardLocalPort (Just 1) ""
          & uiEditForwardRemotePort .~ BE.editor NameForwardRemotePort (Just 1) ""
          & uiEditForwardRemoteHost .~ BE.editor NameForwardRemoteHost (Just 1) "localhost"

    saveSettings st' = do
      let settings = Vec.toList $ (\e -> (A.ec2Name e, A.ec2PortForwards e)) <$> st' ^. (uiInstances . BL.listElementsL)
      let j = Ae.encode . Map.fromList $ settings
      BS.writeFile "settings.js" $ BSL.toStrict j


applyUpdate :: UIState -> [A.Ec2Instance] -> UIState
applyUpdate st0 es = do
  let origInstances = Vec.toList $ st0 ^. (uiInstances . BL.listElementsL)

  -- Update instances with info from AWS but keep in-memory port forwards
  let updatedInstances = foldr (updateInstance origInstances) [] es
  
  -- Use the newly updated instances
  let st1 = st0 & uiInstances .~ BL.list NameInstances (Vec.fromList updatedInstances) 1

  -- Keep current selection
  let origSelected = st0 ^. uiSelectedInstance
  let useOldSelection st' = st' & uiSelectedInstance .~ (snd <$> BL.listSelectedElement (st' ^. uiInstances))
  let st2 = case origSelected of
              Nothing -> --Nothing was selected, use current
                useOldSelection st1

              Just oldSel ->
                -- Try find the old element in the new list
                case Lst.findIndex (\e -> A.ec2Name e == A.ec2Name oldSel) updatedInstances of
                  Nothing -> -- Old item not found, use current
                    useOldSelection st1

                  Just idx -> -- Found
                    let new = updatedInstances Lst.!! idx in
                    st1 & uiSelectedInstance ?~ new
                        & uiInstances %~ BL.listMoveTo idx

  let st3 = st2 & uiForwards .~ BL.list NameForwardsList (Vec.fromList $ maybe [] (\(_, e) -> A.ec2PortForwards e) (BL.listSelectedElement $ st2 ^. uiInstances)) 1
  st3

  where
    updateInstance origInstances inst a =
      --Find the instance being updated
      case headMay . filter (\i -> A.ec2Name i == A.ec2Name inst) $ origInstances of
        -- Not updating, just add
        Nothing -> inst : a

        -- Updating, use in memory forwards
        Just old -> inst { A.ec2PortForwards = A.ec2PortForwards old } : a
      

-- | Draw the UI
drawUI :: UIState -> [B.Widget Name]
drawUI st =
  [B.padAll 1 contentBlock] 

  where
    contentBlock =
      (B.hLimit 50 $ block "Instances" instancesList)
      <+>
      (B.padTop (B.Pad 2) $ rightBlock)
      <=>
      bottomBar

    block n f =
      vtitle n
      <=>
      (B.withBorderStyle BBS.unicode $ BB.border f)

    rightBlock =
      (B.vLimit 20 $ block "Detail" detailBlock)
      <=>
      (B.vLimit 20 $ B.padTop (B.Pad 1) $ B.padBottom (B.Pad 1) $ block "Settings" settingsBlock)
      <=>
      (B.vLimit 20 $ B.padTop (B.Pad 1) $ B.padBottom (B.Pad 1) $ block "Help" helpBlock)

    settingsBlock =
      (B.padAll 1 settingsInner)

    helpBlock =
      (B.padAll 1 $
      ( titleTxt "Enter"
        <=>
        titleTxt "F5"
        <=>
        titleTxt "s"
      )
      <+>
      ( (titleTxt ": " <+> B.txt "Start ssh")
        <=>
        (titleTxt ": " <+> B.txt "Refresh from AWS")
        <=>
        (titleTxt ": " <+> B.txt "Start shell (see echo for variables)")
       )
      )

    settingsInner =
      (block "Port Forwarding" (B.hLimit 30 $ settingsForwardList))
      <+>
      (B.padLeft (B.Pad 3) $ B.hLimit 60 $ settingsAddBlock)
      
    settingsAddBlock =
      (block "New Port Forward" settingsAddForward)
      <=>
      (button NameButtonAdd "Add" <+> errorAdd)

    settingsAddForward =
      ( titleTxt "Local port"
        <=>
        titleTxt "Remote host"
        <=>
        titleTxt "Remote port"
      )
      <+>
      ( (titleTxt ": " <+> editor NameForwardLocalPort 5 (st ^. uiEditForwardLocalPort))
        <=>
        (titleTxt ": " <+> editor NameForwardRemoteHost 15 (st ^. uiEditForwardRemoteHost))
        <=>
        (titleTxt ": " <+> editor NameForwardRemotePort 5 (st ^. uiEditForwardRemotePort))
      )

    settingsForwardList =
      BL.renderList (\_ (l, rh, rp) -> B.txt $ show l <> " <- " <> rh <> ":" <> show rp) (BF.focusGetCurrent (st ^. uiFocus) == Just NameForwardsList) (st ^. uiForwards)

    errorAdd =
      B.withAttr "messageError" $ B.txt $ st ^. uiAddError

    detailBlock =
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
        titleTxt "Security group"
      )
      <+>
      ( (titleTxt ": " <+> (txt $ maybe "" A.ec2Name (st ^. uiSelectedInstance)))
        <=>
        (titleTxt ": " <+> (txt $ maybe "" A.ec2State (st ^. uiSelectedInstance)))
        <=>
        (titleTxt ": " <+> (txt $ maybe "" A.ec2PublicDnsName (st ^. uiSelectedInstance)))
        <=>
        (titleTxt ": " <+> (txt $ maybe "" A.ec2PublicIpAddress (st ^. uiSelectedInstance)))
        <=>
        (titleTxt ": " <+> (txt $ maybe "" A.ec2InstanceType (st ^. uiSelectedInstance)))
        <=>
        (titleTxt ": " <+> (txt $ maybe "" A.ec2Placement (st ^. uiSelectedInstance)))
        <=>
        (titleTxt ": " <+> (txt $ maybe "" A.ec2LaunchTime (st ^. uiSelectedInstance)))
        <=>
        (titleTxt ": " <+> (txt $ maybe "" A.ec2InstanceId (st ^. uiSelectedInstance)))
        <=>
        (titleTxt ": " <+> (txt $ maybe "" (maybe "" snd . A.ec2SecurityGroup) (st ^. uiSelectedInstance)))
      )
      <+>
      B.fill ' '

    instancesList =
      BL.renderList (\_ e -> B.txt $ A.ec2Name e) (BF.focusGetCurrent (st ^. uiFocus) == Just NameInstances) (st ^. uiInstances)

    bottomBar =
      B.vLimit 1 $ bottomBarLeft <+> B.fill ' ' <+> bottomBarRight

    bottomBarLeft =
      B.txt $ "awssy " <> version

    bottomBarRight =
      B.withAttr "messageInfo" $ B.txt $ st ^. uiStatus
      
    titleTxt t =
      B.withAttr "titleText" $ txt t

    txt t =
      B.txt $ if Txt.null t then " " else t
  
    --htitle t =
    --  B.hLimit 20 $
    --  B.withAttr "infoTitle" $
    --  B.txt t
      
    vtitle t =
      B.withAttr "infoTitle" $
      B.txt t

    editor n s e =
      B.hLimit s $
      B.vLimit 1 $
      BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent (st ^. uiFocus) == Just n) e

    button n t =
      let f = BF.focusGetCurrent (st ^. uiFocus) == Just n in
      let a = if f then "buttonFocus" else "button" in
      B.withAttr a $
      B.withBorderStyle (if f then BBS.unicodeBold else BBS.unicodeRounded) $
      BB.border $ B.txt t


theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BE.editAttr               , V.black `B.on` V.cyan)
                              , (BE.editFocusedAttr        , V.black `B.on` V.yellow)
                              , (BL.listAttr               , V.white `B.on` V.blue)
                              , (BL.listSelectedAttr       , V.blue  `B.on` V.white)
                              , (BL.listSelectedFocusedAttr, V.black `B.on` V.yellow)
                              , ("infoTitle"               , B.fg V.cyan)
                              , ("button"                  , V.defAttr)
                              , ("buttonFocus"             , V.black `B.on` V.yellow)
                              , ("messageError"            , B.fg V.red)
                              , ("messageWarn"             , B.fg V.brightYellow)
                              , ("messageInfo"             , B.fg V.cyan)
                              , ("titleText"               , B.fg V.green)
                              ]

exec :: [Text] -> IO ExitCode
exec cmds = A.execWait' "sh" Nothing Nothing ["-c", Txt.intercalate "; " cmds]

startSh :: Text -> Text -> A.Ec2Instance -> IO () -> IO ()
startSh ip sg inst fn = 
  bracket_ auth deauth fn

  where
    auth  =
      exec [ "echo '### " <> A.ec2Name inst <> " ###'"
           , "echo grant access to: " <> ip
           , "echo aws --region eu-west-1 ec2 authorize-security-group-ingress --group-id " <> sg <> " --protocol tcp --port 22 --cidr " <> ip <> "/24"
           , "aws --region eu-west-1 ec2 authorize-security-group-ingress --group-id " <> sg <> " --protocol tcp --port 22 --cidr " <> ip <> "/24"
           ]

    deauth =
      exec [ "echo remove access from: " <> ip
           , "echo aws --region eu-west-1 ec2 revoke-security-group-ingress --group-id " <> sg <> " --protocol tcp --port 22 --cidr " <> ip <> "/24"
           , "aws --region eu-west-1 ec2 revoke-security-group-ingress --group-id " <> sg <> " --protocol tcp --port 22 --cidr " <> ip <> "/24"
           ]


startSsh :: Text -> Text -> Text -> A.Ec2Instance -> IO ()
startSsh pem ip sg inst = 
  startSh ip sg inst $ do
    let fs = (\(l, h, r) -> "-L " <> show l <> ":" <> h <> ":" <> show r) <$> A.ec2PortForwards inst 
    let args = Txt.intercalate " " fs

    void $ exec [ "echo ssh -i " <> pem <> " ec2-user@" <> A.ec2PublicIpAddress inst <> " " <> args
                , "ssh -i " <> pem <> " ec2-user@" <> A.ec2PublicIpAddress inst <> " " <> args
                ]


startShell :: Text -> Text -> Text -> A.Ec2Instance -> IO ()
startShell pem ip sg inst = do
  let env' = Map.fromList [ ("AWS_H", A.ec2PublicIpAddress inst)
                          , ("AWS_U", "ec2-user")
                          , ("AWS_K", pem)
                          , ("AWS_UH", "ec2-user@" <> A.ec2PublicIpAddress inst)
                          ]

  currentEnv <- Map.fromList <$> ((\(k, v) -> (Txt.pack k, Txt.pack v)) <<$>> Env.getEnvironment)
  let env = Map.toList $ Map.union env' currentEnv
  let sh = Map.findWithDefault "sh" "SHELL" currentEnv
  print env

  startSh ip sg inst $ do
    void $ exec [ "echo ''"
                , "echo 'Shell for: ### " <> A.ec2Name inst <> " ###'"
                , "echo '  AWS_H: " <> A.ec2PublicIpAddress inst <> "'"
                , "echo '  AWS_U: ec2-user'"
                , "echo '  AWS_K: " <> pem <> "'"
                , "echo '  AWS_UH: ec2-user@" <> A.ec2PublicIpAddress inst <> "'"
                , "echo '  e.g. scp -i $AWS_K ./README.md $AWS_UH:/home/$AWS_U/README.md'"
                , "echo ''"
                ]

    void $ A.execWait' (Txt.unpack sh) Nothing (Just env) []


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


applySettings :: [A.Ec2Instance] -> IO [A.Ec2Instance]
applySettings es = 
  Dir.doesFileExist "settings.js" >>= \case
    False -> pure es
    True -> do
      j <- BSL.fromStrict <$> BS.readFile "settings.js"
      case Ae.eitherDecode j :: Either [Char] (Map Text [(Int, Text, Int)]) of
        Left _ -> pure es
        Right ss -> pure $ updateEc2 ss <$> es

  where
    updateEc2 ss e =
      case Map.lookup (A.ec2Name e) ss of
        Nothing -> e
        Just s -> e { A.ec2PortForwards = s }
          
 
