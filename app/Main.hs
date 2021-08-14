{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Protolude
import Brick ((<+>))
import qualified Brick as B
import           Control.Exception.Safe (throwString)
import           Control.Lens (makeLenses, (?~))
import qualified Data.Text as Txt
import qualified Data.UUID.V4 as UU
import qualified Data.Version as Ver
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K

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

data AwState = AwState
  { _usStatus :: !Text
  }

data AwPopup
  = PopDemo

data AwWindow
  = WMain

data AwName
  = NameTodo
  deriving (Show, Eq, Ord)

data AwEvent
  = ETodo

makeLenses ''AwState


main :: IO ()
main = Args.runArgs run


run :: Args.Args -> IO ()
run args = do
  let uiinit = Bbd.defaultInit
       { Bb._uioStartWindow = regWindowMain
       , Bb._uioAppName = "Awssy"
       , Bb._uioAppVersion = Txt.pack $ Ver.showVersion Paths.version
       , Bb._uioHandleUserEvents = handleEvent
       }

  let ust = AwState
       { _usStatus = ""
       }

  Bb.runTui uiinit ust


regWindowMain :: Window'
regWindowMain =
  Bb.WUser WMain "Main" . Just $
    Bb.WindowReg
      { Bb._wrDraw = const drawMain
      , Bb._wrEventHandler = const windowKeyHandler
      }


handleEvent :: AwEvent -> UIState' -> IO UIState'
handleEvent ev st =
  case ev of
    ETodo -> pure st


windowKeyHandler :: UIState' -> B.BrickEvent Name' Event' -> B.EventM Name' (B.Next UIState')
windowKeyHandler st ev =
  case ev of
    (B.VtyEvent (V.EvKey k ms)) ->
      case (k, ms) of
        _ -> Bbd.defaultWindowKeyHandler st ev

    _ -> Bbd.defaultWindowKeyHandler st ev


drawMain :: UIState' -> B.Widget Name'
drawMain _st =
  B.txt "awssy main window" <+> B.fill ' '


