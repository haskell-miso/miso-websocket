-----------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE CPP                        #-}
-----------------------------------------------------------------------------
module Main (main) where
-----------------------------------------------------------------------------
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
-----------------------------------------------------------------------------
import           Miso hiding (on)
import           Miso.Lens
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
import           WebSocket
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
data Action
  = AddWebSocket
  | Close Int
  | NoOp
-----------------------------------------------------------------------------
data Model = Model
  { _nextConnection :: Int
  , _connections :: IntSet
  } deriving Eq
-----------------------------------------------------------------------------
nextConnection :: Lens Model Int
nextConnection = lens _nextConnection $ \r x -> r { _nextConnection = x }
-----------------------------------------------------------------------------
connections :: Lens Model IntSet
connections = lens _connections $ \r x -> r { _connections = x }
-----------------------------------------------------------------------------
main :: IO ()
main = run (startApp app)
-----------------------------------------------------------------------------
app :: App Model Action
app = (component emptyModel update_ appView)
  { events = M.singleton "click" False
  , mailbox = checkMail Close (const NoOp)
#ifndef WASM
  , styles = [ Href "assets/style.css" ]
#endif
  } where
     emptyModel = Model 0 mempty
     update_ (Close x) = do
       connections %= IS.delete x
     update_ AddWebSocket = do
       nextConnection += 1
       connId <- use nextConnection
       connections %= IS.insert connId
     update_ NoOp =
       pure ()
-----------------------------------------------------------------------------
githubStar :: View model action
githubStar = iframe_
    [ title_ "GitHub"
    , height_ "30"
    , width_ "170"
    , textProp "scrolling" "0"
    , textProp "frameborder" "0"
    , src_
      "https://ghbtns.com/github-btn.html?user=haskell-miso&repo=miso-websocket&type=star&count=true&size=large"
    ]
    []
-----------------------------------------------------------------------------
appView :: Model -> View Model Action
appView m =
  div_
  []
  [ githubStar
  , div_
    [ class_ "container"
    ]
    [ h1_
      []
      [ "🍜 miso-websocket ⚡"
      ]
    , div_
      [ class_ "controls" ]
      [ button_
        [ class_ "btn btn-primary"
        , id_ "add-websocket-btn"
        , onClick AddWebSocket
        ]
        [ "Add New WebSocket"
        ]
      ]
    , div_
      [ class_ "websockets-container"
      , id_ "websockets-container"
      ] -- the syncChildren case should kick in here as well
      [ div_ [ key_ connId ] +> websocketComponent connId
      | connId <- IS.toList (m ^. connections)
      ]
    ]
  ]
-----------------------------------------------------------------------------
