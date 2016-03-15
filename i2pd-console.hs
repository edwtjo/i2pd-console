{-# LANGUAGE OverloadedStrings #-}
module Main where

-- threads
import Control.Monad (forever,when)
import Control.Concurrent (threadDelay,newChan,writeChan,Chan,forkIO)

-- wallclock
import Data.Time.Clock (getCurrentTime,diffUTCTime,NominalDiffTime)
import System.IO

-- json
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Foldable
import Data.Text hiding (pack)
import Data.Map
import Data.Maybe (fromJust)

-- web
import Data.Text.Encoding (encodeUtf8)
import Control.Lens hiding ((.=))
import Network.Wreq
import qualified Network.Wreq.Session as S
import Network.Connection (TLSSettings(TLSSettingsSimple))
import Network.HTTP.Client (ManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings,mkManagerSettings)

-- gfxs
import Control.Monad
import Data.Default
import Data.Monoid
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Brick.Main
import Brick.Util (fg, bg, on)
import qualified Brick.AttrMap as A
import Brick.Types (Widget, EventM, Next)
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

type RouterInfos = (RpcCall,NominalDiffTime)
type InfoChan = Chan ConsoleEvent
data ConsoleState = MkConsole RouterInfos
data ConsoleEvent = VtyEvt V.Event | RouterUpdate RouterInfos

data NetStat = OK | Testing | Firewalled
  deriving (Eq,Ord,Enum,Bounded,Show)

data RpcCall = RouterInfo { uptime :: Int
                          , status :: String
                          , version :: String
                          , netStat :: NetStat
                          , sentBytes :: Double
                          , recievedBytes :: Double
                          , participatingTunnels :: Int
                          , activePeers :: Int
                          , knownPeers :: Int
                          , inboundBw1s :: Float
                          , outboundBw1s :: Float
                          , token :: Maybe String
                          }
             | CallAuthenticate Int String
                 deriving Show

u = "i2p.router.uptime"
s = "i2p.router.status"
v = "i2p.router.version"
ns = "i2p.router.net.status"
nsb = "i2p.router.net.total.sent.bytes"
nrb = "i2p.router.net.total.received.bytes"
ntp = "i2p.router.net.tunnels.participating"
nap = "i2p.router.netdb.activepeers"
nkp = "i2p.router.netdb.knownpeers"
nbi = "i2p.router.net.bw.inbound.1s"
nbo = "i2p.router.net.bw.outbound.1s"
tok = "Token"
pas = "Password"
apis = "API"

instance FromJSON RpcCall where
  parseJSON (Object o) =
    asum [ RouterInfo
           <$> (r >>= (.: u))
           <*> (r >>= (.: s))
           <*> (r >>= (.: v))
           <*> ((r >>= (.: ns)) >>= \a -> return $ toEnum a)
           <*> (r >>= (.: nsb))
           <*> (r >>= (.: nrb))
           <*> (r >>= (.: ntp))
           <*> (r >>= (.: nap))
           <*> (r >>= (.: nkp))
           <*> (r >>= (.: nbi))
           <*> (r >>= (.: nbo))
           <*> return Nothing
         , CallAuthenticate
           <$> (r >>= (.: apis))
           <*> (r >>= (.: tok))
         ]
           where
             r = o .: "result"
  parseJSON wat = typeMismatch "RpcCall" wat

instance ToJSON RpcCall where
  toJSON (RouterInfo _ _ _ _ _ _ _ _ _ _ _ (Just tok')) =
    object [ "id" .= Number 1
           , "method" .= String "RouterInfo"
           , "jsonrpc" .= String "2.0"
           , "params" .=
             object [ u .= es
                    , s .= es
                    , v .= es
                    , ns .= es
                    , nsb .= es
                    , nrb .= es
                    , ntp .= es
                    , nap .= es
                    , nkp .= es
                    , nbi .= es
                    , nbo .= es
                    , tok .= tok'
                    ]
           ]
           where
             es = String ""
  toJSON (CallAuthenticate apis' tok') =
    object [ "id" .= Number 1
           , "method" .= String "Authenticate"
           , "jsonrpc" .= String "2.0"
           , "params" .=
             object [ apis .= apis'
                    , pas .= tok' ]
           ]

host :: String
host = "127.0.0.1"

port :: Int
port = 7650

i2pdctlUrl :: String
i2pdctlUrl = "https://"++host++":"++show port

acceptAllCertificates :: ManagerSettings
acceptAllCertificates = mkManagerSettings tlsSettings Nothing
  where
    tlsSettings = TLSSettingsSimple True False False

toopie :: RpcCall
toopie = CallAuthenticate 1 "itoopie"

rinfo :: String -> RpcCall
rinfo tok = RouterInfo 0 "" "" OK 0 0 0 0 0 0.0 0.0 (Just tok)

updateRouterInfo :: InfoChan -> IO ()
updateRouterInfo rpcInfo =
  S.withSessionControl
    Nothing acceptAllCertificates $ \sess -> do
      r <- S.post sess i2pdctlUrl (toJSON toopie)
      let ma = (decode $ r ^. responseBody) :: Maybe RpcCall
      case ma of
        Just (CallAuthenticate id toky) -> forever $ do
          t <- getCurrentTime
          r <- S.post sess i2pdctlUrl (toJSON $ rinfo toky)
          t' <- getCurrentTime
          let dt = diffUTCTime t' t
              usecs = floor $ toRational dt*1000000 :: Int
              delay = 1000*1000 - usecs
          let rpc = (decode $ r ^. responseBody) :: Maybe RpcCall
          case rpc of
            Just rpc' -> do
              writeChan rpcInfo $ RouterUpdate (rpc',dt)
            Nothing -> do
              return ()
          when (delay > 0) $ threadDelay delay
        Nothing -> return ()

mkShowy :: Show a => (String,a) -> Widget
mkShowy (lbl,sh) =
  withBorderStyle BS.unicode $
  B.borderWithLabel (str lbl) $
  C.center $
  txt $ (T.pack $ show sh)

mkCollected :: String -> [Widget] -> Widget
mkCollected lbl wis =
    withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str lbl) $
    vLimit 5 $
    C.vCenter $
    hBox wis

frame :: (RpcCall,NominalDiffTime) -> Widget
frame (call@(RouterInfo _ _ _ _ _ _ _ _ _ _ _ _),rtt) =
  mkCollected "Router Info"
    [ mkShowy ("Uptime (Hours)",floor $ (fromIntegral $ uptime call)/(1000*3600))
    , mkShowy ("Net Status", netStat call)
    , mkShowy ("Version",version call)
    , mkShowy ("RTT (Console/Router)", rtt)
    ]
  <=> mkCollected "Network"
    [ mkShowy ("Known Peers", knownPeers call)
    , mkShowy ("Active Peers", activePeers call)
    , mkShowy ("Participating Tunnels", participatingTunnels call)]
  <=> mkCollected "Bandwidth"
    [ mkShowy ("Inbound B/s", inboundBw1s call)
    , mkShowy ("Recieved MBytes", floor $ (recievedBytes call)/1048576)
    , mkShowy ("Outbound B/s", outboundBw1s call)
    , mkShowy ("Sent MBytes", floor $ (sentBytes call)/1048576)
    ]

consoleEvent :: ConsoleState -> ConsoleEvent -> EventM (Next ConsoleState)
consoleEvent cs@(MkConsole info) e = do
      case e of
        VtyEvt (V.EvKey V.KEsc []) -> halt cs
        RouterUpdate ri -> continue $ MkConsole ri
        _ -> continue cs

console :: App ConsoleState ConsoleEvent
console =
    App { appDraw = (\cs@(MkConsole ri) -> [frame ri])
        , appChooseCursor = neverShowCursor
        , appHandleEvent = consoleEvent
        , appStartEvent = return
        , appAttrMap = const def
        , appLiftVtyEvent = VtyEvt
        }

main = do
  hSetBuffering stdout NoBuffering
  infoChan <- newChan
  forkIO (updateRouterInfo infoChan)
  void $ customMain (V.mkVty def) infoChan console (MkConsole (rinfo "dummy",0.0))
