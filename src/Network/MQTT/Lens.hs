{-# LANGUAGE TemplateHaskell #-}

module Network.MQTT.Lens where

import           Control.Lens
import           Network.MQTT.Types

class HasProperties c where
  properties :: Lens' c [Property]

makeLenses ''ConnectRequest
instance HasProperties ConnectRequest where properties = connProperties

makeLenses ''LastWill
instance HasProperties LastWill where properties = willProps

makeLenses ''PublishRequest
instance HasProperties PublishRequest where properties = pubProps

makeLenses ''SubOptions

makePrisms ''ConnACKRC
makePrisms ''DiscoReason
makePrisms ''MQTTPkt
makePrisms ''Property
makePrisms ''ProtocolLevel
makePrisms ''QoS
makePrisms ''RetainHandling
makePrisms ''SessionReuse
makePrisms ''SubErr
makePrisms ''UnsubStatus

-- Manual lenses for unnamed fields

-- TODO: AuthRequest
instance HasProperties AuthRequest where
  properties = lens (\(AuthRequest _ ps) -> ps) (\(AuthRequest a _) p -> AuthRequest a p)
-- TODO: ConnACKFlags
instance HasProperties ConnACKFlags where
  properties = lens (\(ConnACKFlags _ _ ps) -> ps) (\(ConnACKFlags a b _) p -> ConnACKFlags a b p)
-- TODO: DisconnectRequest
instance HasProperties DisconnectRequest where
  properties = lens (\(DisconnectRequest _ ps) -> ps) (\(DisconnectRequest a _) p -> DisconnectRequest a p)
-- TODO: PubACK
instance HasProperties PubACK where
  properties = lens (\(PubACK _ _ ps) -> ps) (\(PubACK a b _) p -> PubACK a b p)
-- TODO: PubCOMP
instance HasProperties PubCOMP where
  properties = lens (\(PubCOMP _ _ ps) -> ps) (\(PubCOMP a b _) p -> PubCOMP a b p)
-- TODO: PubREC
instance HasProperties PubREC where
  properties = lens (\(PubREC _ _ ps) -> ps) (\(PubREC a b _) p -> PubREC a b p)
-- TODO: PubREL
instance HasProperties PubREL where
  properties = lens (\(PubREL _ _ ps) -> ps) (\(PubREL a b _) p -> PubREL a b p)
-- TODO: SubscribeRequest
instance HasProperties SubscribeRequest where
  properties = lens (\(SubscribeRequest _ _ ps) -> ps) (\(SubscribeRequest a b _) p -> SubscribeRequest a b p)
-- TODO: SubscribeResponse
instance HasProperties SubscribeResponse where
  properties = lens (\(SubscribeResponse _ _ ps) -> ps) (\(SubscribeResponse a b _) p -> SubscribeResponse a b p)
-- TODO: UnsubscribeRequest
instance HasProperties UnsubscribeRequest where
  properties = lens (\(UnsubscribeRequest _ _ ps) -> ps) (\(UnsubscribeRequest a b _) p -> UnsubscribeRequest a b p)
-- TODO: UnsubscribeResponse
instance HasProperties UnsubscribeResponse where
  properties = lens (\(UnsubscribeResponse _ ps _) -> ps) (\(UnsubscribeResponse a _ b) p -> UnsubscribeResponse a p b)
