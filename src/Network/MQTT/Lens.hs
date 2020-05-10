{-# LANGUAGE TemplateHaskell #-}

module Network.MQTT.Lens where

import           Control.Lens
import           Data.Word          (Word16)
import           Network.MQTT.Types

class HasProperties c where
  properties :: Lens' c [Property]

class HasPktID c where
  pktID :: Lens' c Word16

makeLenses ''ConnectRequest
instance HasProperties ConnectRequest where properties = connProperties

makeLenses ''LastWill
instance HasProperties LastWill where properties = willProps

makeLenses ''PublishRequest
instance HasProperties PublishRequest where properties = pubProps
instance HasPktID PublishRequest where pktID = pubPktID

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
instance HasPktID PubACK where
  pktID = lens (\(PubACK i _ _) -> i) (\(PubACK _ a b) i -> PubACK i a b)

-- TODO: PubCOMP
instance HasProperties PubCOMP where
  properties = lens (\(PubCOMP _ _ ps) -> ps) (\(PubCOMP a b _) p -> PubCOMP a b p)
instance HasPktID PubCOMP where
  pktID = lens (\(PubCOMP i _ _) -> i) (\(PubCOMP _ a b) i -> PubCOMP i a b)

-- TODO: PubREC
instance HasProperties PubREC where
  properties = lens (\(PubREC _ _ ps) -> ps) (\(PubREC a b _) p -> PubREC a b p)
instance HasPktID PubREC where
  pktID = lens (\(PubREC i _ _) -> i) (\(PubREC _ a b) i -> PubREC i a b)

-- TODO: PubREL
instance HasProperties PubREL where
  properties = lens (\(PubREL _ _ ps) -> ps) (\(PubREL a b _) p -> PubREL a b p)
instance HasPktID PubREL where
  pktID = lens (\(PubREL i _ _) -> i) (\(PubREL _ a b) i -> PubREL i a b)

-- TODO: SubscribeRequest
instance HasProperties SubscribeRequest where
  properties = lens (\(SubscribeRequest _ _ ps) -> ps) (\(SubscribeRequest a b _) p -> SubscribeRequest a b p)
instance HasPktID SubscribeRequest where
  pktID = lens (\(SubscribeRequest i _ _) -> i) (\(SubscribeRequest _ a b) i -> SubscribeRequest i a b)

-- TODO: SubscribeResponse
instance HasProperties SubscribeResponse where
  properties = lens (\(SubscribeResponse _ _ ps) -> ps) (\(SubscribeResponse a b _) p -> SubscribeResponse a b p)
instance HasPktID SubscribeResponse where
  pktID = lens (\(SubscribeResponse i _ _) -> i) (\(SubscribeResponse _ a b) i -> SubscribeResponse i a b)

-- TODO: UnsubscribeRequest
instance HasProperties UnsubscribeRequest where
  properties = lens (\(UnsubscribeRequest _ _ ps) -> ps) (\(UnsubscribeRequest a b _) p -> UnsubscribeRequest a b p)
instance HasPktID UnsubscribeRequest where
  pktID = lens (\(UnsubscribeRequest i _ _) -> i) (\(UnsubscribeRequest _ a b) i -> UnsubscribeRequest i a b)

-- TODO: UnsubscribeResponse
instance HasProperties UnsubscribeResponse where
  properties = lens (\(UnsubscribeResponse _ ps _) -> ps) (\(UnsubscribeResponse a _ b) p -> UnsubscribeResponse a p b)
instance HasPktID UnsubscribeResponse where
  pktID = lens (\(UnsubscribeResponse i _ _) -> i) (\(UnsubscribeResponse _ a b) i -> UnsubscribeResponse i a b)
