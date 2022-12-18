{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.List.NonEmpty ()
import qualified Data.Text          as T
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
import           Language.Javascript.JSaddle
import           Control.Monad.Fix (MonadFix)
import           Control.Monad      (void)
import qualified Data.Map.Strict as M

import WebChat.Common

main :: IO ()
main = mainWidget $ do
  host <- getLocationHost
  protocol <- getLocationProtocol
  let
    websocketProtocol :: Text
    websocketProtocol = case protocol of
      "http:" -> "ws:"
      "https:" -> "wss:"
      _ -> "ws:"
    endpointUri :: Text
    endpointUri = T.concat [websocketProtocol ,"//", host, "/endpoint"]

  el "h3" $ text "Webchat"

  -- https://stackoverflow.com/a/5406008
  rec
    let
      msgSendEv = traceEvent "send" $ switch (current msgEvDyn)
      msgRecvEv = traceEvent "recv" $ wsRespEv
      userEv = traceEvent "user" $ fmapMaybe loginEv msgSendEv

    wsRespEv <- websocketWidget endpointUri msgSendEv

    channelsDyn <- foldDynMaybe channelFold [defaultChannel] msgRecvEv
    let filteredChannelsDyn = traceDyn "channels" $ filter . (/=) . Private <$> userDyn <*> channelsDyn
    chEv <- traceEvent "selected channel" <$> channelWidget filteredChannelsDyn
    chDyn <- holdDyn defaultChannel chEv

    userDyn <- holdDyn (User "") userEv
    msgEvDyn <- widgetHold loginWidget (chatWidget userDyn chDyn <$ userEv)

  messages <- foldDynMaybe msgFold [] msgRecvEv

  let
    filteredMessages = filter <$> (filterMsg <$> userDyn <*> chDyn)  <*> messages
    shownMessages = fmap showMsg <$> filteredMessages

  void $ el "div" $ do
    el "ul" $ simpleList shownMessages (\m -> el "li" $ dynText m)

  where
    filterMsg :: User -> Channel -> Message -> Bool
    filterMsg _ ch@(Public _) Message{..} = ch == recipient
    filterMsg user ch@(Private s) Message{..}
      = (s == sender && recipient == (Private user))
      || (user == sender && recipient == ch)

    loginEv (Login name) = Just $ User name
    loginEv _ = Nothing

    msgFold :: ServerCommand -> [Message] -> Maybe [Message]
    msgFold (NewMessage msg) msgs = Just $ msg : msgs
    msgFold _ _ = Nothing

    showMsg :: Message -> Text
    showMsg m = T.concat [userName $ sender m, ": ", contents m]

    channelFold :: ServerCommand -> [Channel] -> Maybe [Channel]
    channelFold (NewChannel ch) chs = Just $ ch : chs
    channelFold (RemoveChannel ch) chs = Just $ filter (/= ch) chs
    channelFold (ChannelList chs) _ = Just chs
    channelFold _ _ = Nothing

-- Websocket
websocketWidget ::
     ( MonadJSM m
     , MonadJSM (Performable m)
     , HasJSContext m
     , PerformEvent t m
     , TriggerEvent t m
     , PostBuild t m
     )
  => Text
  -> Event t ClientCommand
  -> m (Event t ServerCommand)
websocketWidget endpointUri msgSendEv = do
  let sendEv = fmap ((:[]) . toStrict . Aeson.encode) msgSendEv
  ws <- webSocket endpointUri $ def & webSocketConfig_send .~ sendEv
  return $ fmapMaybe decodeOneMsg (_webSocket_recv ws)
  where
    decodeOneMsg :: B.ByteString -> Maybe ServerCommand
    decodeOneMsg = Aeson.decode . fromStrict

-- Login widget
loginWidget
  :: ( DomBuilder t m
     , MonadFix m
     )
  => m (Event t ClientCommand)
loginWidget = el "div" $ do
  rec
    tn <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eNewName
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter username")
    bn <- button "Login"
    let eNewName = fmap T.strip
          $ tag (current $ value tn)
          $ leftmost [bn, keypress Enter tn]
  return $ Login <$> eNewName

-- Message widget
messagingWidget
  :: ( DomBuilder t m
     , MonadFix m
     )
  =>
  Dynamic t User ->
  Dynamic t Channel ->
  m (Event t ClientCommand)
messagingWidget userDyn channelDyn = el "div" $ do
  rec
    t <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") commandEv
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter your message")
    b <- button "Send"
    let
      keypressEv = leftmost [b, keypress Enter t]
      textDyn = value t
      newMessageDyn = Message <$> userDyn <*> channelDyn <*> textDyn
      commandEv = fmap SendMessage $ tag (current newMessageDyn) keypressEv
  return commandEv

-- Channel widget
channelWidget
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => Dynamic t [Channel] -> m (Event t Channel)
channelWidget channelsDyn = el "div" $ do
  rec
    let channelMapDyn = fmap channelsToMap channelsDyn
    dd <- dropdown defaultChannel channelMapDyn def
  return $ dd & _dropdown_change
  where
    channelToText :: Channel -> Text
    channelToText (Public name) = T.append "#" name
    channelToText (Private (User name)) = name

    channelsToMap :: [Channel] -> M.Map Channel Text
    channelsToMap channels = M.fromList $ fmap (\ch -> (ch, channelToText ch)) channels

-- New channel creation
newChannelWidget
  :: ( DomBuilder t m
     , MonadFix m
     )
  => m (Event t ClientCommand)
newChannelWidget = el "div" $ do
  rec
    tn <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eNewName
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter new channel name")
    bn <- button "Create"
    let eNewName = fmap T.strip
          $ tag (current $ value tn)
          $ leftmost [bn, keypress Enter tn]
  return $ CreatePublicChannel <$> eNewName


-- Chat widget
chatWidget ::
     ( DomBuilder t m
     , MonadFix m
     )
  => Dynamic t User
  -> Dynamic t Channel
  -> m (Event t ClientCommand)
chatWidget userDyn channelDyn = do
  newChEv <- newChannelWidget
  msgEv <- messagingWidget userDyn channelDyn
  return $ leftmost [newChEv, msgEv]
