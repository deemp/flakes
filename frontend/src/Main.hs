{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Aeson as Aeson
import           Data.ByteString    as B
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.Functor.Sum
import           Data.List.NonEmpty
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import           Data.Text (Text)
import           Reflex
import           Reflex.Dom
import           Language.Javascript.JSaddle
import           Control.Monad.Fix (MonadFix)
import           Control.Monad      (void)
import qualified Data.Map.Strict as M

import WebChat.Common

endpointUri :: Text
endpointUri = "ws://localhost:8080/endpoint"

main :: IO ()
main = mainWidget $ do
  rec
    let
      msgSendEv = traceEvent "send" $ switch (current msgEvDyn)
      msgRecvEv = traceEvent "recv" $ wsRespEv
      userEv = traceEvent "user" $ fmapMaybe loginEv msgSendEv

    wsRespEv <- websocketWidget msgSendEv

    channelsDyn <- foldDynMaybe channelFold [defaultChannel] msgRecvEv
    chEv <- traceEvent "selected channel" <$> channelWidget channelsDyn
    chDyn <- holdDyn defaultChannel chEv

    userDyn <- holdDyn (User "") userEv
    msgEvDyn <- widgetHold loginWidget (chatWidget userDyn chDyn <$ userEv)

  messages <- foldDynMaybe msgFold [] msgRecvEv
  let
    filteredMessages = Prelude.filter . filterMsg <$> chDyn <*> messages
    shownMessages = fmap showMsg <$> filteredMessages

  void $ el "div" $ do
    el "ul" $ simpleList shownMessages (\m -> el "li" $ dynText m)
  where
    filterMsg :: Channel -> Message -> Bool
    filterMsg ch (Message _ recep _) = ch == recep

    loginEv (Login name) = Just $ User name
    loginEv _ = Nothing

    msgFold :: ServerCommand -> [Message] -> Maybe [Message]
    msgFold (NewMessage msg) msgs = Just $ msg : msgs
    msgFold _ _ = Nothing

    showMsg :: Message -> Text
    showMsg m = T.concat [userName $ sender m, ": ", contents m]

    channelFold :: ServerCommand -> [Channel] -> Maybe [Channel]
    channelFold (NewChannel ch) chs = Just $ ch : chs
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
  => Event t ClientCommand
  -> m (Event t ServerCommand)
websocketWidget msgSendEv = do
  let sendEv = fmap ((:[]) . toStrict . Aeson.encode) msgSendEv
  ws <- webSocket endpointUri $ def & webSocketConfig_send .~ sendEv
  return $ traceEvent "websocketRaw" $ fmapMaybe decodeOneMsg (_webSocket_recv ws)
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
    t <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") commandEv
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
channelWidget channels = el "div" $ do
  rec
    let channelMapDyn = fmap channelsToMap channels
    dd <- dropdown defaultChannel channelMapDyn $ def
  return $ dd & _dropdown_change

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
      -- & inputElementConfig_setValue .~ fmap (const "") eNewName
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
  newChEv <- traceEvent "channelWidget" <$> newChannelWidget
  msgEv <- traceEvent "messagingWidget" <$> messagingWidget userDyn channelDyn
  return $ leftmost [newChEv, msgEv]
