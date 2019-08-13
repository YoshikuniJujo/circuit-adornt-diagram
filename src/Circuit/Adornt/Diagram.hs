{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Diagram where

import Control.Monad.State
import Data.Map.Strict

import qualified Data.ByteString.Char8 as BSC

import Circuit.DiagramDsl
import Circuit.Adornt.Builder

newtype BG = BG BasicGate deriving Show

instance ElementIdable BG where
	elementIdGen (BG (NotGate iw)) = "NotGate-" <> BSC.pack (show iw)
	elementIdGen _ = error "not yet implemented"

diagramM :: CBState -> Maybe Pos -> [OWire] -> DiagramMapM BasicGate
diagramM cbs mpos [o] = case cbsGate cbs  !? o of
	Just e@(NotGate iw) -> do
		ip <- inputPosition =<< lift . maybe (Left "Oops") Right =<< case mpos of
			Nothing -> putElement0 (BG e) notGateD
			Just ps -> putElement (BG e) notGateD ps
		case cbsWireConn cbs !? iw of
			Just [(o', _)] -> do
				bg <- diagramM cbs (Just ip) [o']
				connectLine (BG e) (BG bg)
				return ()
			Nothing -> return ()
			_ -> lift $ Left "not yet implemented"
		return e
	_ -> lift $ Left "not yet implemented"
diagramM _ _ _ = lift $ Left "not yet implemented"

sampleNotGate :: (OWire, CBState)
sampleNotGate = (`runState` initCBState) $ do
	(_, ow) <- notGate
	(iw', ow') <- notGate
	(iw'', ow'') <- notGate
	connectWire64 ow iw'
	connectWire64 ow' iw''
	return ow''
