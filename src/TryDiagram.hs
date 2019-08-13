{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-unused-imports #-}

module TryDiagram where

import Control.Monad.State
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.Adornt.Builder
import Circuit.DiagramDsl

import Circuit.Adornt.Diagram
import Circuit.Adornt.Parts

sampleNotGate :: (OWire, CBState)
sampleNotGate = (`runState` initCBState) $ do
	(_, ow) <- notGate
	(iw', ow') <- notGate
	(iw'', ow'') <- notGate
	connectWire64 ow iw'
	connectWire64 ow' iw''
	return ow''

sampleAndGate :: (OWire, CBState)
sampleAndGate = (`runState` initCBState) $ do
	(_, _, ow) <- andGate
	(_, _, ow') <- andGate
	(iw1'', iw2'', ow'') <- andGate
	connectWire64 ow iw1''
	connectWire64 ow' iw2''
	return ow''

sampleNandGate :: (OWire, CBState)
sampleNandGate = (`runState` initCBState) $ do
	(_, _, o) <- nandGate
	return o

sampleNorGate :: (OWire, CBState)
sampleNorGate = (`runState` initCBState) $ do
	(_, _, o) <- norGate
	return o

sampleXorGate :: (OWire, CBState)
sampleXorGate = (`runState` initCBState) $ do
	(_, _, o) <- xorGate
	return o

sampleAndNotBGate :: (OWire, CBState)
sampleAndNotBGate = (`runState` initCBState) $ do
	(_, _, o) <- andNotBGate
	return o

sampleOrNotBGate :: (OWire, CBState)
sampleOrNotBGate = (`runState` initCBState) $ do
	(_, _, o) <- orNotBGate
	return o
