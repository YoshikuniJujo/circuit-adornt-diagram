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

sample2 :: (OWire, CBState)
sample2 = (`runState` initCBState) $ do
	(_, no) <- notGate
	(a, _, o) <- andGate
	(ni', no') <- notGate
	connectWire64 no a
	connectWire64 o ni'
	return no'

sampleBranch :: (OWire, CBState)
sampleBranch = (`runState` initCBState) $ do
	(_ni, no) <- notGate
	(_ni', no') <- notGate
	(ni'', no'') <- notGate
	connectWire (no, 32, 0) (ni'', 32, 32)
	connectWire (no', 32, 0) (ni'', 32, 0)
	return no''

sampleBranch2 :: (OWire, CBState)
sampleBranch2 = (`runState` initCBState) $ do
	(a, b, o) <- andGate
	(_ni0, no0) <- notGate
	(_ni1, no1) <- notGate
	(_ni2, no2) <- notGate
	(_ni3, no3) <- notGate
	(_ni4, no4) <- notGate
	connectWire (no0, 32, 0) (a, 32, 0)
	connectWire (no1, 32, 0) (a, 32, 32)
	connectWire (no2, 16, 0) (b, 16, 0)
	connectWire (no3, 16, 0) (b, 16, 16)
	connectWire (no4, 32, 0) (b, 32, 32)
	return o
