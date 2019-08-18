{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-unused-imports #-}

module TryDiagram where

import Control.Monad.State
import Data.Word
import System.FilePath
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.Adornt.Builder
import Circuit.DiagramDsl

import Circuit.Adornt.Diagram
import Circuit.Adornt.DiagramBf
import Circuit.Adornt.Parts

import CarryLookahead2

type Sample = (([OWire], CBState), (Double, FilePath))

trySample :: Sample -> IO ()
trySample ((ows, cbs), (s, fp)) =
	either error (renderSVG ("results_old" </> fp) (mkWidth s) . drawDiagram)
		. (`execDiagramMapM` 4) $ diagramM cbs ows

trySampleBf :: Sample -> IO ()
trySampleBf ((ows, cbs), (s, fp)) =
	either error (renderSVG ("results" </> fp) (mkWidth s) . drawDiagram)
		. (`execDiagramMapM` 4) . diagramBfM cbs $ (Nothing ,) <$> ows

trySampleBf2 :: Sample -> IO ()
trySampleBf2 ((ows, cbs), (s, fp)) =
	either error (renderSVG ("results2" </> fp) (mkWidth s) . drawDiagram)
		. (`execDiagramMapM` 4) . diagramBfM2 cbs $ (Nothing ,) <$> ows

sampleNotGate :: Sample
sampleNotGate = (, (950, "notGate.svg")) . (`runState` initCBState) $ do
	(_, ow) <- notGate
	(iw', ow') <- notGate
	(iw'', ow'') <- notGate
	connectWire64 ow iw'
	connectWire64 ow' iw''
	return [ow'']

sampleAndGate :: Sample
sampleAndGate = (, (950, "andGate.svg")) . (`runState` initCBState) $ do
	(_, _, ow) <- andGate
	(_, _, ow') <- andGate
	(iw1'', iw2'', ow'') <- andGate
	connectWire64 ow iw1''
	connectWire64 ow' iw2''
	return [ow'']

sampleNandGate :: Sample
sampleNandGate = (, (950, "nandGate.svg")) . (`runState` initCBState) $ do
	(_, _, o) <- nandGate
	return [o]

sampleNorGate :: Sample
sampleNorGate = (, (950, "norGate.svg")) . (`runState` initCBState) $ do
	(_, _, o) <- norGate
	return [o]

sampleXorGate :: Sample
sampleXorGate = (, (950, "xorGate.svg")) . (`runState` initCBState) $ do
	(_, _, o) <- xorGate
	return [o]

sampleAndNotBGate :: Sample
sampleAndNotBGate = (, (950, "andNotBGate.svg")) . (`runState` initCBState) $ do
	(_, _, o) <- andNotBGate
	return [o]

sampleOrNotBGate :: Sample
sampleOrNotBGate = (, (950, "orNotBGate.svg")) . (`runState` initCBState) $ do
	(_, _, o) <- orNotBGate
	return [o]

sample2 :: Sample
sample2 = (, (950, "sample2.svg")) . (`runState` initCBState) $ do
	(_, no) <- notGate
	(a, _, o) <- andGate
	(ni', no') <- notGate
	connectWire64 no a
	connectWire64 o ni'
	return [no']

sampleBranch :: Sample
sampleBranch = (, (950, "branch.svg")) . (`runState` initCBState) $ do
	(_ni, no) <- notGate
	(_ni', no') <- notGate
	(ni'', no'') <- notGate
	connectWire (no, 32, 0) (ni'', 32, 32)
	connectWire (no', 32, 0) (ni'', 32, 0)
	return [no'']

sampleBranch2 :: Sample
sampleBranch2 = (, (950, "branch2.svg")) . (`runState` initCBState) $ do
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
	return [o]

sampleTriGate :: Sample
sampleTriGate = (, (950, "triGate.svg")) . (`runState` initCBState) $ do
	(_a1, _b1, o1) <- triGate
	(_a2, _b2, o2) <- triGate
	(oin, oout) <- idGate
	connectWire64 o1 oin
	connectWire64 o2 oin
	return [oout]

sampleDelayGate :: Sample
sampleDelayGate = (, (950, "delayGate.svg")) . (`runState` initCBState) $ do
	(_ni0, no0) <- notGate
	(_ni1, no1) <- notGate
	(a, b, o) <- andGate
	(ni, no) <- notGate
	delay ni 255
	connectWire64 o ni
	connectWire64 no0 a
	delay a 123
	connectWire64 no1 b
	delay b 5
	return [no]

sampleDelayTriGate :: Sample
sampleDelayTriGate = (, (950, "delayTriGate.svg")) . (`runState` initCBState) $ do
	(_ni, no) <- notGate
	(_a, _b, o) <- andGate
	(ta, tb, tout) <- triGate
	connectWire64 no ta
	delay ta 55
	connectWire64 o tb
	delay tb 99
	return [tout]

sampleConstGate :: Sample
sampleConstGate = (, (950, "constGate.svg")) . (`runState` initCBState) $ (: []) <$> constGate 0xf0f0f0f0f0f0f0f0

sampleMultipleAnd :: Sample
sampleMultipleAnd = (, (950, "multipleAnd.svg")) . (`runState` initCBState) $ (: []) . snd <$> multiple andGate 25

sampleMultipleOr :: Sample
sampleMultipleOr = (, (950, "multipleOr.svg")) . (`runState` initCBState) $ (: []) . snd <$> multiple orGate 31

sampleMultipleXor :: Sample
sampleMultipleXor = (, (950, "multipleXor.svg")) . (`runState` initCBState) $ (: []) . snd <$> multiple xorGate 43

sampleDecoder :: Sample
sampleDecoder = (, (950, "decodre.svg")) . (`runState` initCBState) $ snd <$> decoder 8

sampleMux4 :: Sample
sampleMux4 = (, (950, "mux4.svg")) . (`runState` initCBState) $ (\(_, _, o) -> [o]) <$> multiplexer 4

sampleMux13 :: Sample
sampleMux13 = (, (950, "mux13.svg")) . (`runState` initCBState) $ (\(_, _, o) -> [o]) <$> multiplexer 13

sampleSrlatch :: Sample
sampleSrlatch = (, (950, "srlatch.svg")) . (`runState` initCBState)
	$ (\(_, _, q, q_) -> [q, q_]) <$> srlatch

sampleDlatch :: Sample
sampleDlatch = (, (950, "dlatch.svg")) . (`runState` initCBState) $ (\(_, _, q, q_) -> [q, q_]) <$> dlatch

sampleDflipflop :: Sample
sampleDflipflop = (, (950, "dflipflop.svg")) . (`runState` initCBState)
	$ (\(_, _, q, q_) -> [q, q_]) <$> dflipflop

samplePla8 :: Sample
samplePla8 = (, (2000, "pla8.svg")) . (`runState` initCBState)
	$ (: []) . snd <$> pla8 [(3, 8), (9, 7), (15, 123)]

sampleZeroDetector :: Sample
sampleZeroDetector = (, (950, "zeroDetector.svg")) . (`runState` initCBState) $ (: []) . snd <$> zeroDetector

sampleCarryLookahead :: Word8 -> Sample
sampleCarryLookahead n = (, (2000, "carryLookahead.svg")) . (`runState` initCBState)
	$ (\(_, _, _, cs, c) -> [cs, c]) <$> carriesN n
