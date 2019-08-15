{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CarryLookaheadCommon where

import Control.Monad

import Circuit.Adornt.Builder

generateGp :: CircuitBuilder (IWire, IWire, OWire, OWire)
generateGp = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, g) <- andGate
	(oa, ob, p) <- orGate
	zipWithM_ connectWire64 [aout, bout, aout, bout] [aa, ab, oa, ob]
	return (ain, bin, g, p)
