{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CarryLookahead2 (carries, carries8, carriesN) where

import Data.Word

import Circuit.Adornt.Builder
import CarryLookaheadCommon

raiseGp :: Word8 -> CircuitBuilder (IWire, IWire, OWire, OWire)
raiseGp n = do
	(gin, gout) <- idGate
	(pin, pout) <- idGate
	(p1g, g0, p1g0) <- andGate
	(g1, p1g0', rgout) <- orGate
	(p1p, p0, rpout) <- andGate
	connectWire (pout, m, n) (p1g, m, 0)
	connectWire (gout, m, 0) (g0, m, 0)
	connectWire (gout, m, n) (g1, m, 0)
	connectWire (p1g0, m, 0) (p1g0', m, 0)
	connectWire (pout, m, n) (p1p, m, 0)
	connectWire (pout, m, 0) (p0, m, 0)
	return (gin, pin, rgout, rpout)
	where m = 64 - n

raisingGp :: Word8 -> CircuitBuilder (IWire, IWire, [OWire], [OWire])
raisingGp 0 = do
	(a, b, g1, p1) <- generateGp
	return (a, b, [g1], [p1])
raisingGp n = do
	(gn, pn, rgn, rpn) <- raiseGp n
	(a, b, rga, rpa) <- raisingGp (n `div` 2)
	let	rg : _ = rga
		rp : _ = rpa
	connectWire64 rg gn
	connectWire64 rp pn
	return (a, b, rgn : rga, rpn : rpa)

carries, carries8 :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
carries = do
	(ain, bin, gs, ps) <- raisingGp 32
	cs@(csin, csout) <- idGate
	co <- makeCarries 0 64 gs ps cs
	return (csin, ain, bin, csout, co)

carries8 = do
	(ain, bin, gs, ps) <- raisingGp 4
	cs@(csin, csout) <- idGate
	co <- makeCarries 0 8 gs ps cs
	return (csin, ain, bin, csout, co)

carriesN :: Word8 -> CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
carriesN n = do
	(ain, bin, gs, ps) <- raisingGp (n `div` 2)
	cs@(csin, csout) <- idGate
	co <- makeCarries 0 n gs ps cs
	return (csin, ain, bin, csout, co)

makeCarries :: Word8 -> Word8 -> [OWire] -> [OWire] -> (IWire, OWire) -> CircuitBuilder OWire
makeCarries f t [g] [p] (_csin, csout) | f + 1 == t = generateCarry f g p csout
makeCarries f t (g : gs) (p : ps) cs@(csin, csout) = do
	co <- makeCarries f m gs ps cs
	_ <- makeCarries m t gs ps cs
	connectWire (co, 1, 0) (csin, 1, m)
	generateCarry f g p csout
	where m = (t - f) `div` 2 + f
makeCarries _ _ _ _ _ = error "circuit.adornt.Sample.CarryLookaphead2.makeCarries _ _ _ _"

generateCarry :: Word8 -> OWire -> OWire -> OWire -> CircuitBuilder OWire
generateCarry f g p cs = do
	(p', ci, pci) <- andGate
	(g', pci', co) <- orGate
	connectWire (g, 1, f) (g', 1, 0)
	connectWire (p, 1, f) (p', 1, 0)
	connectWire (cs, 1, f) (ci, 1, 0)
	connectWire0 pci pci'
	return co
