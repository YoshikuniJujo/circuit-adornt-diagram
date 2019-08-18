{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.DiagramBf where

import Data.Map.Strict
import Circuit.Adornt.Builder
import Circuit.DiagramDsl

import qualified Data.ByteString.Char8 as BSC
import qualified Circuit.DiagramDslOld as O

newtype ElemId = EidOWire OWire deriving Show

instance ElementIdable ElemId where
	elementId (EidOWire o) = "OWire-" <> BSC.pack (show o)

type Connection = ElemId -> DiagramMapM ()

diagramBfM2 :: CBState -> [(Maybe (Connection, Pos), OWire)] -> DiagramMapM ()
diagramBfM2 _ [] = return ()
diagramBfM2 cbs ((mpre, o) : os) = do
	case cbsGate cbs !? o of
		Just e -> case e of
			NotGate iw -> case mpre of
				Nothing -> () <$ putElementEnd eid notGateD
	where
	eid = EidOWire o

diagramBfM :: CBState -> [(Maybe (ElemId, Int, Pos), OWire)] -> DiagramMapM ()
diagramBfM _ [] = return ()
diagramBfM cbs ((mpre, o) : os) = do
	os' <- case cbsGate cbs !? o of
		Just e -> do
			miwps <- case e of
				NotGate iw -> do
					mlps <- case mpre of
						Nothing -> O.putElement0 eid O.notGateD
						Just (preid, _, ips) -> O.putElement eid O.notGateD ips
					mps <- case mlps of
						Just lps -> Just <$> O.inputPosition lps
						Nothing -> return Nothing
					return $ (: []) . (iw ,) <$> mps
				OrGate iw1 iw2 -> do
					mlps <- case mpre of
						Nothing -> O.putElement0 eid O.orGateD
						Just (preid, _, ips) -> O.putElement eid O.orGateD ips
					mps <- case mlps of
						Just lps -> (Just <$>)
							$ (\a b -> [a, b]) <$> O.inputPosition1 lps <*> O.inputPosition2 lps
						Nothing -> return Nothing
					return $ zipWith (,) [iw1, iw2] <$> mps
			case mpre of
				Just (p, 0, _) -> O.connectLine p eid
				Just (p, 1, _) -> O.connectLine1 p eid
				Just (p, 2, _) -> O.connectLine2 p eid
				Nothing -> return ()
			case miwps of
				Just [(iw, ps)] -> do
					return $ case cbsWireConn cbs !? iw of
						Just ((ow, _) : _) -> [(Just (eid, 0, ps), ow)]
						Nothing -> []
				Just [(iw1, ps1), (iw2, ps2)] -> do
					let	r1 = case cbsWireConn cbs !? iw1 of
							Just ((ow1, _) : _) -> [(Just (eid, 1, ps1), ow1)]
							Nothing -> []
						r2 = case cbsWireConn cbs !? iw2 of
							Just ((ow2, _) : _) -> [(Just (eid, 2, ps2), ow2)]
							Nothing -> []
					return $ r1 ++ r2
				Nothing -> return []
	diagramBfM cbs $ os ++ os'
	where
	eid = EidOWire o
