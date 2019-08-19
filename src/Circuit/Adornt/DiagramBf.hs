{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.DiagramBf where

import Data.Maybe
import Data.Map.Strict
import Circuit.Adornt.Builder
import Circuit.DiagramDsl

import qualified Data.ByteString.Char8 as BSC

data ElemId
	= EidOWire OWire
	| EidLabel IWire Int
	deriving Show

instance ElementIdable ElemId where
	elementId (EidOWire o) = "OWire-" <> BSC.pack (show o)
	elementId (EidLabel i n) =
		"Label-" <> BSC.pack (show i) <> "-" <> BSC.pack (show n)

type Connection = ElemId -> DiagramMapM ()

diagramBfM :: CBState -> [(Maybe (Connection, Pos), OWire)] -> DiagramMapM ()
diagramBfM _ [] = return ()
diagramBfM cbs ((mpre, o) : os) = do
	os' <- case cbsGate cbs !? o of
		Just e -> do
			iwcps <- case e of
				IdGate iw' -> do
					me <- case mpre of
						Nothing -> putElementEnd eid hLineD
						Just (_, pos) -> putElement eid hLineD pos
					let	mcon = connectLine0 <$> me
					mpos <- maybe (return Nothing) ((Just <$>) . inputPosition0) me
					return . maybeToList $ (iw',) <$> ((,) <$> mcon <*> mpos)
				NotGate iw' -> do
					me <- case mpre of
						Nothing -> putElementEnd eid notGateD
						Just (_, pos) -> putElement eid notGateD pos
					let	mcon = connectLine0 <$> me
					mpos <- maybe (return Nothing) ((Just <$>) . inputPosition0) me
					return . maybeToList $ (iw',) <$> ((,) <$> mcon <*> mpos)
				OrGate iw1 iw2 -> do
					me <- case mpre of
						Nothing -> putElementEnd eid orGateD
						Just (_, pos) -> putElement eid orGateD pos
					let	mcon1 = connectLine1 <$> me
						mcon2 = connectLine2 <$> me
					mpos1 <- maybe (return Nothing) ((Just <$>) . inputPosition1) me
					mpos2 <- maybe (return Nothing) ((Just <$>) . inputPosition2) me
					return $ catMaybes [
						(iw1 ,) <$> ((,) <$> mcon1 <*> mpos1),
						(iw2 ,) <$> ((,) <$> mcon2 <*> mpos2) ]
				AndGate iw1 iw2 -> do
					me <- case mpre of
						Nothing -> putElementEnd eid andGateD
						Just (_, pos) -> putElement eid andGateD pos
					let	mcon1 = connectLine1 <$> me
						mcon2 = connectLine2 <$> me
					mpos1 <- maybe (return Nothing) ((Just <$>) . inputPosition1) me
					mpos2 <- maybe (return Nothing) ((Just <$>) . inputPosition2) me
					return $ catMaybes [
						(iw1 ,) <$> ((,) <$> mcon1 <*> mpos1),
						(iw2 ,) <$> ((,) <$> mcon2 <*> mpos2) ]
			case mpre of
				Nothing -> return ()
				Just (con, _) -> con eid
			case iwcps of
				[(iw, (con, pos))] -> nextDiagramMBf cbs iw con pos
				[(iw1, cp1), (iw2, cp2)] -> (++)
					<$> uncurry (nextDiagramMBf cbs iw1) cp1
					<*> uncurry (nextDiagramMBf cbs iw2) cp2
				[] -> return []
				_ -> error "Oops!!!"
		Nothing -> return []
	diagramBfM cbs $ os ++ os'
	where
	eid = EidOWire o

mkLabel :: FromOWire -> (String, String)
mkLabel ((lo, poso), (li, posi)) =
	(show msbo ++ ":" ++ show lsbo, show msbi ++ ":" ++ show lsbi)
	where
	msbo = poso + lo - 1; lsbo = poso
	msbi = posi + li - 1; lsbi = posi


nextDiagramMBf :: CBState -> IWire -> Connection -> Pos ->
	DiagramMapM [(Maybe (Connection, Pos), OWire)]
nextDiagramMBf cbs iw conn pos = case cbsWireConn cbs !? iw of
	Just owfos -> nextDiagramMBfList iw 0 owfos conn pos
	Nothing -> return []

nextDiagramMBfList :: IWire -> Int -> [(OWire, FromOWire)] -> Connection -> Pos ->
	DiagramMapM [(Maybe (Connection, Pos), OWire)]
nextDiagramMBfList _ _ [] _ _ = return []
nextDiagramMBfList iw n [(ow, fo)] conn pos = do
	lbl <- newElement eid (uncurry hLineTextD $ mkLabel fo) pos
	conn eid
	let	c = connectLine0 lbl
	p <- inputPosition0 lbl
	return [(Just (c, p), ow)]
	where eid = EidLabel iw n
nextDiagramMBfList iw n ((ow, fo) : owfos) conn pos = do
	br <- newElement eid0 branchD pos
	conn eid0
	p0 <- inputPosition1 br
	lbl <- newElement eid1 (uncurry hLineTextD $ mkLabel fo) p0
	connectLine1 br eid1
	let	c = connectLine0 lbl
	p <- inputPosition0 lbl
	let	c' = connectLine2 br
	p' <- inputPosition2 br
	((Just (c, p), ow) :) <$> nextDiagramMBfList iw (n + 2) owfos c' p'
	where
	eid0 = EidLabel iw n
	eid1 = EidLabel iw $ n + 1
