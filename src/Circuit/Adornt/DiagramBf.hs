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
	| EidTri OWire
	| EidDelay IWire
	deriving Show

instance ElementIdable ElemId where
	elementId (EidOWire o) = "OWire-" <> BSC.pack (show o)
	elementId (EidLabel i n) =
		"Label-" <> BSC.pack (show i) <> "-" <> BSC.pack (show n)
	elementId (EidTri o) = "TriGate-" <> BSC.pack (show o)
	elementId (EidDelay i) = "Delay-" <> BSC.pack (show i)

type Connection = ElemId -> DiagramMapM ()

diagramBfM :: CBState -> [(Maybe (Connection, Pos), OWire)] -> DiagramMapM ()
diagramBfM _ [] = return ()
diagramBfM cbs ((mpre, o@(OWire _ miw)) : os) = (diagramBfM cbs . (os ++) =<<) $ do
	(mpre', os') <- case miw of
		Just iw -> do
			me <- case mpre of
				Nothing -> putElementEnd eid (triGateD "0:0" "63:0")
				Just (conn, pos) -> do
					putElement eid (triGateD "0:0" "63:0") pos
						<* conn eid
			let	mcon0 = connectLine1 <$> me
			mpos0 <- maybe (return Nothing) ((Just <$>) . inputPosition1) me
			os'' <- case ((,) <$> mcon0 <*> mpos0) of
				Just (con0, pos0) -> nextDiagramMBf cbs iw con0 pos0
				Nothing -> return []
			let	mcon = connectLine2 <$> me
			mpos <- maybe (return Nothing) ((Just <$>) . inputPosition2) me
			return ((,) <$> mcon <*> mpos, os'')
		Nothing -> return (mpre, [])
	(os' ++) <$> diagramBfM1 cbs mpre' o
	where eid = EidTri o

diagramBfM1 :: CBState -> Maybe (Connection, Pos) -> OWire ->
	DiagramMapM [(Maybe (Connection, Pos), OWire)]
diagramBfM1 cbs mpre o = do
	case cbsGate cbs !? o of
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

checkDelay :: CBState -> Pos -> IWire -> DiagramMapM (Maybe (Connection, Pos))
checkDelay cbs ip iw = case cbsDelay cbs !? iw of
	Just d -> do
		e <- newElement (EidDelay iw) (delayD d) ip
		let	conn = connectLine0 e
		pos <- inputPosition0 e
		return (Just (conn, pos))
	Nothing -> return Nothing
