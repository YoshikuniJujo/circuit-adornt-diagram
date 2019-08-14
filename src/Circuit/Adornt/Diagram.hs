{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Diagram where

import Control.Arrow
import Control.Monad.State
import Data.Map.Strict

import qualified Data.ByteString.Char8 as BSC

import Circuit.DiagramDsl
import Circuit.Adornt.Builder

data BG
	= BG BasicGate
	| BGConst OWire
	| BGLabel BasicGate Int
	| BGBranch BasicGate Int
	| BGTri OWire
	| BGLabelTri OWire
	| BGBranchTri OWire
	| BGDelay IWire
	deriving Show

instance ElementIdable BG where
	elementIdGen (BG (AndGate iw1 iw2)) =
		"AndGate-" <> BSC.pack (show iw1) <> "-" <> BSC.pack (show iw2)
	elementIdGen (BG (OrGate iw1 iw2)) =
		"OrGate-" <> BSC.pack (show iw1) <> "-" <> BSC.pack (show iw2)
	elementIdGen (BG (NotGate iw)) = "NotGate-" <> BSC.pack (show iw)
	elementIdGen (BG (IdGate iw)) = "IdGate-" <> BSC.pack (show iw)
	elementIdGen (BGConst ow) = "ConstGate-" <> BSC.pack (show ow)
	elementIdGen (BGLabel bg n) = "Label-" <> elementIdGen (BG bg) <> "-" <> BSC.pack (show n)
	elementIdGen (BGBranch bg n) = "Branch-" <> elementIdGen (BG bg) <> "-" <> BSC.pack (show n)
	elementIdGen (BGTri ow) = "TriGate-" <> BSC.pack (show ow)
	elementIdGen (BGLabelTri ow) = "LabelTri-" <> BSC.pack (show ow)
	elementIdGen (BGBranchTri ow) = "BranchTri-" <> BSC.pack (show ow)
	elementIdGen (BGDelay iw) = "Delay-" <> BSC.pack (show iw)
	elementIdGen bg = error $ "ElementIdable BG: elementIdGen " ++ show bg

diagramM :: CBState -> [OWire] -> DiagramMapM ()
diagramM cbs = mapM_ (diagramM1 cbs Nothing)

diagramM1 :: CBState -> Maybe Pos -> OWire -> DiagramMapM BG
diagramM1 cbs mpos o@(OWire _ Nothing) =diagramMGen cbs mpos [o]
diagramM1 cbs mpos o@(OWire _ (Just iw)) = do
	mlp <- case mpos of
		Nothing -> putElement0 (BGTri o) (triGateD "0:0" "63:0")
		Just pos -> putElement (BGTri o) (triGateD "0:0" "63:0") pos
	case mlp of
		Just lp -> do
			ip1 <- inputPosition1 lp
			case cbsWireConn cbs !? iw of
				Just ofos -> do
					mdps <- checkDelay cbs ip1 iw
					case mdps of
						Just dps -> do
							bg' <- nextDiagramTriMList cbs o dps ofos
							connectLine1 (BGTri o) (BGDelay iw)
							connectLine (BGDelay iw) bg'
						Nothing -> do
							bg' <- nextDiagramTriMList cbs o ip1 ofos
							connectLine1 (BGTri o) bg'
				Nothing -> return ()
			ip2 <- inputPosition2 lp
			bg <- diagramMGen cbs (Just ip2) [o]
			connectLine2 (BGTri o) bg
			return (BGTri o)
		Nothing -> lift $ Left "diagramM: yet"

nextDiagramTriMList ::
	CBState -> OWire -> Pos -> [(OWire, FromOWire)] -> DiagramMapM BG
nextDiagramTriMList _ _ _ [] = lift $ Left "nextDiagramMList: shouldn't take null"
nextDiagramTriMList cbs e ip [ofo] = do
	nbg <- nextDiagramTriM1 cbs e ip ofo
	return nbg
nextDiagramTriMList cbs e ip (ofo : ofos) = do
	lp <- newElement (BGBranchTri e) branchD ip
	ip1 <- inputPosition1 lp
	ip2 <- inputPosition2 lp
	nbg <- nextDiagramTriM1 cbs e ip1 ofo
	connectLine1 (BGBranchTri e) nbg
	nbg' <- nextDiagramTriMList cbs e ip2 ofos
	connectLine2 (BGBranchTri e) nbg'
	return (BGBranchTri e)

nextDiagramTriM1 ::
	CBState -> OWire -> Pos -> (OWire, FromOWire) -> DiagramMapM BG
nextDiagramTriM1 cbs e ip (o', fo) = do
	ip' <- inputPosition
		=<< newElement (BGLabelTri e) (uncurry hLineTextD $ mkLabel fo) ip
	bg <- diagramM1 cbs (Just ip') o'
	connectLine (BGLabelTri e) bg
	return $ BGLabelTri e

diagramMGen :: CBState -> Maybe Pos -> [OWire] -> DiagramMapM BG
diagramMGen cbs mpos [o] = case cbsGate cbs  !? o of
	Just e -> do
		mipsiws <- case e of 
			AndGate iw1 iw2 -> do
				(ip1, ip2) <- (\(m1, m2) -> (,) <$> m1 <*> m2) . (inputPosition1 &&& inputPosition2)
					=<< lift . maybe (Left "Oops2") Right =<< case mpos of
						Nothing -> putElement0 (BG e) andGateD
						Just ps -> putElement (BG e) andGateD ps
				return $ Just ([ip1, ip2], [iw1, iw2])
			OrGate iw1 iw2 -> do
				(ip1, ip2) <- (\(m1, m2) -> (,) <$> m1 <*> m2) . (inputPosition1 &&& inputPosition2)
					=<< lift . maybe (Left "Oops2") Right =<< case mpos of
						Nothing -> putElement0 (BG e) orGateD
						Just ps -> putElement (BG e) orGateD ps
				return $ Just ([ip1, ip2], [iw1, iw2])
			NotGate iw' -> do
				ip' <- inputPosition =<< lift . maybe (Left "Oops3") Right =<< case mpos of
					Nothing -> putElement0 (BG e) notGateD
					Just ps -> putElement (BG e) notGateD ps
				return $ Just ([ip'], [iw'])
			IdGate iw' -> do
				mlp <- case mpos of
					Nothing -> putElement0 (BG e) hLineD
					Just ps -> putElement (BG e) hLineD ps
				mip' <- maybe (return Nothing) ((Just <$>) . inputPosition) mlp
				return $ (, [iw']) . (: []) <$> mip'
			ConstGate c -> do
				mlp <- case mpos of
					Nothing -> putElement0 (BGConst o) (constGateD c)
					Just ps -> putElement (BGConst o) (constGateD c) ps
				maybe (return Nothing) (const $ return (Just ([], []))) mlp
--			bg -> lift . Left $ "diagramM " ++ show bg
		maybe (return ()) (uncurry $ nextDiagramM cbs e) mipsiws
		return $ case mipsiws of
			Just ([], []) -> BGConst o
			_ -> BG e
	_ -> lift $ Left "not yet implemented 2"
diagramMGen _ _ _ = lift $ Left "not yet implemented 3"

checkDelay :: CBState -> Pos -> IWire -> DiagramMapM (Maybe Pos)
checkDelay cbs ip iw = case cbsDelay cbs !? iw of
		Just d -> Just <$> (inputPosition =<< newElement (BGDelay iw) (delayD d) ip)
		Nothing -> return Nothing

nextDiagramM :: CBState -> BasicGate -> [Pos] -> [IWire] -> DiagramMapM ()
nextDiagramM cbs e [ip] [iw] = do
	case cbsWireConn cbs !? iw of
		Just ofos -> do
			mdps <- checkDelay cbs ip iw
			case mdps of
				Just dps -> do
					(_, nbg) <- nextDiagramMList cbs e dps ofos 0
					connectLine (BG e) (BGDelay iw)
					connectLine (BGDelay iw) nbg
				Nothing -> do
					(_, nbg) <- nextDiagramMList cbs e ip ofos 0
					connectLine (BG e) nbg
		Nothing -> return ()
nextDiagramM cbs e [ip1, ip2] [iw1, iw2] = do
	n <- case cbsWireConn cbs !? iw1 of
		Just ofos -> do
			mdps <- checkDelay cbs ip1 iw1
			case mdps of
				Just dps -> do
					(n, nbg) <- nextDiagramMList cbs e dps ofos 1
					connectLine1 (BG e) (BGDelay iw1)
					connectLine (BGDelay iw1) nbg
					return n
				Nothing -> do
					(n, nbg) <- nextDiagramMList cbs e ip1 ofos 1
					connectLine1 (BG e) nbg
					return n
		Nothing -> return 1
	case cbsWireConn cbs !? iw2 of
		Just ofos -> do
			mdps <- checkDelay cbs ip2 iw2
			case mdps of
				Just dps -> do
					(_, nbg) <- nextDiagramMList cbs e dps ofos n
					connectLine2 (BG e) (BGDelay iw2)
					connectLine (BGDelay iw2) nbg
					return ()
				Nothing -> do
					(_, nbg) <- nextDiagramMList cbs e ip2 ofos n
					connectLine2 (BG e) nbg
					return ()
		Nothing -> return ()
nextDiagramM _ _ [] [] = return ()
nextDiagramM _ _ _ _ = lift $ Left "Oops!!!!!!!!!!!!!!!!!!"

nextDiagramMList :: CBState ->
	BasicGate -> Pos -> [(OWire, FromOWire)] -> Int -> DiagramMapM (Int, BG)
nextDiagramMList _ _ _ [] _ = lift $ Left "nextDiagramMList: shouldn't take null"
nextDiagramMList cbs e ip [ofo] n = do
	nbg <- nextDiagramM1 cbs e ip ofo n
	return (n + 1, nbg)
nextDiagramMList cbs e ip (ofo : ofos) n = do
	lp <- newElement (BGBranch e n) branchD ip
	ip1 <- inputPosition1 lp
	ip2 <- inputPosition2 lp
	nbg <- nextDiagramM1 cbs e ip1 ofo n
	connectLine1 (BGBranch e n) nbg
	(n', nbg') <- nextDiagramMList cbs e ip2 ofos (n + 1)
	connectLine2 (BGBranch e n) nbg'
	return (n' + 1, BGBranch e n)

nextDiagramM1 :: CBState ->
	BasicGate -> Pos -> (OWire, FromOWire) -> Int -> DiagramMapM BG
nextDiagramM1 cbs e ip (o', fo) n = do
	ip' <- inputPosition
		=<< newElement (BGLabel e n) (uncurry hLineTextD $ mkLabel fo) ip
	bg <- diagramM1 cbs (Just ip') o'
	connectLine (BGLabel e n) bg
	return $ BGLabel e n

mkLabel :: FromOWire -> (String, String)
mkLabel ((lo, poso), (li, posi)) =
	(show msbo ++ ":" ++ show lsbo, show msbi ++ ":" ++ show lsbi)
	where
	msbo = poso + lo - 1; lsbo = poso
	msbi = posi + li - 1; lsbi = posi
