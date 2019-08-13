{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Adornt.Diagram where

import Control.Arrow
import Control.Monad.State
import Data.Map.Strict

import qualified Data.ByteString.Char8 as BSC

import Circuit.DiagramDsl
import Circuit.Adornt.Builder

newtype BG = BG BasicGate deriving Show

instance ElementIdable BG where
	elementIdGen (BG (AndGate iw1 iw2)) =
		"AndGate-" <> BSC.pack (show iw1) <> "-" <> BSC.pack (show iw2)
	elementIdGen (BG (OrGate iw1 iw2)) =
		"OrGate-" <> BSC.pack (show iw1) <> "-" <> BSC.pack (show iw2)
	elementIdGen (BG (NotGate iw)) = "NotGate-" <> BSC.pack (show iw)
	elementIdGen (BG (IdGate iw)) = "IdGate-" <> BSC.pack (show iw)
	elementIdGen bg = error $ "ElementIdable BG: elementIdGen " ++ show bg

diagramM :: CBState -> Maybe Pos -> [OWire] -> DiagramMapM BasicGate
diagramM cbs mpos [o] = case cbsGate cbs  !? o of
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
			bg -> lift . Left $ "diagramM " ++ show bg
		maybe (return ()) (uncurry $ nextDiagramM cbs e) mipsiws
		return e
	_ -> lift $ Left "not yet implemented 2"
diagramM _ _ _ = lift $ Left "not yet implemented 3"

nextDiagramM :: CBState -> BasicGate -> [Pos] -> [IWire] -> DiagramMapM ()
nextDiagramM cbs e [ip] [iw] = do
	case cbsWireConn cbs !? iw of
		Just [(o', _)] -> do
			bg <- diagramM cbs (Just ip) [o']
			connectLine (BG e) (BG bg)
			return ()
		Nothing -> return ()
		_ -> lift $ Left "not yet implemented 4"
nextDiagramM cbs e [ip1, ip2] [iw1, iw2] = do
	case cbsWireConn cbs !? iw1 of
		Just [(o', _)] -> do
			bg <- diagramM cbs (Just ip1) [o']
			connectLine1 (BG e) (BG bg)
		Nothing -> return ()
		_ -> lift $ Left "not yet implemented 5"
	case cbsWireConn cbs !? iw2 of
		Just [(o', _)] -> do
			bg <- diagramM cbs (Just ip2) [o']
			connectLine2 (BG e) (BG bg)
		Nothing -> return ()
		_ -> lift $ Left "not yet implemented 6"
nextDiagramM _ _ _ _ = lift $ Left "Oops!!!!!!!!!!!!!!!!!!"
