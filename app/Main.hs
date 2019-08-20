{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Map.Strict
import System.Environment
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.DiagramDsl
-- import Circuit.Adornt.Diagram
import Circuit.Adornt.DiagramBf

import TryDiagram

main :: IO ()
main = do
	fp : s_ : sp_ : sl : args <- getArgs
	let	s = read s_
		sp = read sp_
	let	mowsng = case sl of
			"pla" -> Just . fst $ samplePla8
			"carry_lookahead" -> let
				n_ : _ = args
				n = read n_ in
				if n `elem` [1, 2, 4, 8, 16, 32, 64] then
					Just . fst $ sampleCarryLookahead n
				else Nothing
			_ -> Nothing
	case mowsng of
		Just (ows, ng) ->
			either error (renderSVG fp (mkWidth s) . drawDiagram)
				. (`execDiagramMapM` sp) $ diagramDfM0 ng (BlockDefinition empty) ows []
		Nothing -> do
			putStrLn "sl = pla of carry_lookahead"
			putStrLn "n = 1, 2, 4, 8, 16, 32 or 64"
