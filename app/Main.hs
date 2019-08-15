{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.DiagramDsl
import Circuit.Adornt.Diagram

import TryDiagram

main :: IO ()
main = do
	n_ : s_ : fp : _ <- getArgs
	let	n = read n_
		s = read s_
	if n `elem` [1, 2, 4, 8, 16, 32, 64] then do
		let	(ows, ng) = sampleCarryLookahead n
		either error (renderSVG fp (mkWidth s) . drawDiagram) $ (`execDiagramMapM` 4) $ diagramM ng ows
	else putStrLn "n = 1, 2, 4, 8, 16, 32 or 64"
