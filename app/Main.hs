{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.DiagramDsl
import Circuit.Adornt.Diagram

import TryDiagram

main :: IO ()
main = let (ows, ng) = sampleCarryLookahead 64 in
	either error (renderSVG "sample2.svg" (mkWidth 4000) . drawDiagram) $ (`execDiagramMapM` 4) $ diagramM ng ows
