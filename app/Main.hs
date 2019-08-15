{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.DiagramDsl
import Circuit.Adornt.Diagram

import TryDiagram

main :: IO ()
main = let (ows, ng) = samplePla8 in
	either error (renderSVG "sample.svg" (mkWidth 2200) . drawDiagram) $ (`execDiagramMapM` 7) $ diagramM ng ows
