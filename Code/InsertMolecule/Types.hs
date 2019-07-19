{-|
Module      : Types
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental

This module contains the description of
auxiliary types used by the program.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK prune#-}

module InsertMolecule.Types where

import Control.Lens
import Numeric.LinearAlgebra.Data
import qualified Data.Map as Map

type Point = Vector Double

-- | This type describes the structure of @Atom@.
data Atom = Atom
  { _adtype :: String -- ^ "ATOM"/"HETATM"
  , _aname :: String -- ^ Atom name
  , _aresname :: String -- ^ Residue name
  , _aresseq :: Int -- ^ Residue sequence number
  , _acoordin :: Point -- ^ (X,Y,Z) orthogonal Å coordinate
  }
  deriving (Show)
  
newAtom :: Atom
newAtom = Atom 
  { _adtype = "ATOM"
  , _aname = ""
  , _aresname = ""
  , _aresseq = 0
  , _acoordin = vector [0,0,0]
  }

-- | This type describes the structure of a single atom
-- in the @z-matrix@ representation.
data ZAtom = ZAtom
  { _atom :: Atom
  , _atomid :: Int
  , _con :: Int
  , _dist :: Double
  , _valcon :: Int
  , _valangl :: Double
  , _dihcon :: Int
  , _dihangl :: Double
  }
  deriving (Show)

newZAtom :: ZAtom
newZAtom = ZAtom 
  { _atom = newAtom
  , _atomid = 0
  , _con = 0
  , _dist = 0
  , _valcon = 0
  , _valangl = 0
  , _dihcon = 0
  , _dihangl = 0
  }

makeLenses ''Atom
makeLenses ''ZAtom

-- | This type describes a molecule.
type Molecule = Map.Map Int Atom
newMolecule = Map.empty

-- | This type describes molecules in the @z-matrix@ representation.
type ZMolecule = [ZAtom]
newZMolecule = []

addAtom :: (Int, Atom) -> Molecule -> Molecule
addAtom (id, atom) = Map.insert id atom