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

module Types where

import Control.Category
import Data.Label
import qualified Data.Map as Map
import Prelude hiding ((.), id)

-- * Типы данных
-- | This type is intended to indicate the number of an atom in the 'Molecule'.
newtype ID =
  ID Int
  deriving (Num, Enum, Eq, Ord, Show)

-- | This type is intended to indicate the type of atom in 'Atom'.
newtype Type =
  Type String
  deriving (Show)

-- | This type is intended to indicate the type of atom in 'Atom'.
newtype Element =
  Element String
  deriving (Show)

-- | This type is intended to indicate the full name of the atom
-- (which it had in the original molecule) in 'Atom'.
newtype Resname =
  Resname String
  deriving (Show)

-- | This type is intended to indicate the number of amino acid
-- to which the atom in the parent molecule belongs, in 'Atom'.
newtype Resseq =
  Resseq Int
  deriving (Num, Enum, Eq, Ord, Show)

-- | This type is intended to indicate the @XYZ@ coordinates 
-- of the atom in 'Atom'.
type Point = (Double, Double, Double)

-- | This type is intended to specify the Van-der-Waals 
-- radius of the atom in 'Atom'.
type Radius = Double

-- | This type describes the structure of @Atom@.
data Atom =
  Atom
    { _elem :: Element
    , _etype :: Type
    , _resname :: Resname
    , _resseq :: Resseq
    , _coordin :: Point
    , _radius :: Radius
    }
  deriving (Show)

-- | This type describes the structure of a single atom
-- in the @z-matrix@ representation.
data ZAtom =
  ZAtom
    { _atom :: Atom
    , _atomid :: Maybe ID
    , _atomcon :: Maybe ID
    , _bonddist :: Maybe Double
    , _anglcon :: Maybe ID
    , _bondangl :: Maybe Double
    , _dihedcon :: Maybe ID
    , _dihedangl :: Maybe Double
    }
  deriving (Show)

-- | This type describes a molecule.
type Molecule = Map.Map ID Atom

-- | This type describes molecules in the @z-matrix@ representation.
type ZMolecule = [ZAtom]

mkLabels [''Atom, ''ZAtom]

-- * Empty object constructors.
-- | The function creates a new 'Atom' without any characteristics.
-- 
-- >>> newAtom
-- Atom  { _elem = Element "", 
--          _etype = Type "", 
--          _resname = Resname "",
--          _resseq = Resseq 0, 
--          _coordin = (0.0,0.0,0.0), 
--          _radius = 0.0}, 
newAtom :: Atom
newAtom =
  Atom
    { _elem = Element []
    , _etype = Type []
    , _resname = Resname []
    , _resseq = Resseq 0
    , _coordin = (0, 0, 0)
    , _radius = 0
    }

-- | The function creates an empty 'Molecule'
-- 
-- >>> newMolecule
-- fromList []
newMolecule :: Molecule
newMolecule = Map.empty

-- | The function creates an empty empty 'Atom' in the @z-matrix@ 
-- representation without any characteristics.
-- 
-- >>> newZAtom
-- ZAtom {_atom = 
--        Atom  { _elem = Element "", 
--                _etype = Type "", 
--                _resname = Resname "",
--                _resseq = Resseq 0, 
--                _coord = (0.0,0.0,0.0), 
--                _radius = 0.0}, 
--        _atomid = Nothing, 
--        _atomcon = Nothing, 
--        _bonddist = Nothing, 
--        _anglcon = Nothing, 
--        _bondangl = Nothing, 
--        _dihedcon = Nothing, 
--        _dihedangl = Nothing}
newZAtom :: ZAtom
newZAtom =
  ZAtom
    { _atom = newAtom
    , _atomid = Nothing
    , _atomcon = Nothing
    , _bonddist = Nothing
    , _anglcon = Nothing
    , _bondangl = Nothing
    , _dihedcon = Nothing
    , _dihedangl = Nothing
    }

-- | The function creates an empty 'Molecule' in the @z-matrix@ representation.
-- 
-- >>> newZMolecule
-- []
newZMolecule :: ZMolecule
newZMolecule = mempty

-- | The function inserts 'Atom' with the specified 'ID' into the 'Molecule'.
addAtom :: (ID, Atom) -> Molecule -> Molecule
addAtom (id, atom) = Map.insert id atom
