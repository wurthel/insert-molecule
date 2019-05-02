{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Category
import Data.Label
import qualified Data.Map as Map
import Prelude hiding ((.), id)

-- | НАЧАЛО. ОПИСАНИЕ ТИПОВ.
newtype ID =
  ID Int
  deriving (Num, Enum, Eq, Ord, Show)

newtype Type =
  Type String
  deriving (Show)

newtype Element =
  Element String
  deriving (Show)

newtype Resname =
  Resname String
  deriving (Show)

newtype Resseq =
  Resseq Int
  deriving (Num, Enum, Eq, Ord, Show)

type CompAccur = Double

type Point = (CompAccur, CompAccur, CompAccur)

type Radius = CompAccur

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

data ZAtom =
  ZAtom
    { _atom :: Atom
    , _atomid :: Maybe ID
    , _atomcon :: Maybe ID
    , _bonddist :: Maybe CompAccur
    , _anglcon :: Maybe ID
    , _bondangl :: Maybe CompAccur
    , _dihedcon :: Maybe ID
    , _dihedangl :: Maybe CompAccur
    }
  deriving (Show)

type Molecule = Map.Map ID Atom

type ZMolecule = [ZAtom]

mkLabels [''Atom, ''ZAtom]

-- | КОНЕЦ. ОПИСАНИЕ ТИПОВ.
-- | НАЧАЛО. КОНСТРУКТОРЫ.
-- Простейшие конструкторы пустых значений типов
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

newMolecule :: Molecule
newMolecule = Map.empty

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

newZMolecule :: ZMolecule
newZMolecule = mempty

addAtom :: (ID, Atom) -> Molecule -> Molecule
addAtom (id, atom) = Map.insert id atom
-- | КОНЕЦ. КОНСТРУКТОРЫ.
