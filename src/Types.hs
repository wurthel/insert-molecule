{-|
Module      : Types
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental

Этот модуль содержит описание вспомогательных типов,
используемых программой. 
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
-- | Данный тип предназначен для указания номера атома в 'Molecule'.
newtype ID =
  ID Int
  deriving (Num, Enum, Eq, Ord, Show)

-- | Данный тип предназначен для указания типа атома в 'Atom'.
newtype Type =
  Type String
  deriving (Show)

-- | Данный тип предназначен для указания имени атомного элемента в 'Atom'.
newtype Element =
  Element String
  deriving (Show)

-- | Данный тип предназначен для указания полного именени атома (какой
-- он имел в исходной молекуле) в 'Atom'.
newtype Resname =
  Resname String
  deriving (Show)

-- | Данный тип предназначен для указания номера аминоксилоты,
-- которой принадлежит атом в исходной молекуле, в 'Atom'.
newtype Resseq =
  Resseq Int
  deriving (Num, Enum, Eq, Ord, Show)

-- | Данный тип предназначен для указания @XYZ@ координат атома в 'Atom'.
type Point = (Double, Double, Double)

-- | Данный тип предназначен для указания Ван-дер-Ваальсова
-- радиуса атома в 'Atom'.
type Radius = Double

-- | Данный тип описывает структуру @Atom@.
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

-- | Данный тип описывает структуру одного атома в @z-matrix@.
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

-- | Данный тип описывает молекулу, составленной из 'Atom'.
type Molecule = Map.Map ID Atom

-- | Данный тип описывает молекулы в @z-matrix@ представлении.
type ZMolecule = [ZAtom]

mkLabels [''Atom, ''ZAtom]

-- * Конструкторы пустых объектов
-- | Функция создает новый 'Atom' без каких-либо характеристик
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

-- | Функция создает пустую 'Molecule' 
-- 
-- >>> newMolecule
-- fromList []
newMolecule :: Molecule
newMolecule = Map.empty

-- | Функция создает пустую пустой 'Atom' в @z-matrix@ представлени
-- без каких-либо характеристик. 
-- 
-- >>> newZAtom
-- ZAtom {_atom = 
--        Atom  { _elem = Element "", 
--                _etype = Type "", 
--                _resname = Resname "",
--                _resseq = Resseq 0, 
--                _coordin = (0.0,0.0,0.0), 
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

-- | Функция создает пустую 'Molecule' в @z-matrix@ представлении
-- 
-- >>> newZMolecule
-- []
newZMolecule :: ZMolecule
newZMolecule = mempty

-- | Функция вставляет 'Atom' с указанным 'ID' в 'Molecule'.
addAtom :: (ID, Atom) -> Molecule -> Molecule
addAtom (id, atom) = Map.insert id atom
