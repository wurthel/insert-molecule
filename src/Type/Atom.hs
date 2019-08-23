{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Type.Atom where

import Control.Lens
import Linear.V3

type Serial = Int
type Resseq = Int
type Resname = String
type Element = String
type Point   = V3 Double

data Atom = Atom
  { _adatatype    :: String -- ^ "ATOM"/"HETATM"
  , _anumeric      :: Int -- ^ Atom serial number
  , _aname        :: String -- ^ Atom name
  , _aaltloc      :: String -- ^ Alternate location indicator. 
  , _aresname     :: String -- ^ Residue name
  , _achainid     :: String -- ^ Chain identifier
  , _aresseq      :: Int -- ^ Residue sequence number
  , _arescode     :: String -- ^ Code for insertions of residues
  , _acoordin     :: Point -- ^ (X,Y,Z) orthogonal Å coordinate
  , _aoccup       :: Double -- ^ Occupancy
  , _atempfac     :: Double -- ^ Temperature factor
  , _aelement     :: String -- ^ Element symbol
  , _acharge      :: String -- ^ Atom charge
  , _vdwr         :: Double
  }
  deriving (Show, Eq)
  
atom = Atom 
  { _adatatype = "ATOM"
  , _anumeric = 0
  , _aname = ""
  , _aaltloc = ""
  , _aresname = ""
  , _achainid = ""
  , _aresseq = 0
  , _arescode = ""
  , _acoordin = V3 0 0 0
  , _aoccup = 0
  , _atempfac = 0
  , _aelement = ""
  , _acharge = ""
  , _vdwr = 0
  }

makeLenses ''Atom

data ZAtom = ZAtom
  { _serial :: Int
  , _con :: Int
  , _dist :: Double
  , _valcon :: Int
  , _valangl :: Double
  , _dihcon :: Int
  , _dihangl :: Double
  , _name :: String
  }

zatom = ZAtom 
  { _serial = 0
  , _con = 0
  , _dist = 0
  , _valcon = 0
  , _valangl = 0
  , _dihcon = 0 
  , _dihangl = 0
  , _name = ""
  }

makeLenses ''ZAtom