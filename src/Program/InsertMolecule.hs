{-|
Module      : InsertMolecule
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental

The module describes the work with functions that perform the insertion.
-}
{-# LANGUAGE BangPatterns #-}

module Program.InsertMolecule where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Linear hiding (ei, ej, ek)
import Numeric (acos, sqrt)
import System.Process (callCommand)
import qualified Data.List as L
import qualified Data.IntMap as M

import Util.ReadWrite
import Util.TrigDegree
import Util.Operators
import Type.Atom
import Type.Molecule

alpha = [Degree 0, Degree 1 .. Degree 359]
beta = [Degree (-90), Degree (-89) .. Degree 90]
ei = V3 1 0 0
ej = V3 0 1 0
ek = V3 0 0 1

setAtomWithOutOptimization :: Int -> ZMolecule -> Molecule -> Molecule -> [Molecule]
setAtomWithOutOptimization n zMol oMol = undefined -- do
  -- let zAtom = (zMol ^. zatoms) !! n
  --     newAtom = uploadData zAtom atom
  --     [idA, idB, idC, idD] = map (zatom ^.) [serial, con, valcon, dihcon]
  --     [dist', valcon', dihcon'] = map (zatom ^.) [dist, valangl, dihangl]
  --     atomB = getAtomById idB oMol
  --     atomC = getAtomById idC oMol
  --     atomD = getAtomById idD oMol
  -- let possibleAtoms a b = rotateAtom ej b . rotateAtom ek a
  --     allVariance = possibleAtoms <$> map deg2Rad alpha <*> map deg2Rad beta
  -- goodVariance <- filter (\x -> not (x `isIntersection` oMol)) (allVariance atom) 
  -- return undefined -- $ M.insert atomID_B goodVariance insMol

setAtom12 :: (Atom, Atom, Double) -> Int -> Molecule -> [Molecule]
setAtom12 (a, b, x) i mol = do
  let b . acoordin = a ^. acoordin >-> (& _x) +~ x
      possibleCoord a b x = rotateAtom ej b (rotateAtom ek a x)
  map (\x -> addAtom (i, x) mol) (possibleCoord <$> alpha <*> beta <*> pure b)

setAtom13 :: (Atom, Atom, Atom, Double, Double) -> Molecule -> [Molecule]
setAtom13 (a, b, c, x, y) mol = undefined

setAtom14 :: (Atom, Atom, Atom, Atom, Double, Double, Double) -> Molecule -> [Molecule]
setAtom14 (a, b, c, d, x, y, z) mol = undefined

addAtom :: (Int, Atom) -> Molecule -> Molecule
addAtom (i, x) mol = mol & over atoms (M.insert i x)

getAtomById :: Int -> Molecule -> Atom
getAtomById id mol = 
  case M.lookup id (mol ^. atoms) of
    Just a -> a
    Nothing -> undefined

isIntersection :: Atom -> Molecule -> Bool
isIntersection i mol = or $ map (isIntersection' i) is
  where
    is = map snd (M.toList $ mol ^. atoms)
    isIntersection' a b = distance v1 v2 <= (r1 + r2)
      where 
        (v1, r1) = (a ^. acoordin, a ^. vdwr)
        (v2, r2) = (b ^. acoordin, b ^. vdwr)

rotateAtom :: Point -> Degree Double -> Atom -> Atom
rotateAtom axis angle atom = 
  let newCoord = rotate (axisAngle axis (deg2Rad angle)) (atom ^. acoordin)
  in atom & acoordin .~ newCoord

valAngle :: Atom -> Atom -> Double
valAngle a b = 
  let dist01 = norm (a ^. acoordin)
      dist02 = norm (b ^. acoordin)
      dist12 = norm (a ^. acoordin ^-^ b ^. acoordin)
      angl = (dist01 ** 2 + dist02 ** 2 - dist12 ** 2 ) / (2 * dist01 * dist02)
  in acos angl