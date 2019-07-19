{-|
Module      : InsertMolecule
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental

The module describes the work with functions that perform the insertion.
-}
{-# LANGUAGE BangPatterns #-}

module InsertMolecule.InsertMolecule
  ( setAtomWithOutOptimization
  , setAtomWithOptimization
  ) where

import Control.Lens
import Control.DeepSeq
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import System.Directory
  ( doesFileExist
  , getCurrentDirectory
  , removeFile
  , renameFile
  , setCurrentDirectory
  )
import System.IO
import qualified System.IO.Strict as StrictIO
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callCommand)
import Text.Printf
import Text.Read (readMaybe)

import InsertMolecule.ReadWrite
import InsertMolecule.TrigDegree
import InsertMolecule.Types

-- | The function is intended for the sequential insertion 
-- of n atoms of the molecule in the z-matrix representation, 
-- located in the rows [1, n] (numbering with 0), without 
-- carrying out the optimization process. As a result, the 
-- first molecule is returned, into which n atoms could be inserted.
-- If there is no such molecule, then Nothing is returned.
setAtomWithOutOptimization ::
     Int -- ^ The number of inserted atoms.
  -> ZMolecule -- ^ The inserted molecule in the @ z-matrix @ representation.
  -> Molecule -- ^ The molecule in which the insertion is making.
  -> Maybe Molecule -- ^ The resulting molecule.
setAtomWithOutOptimization 0 _ mol = return mol
setAtomWithOutOptimization n zmatr mol = 
  listToMaybe $ setAtomWithOutOptimization' 1 n zmatr mol newMolecule
  where
    setAtomWithOutOptimization' m n zmatr originMol insMol
      | n < 1 = error "setAtomWithOutOptimization: @n@ must be great than 0"
      | m > n = return $ Map.union originMol insMol
      | m == 1 = concat $
        setAtomWithOutOptimization' (m + 1) n zmatr originMol <$>
        setAtomWithOutOptimization1 1 zmatr mol insMol
      | m == 2 = concat $
        setAtomWithOutOptimization' (m + 1) n zmatr originMol <$>
        setAtomWithOutOptimization2 2 zmatr mol insMol
      | otherwise = concat $
        setAtomWithOutOptimization' (m + 1) n zmatr originMol <$>
        setAtomWithOutOptimization3 m zmatr mol insMol

-- | The function is intended for the sequential insertion
-- of the atoms of the molecule from @ zmatr @, located
-- in the lines [s, e] (numbering with 0), and carrying out
-- the optimization process. As a result, the molecule returns
-- with the atoms inserted.
setAtomWithOptimization ::
     (FilePath, FilePath, FilePath, FilePath, FilePath) -- ^Supporting files for the optimizer. The order of the files in a tuple is as follows: 
                                                        -- 
                                                        -- (1) an optimized molecule; 
                                                        -- (2) a file indicating @ resid @, which will be used by the optimizer in its work; 
                                                        -- (3) folder where the optimizer is located; 
                                                        -- (4) script that performs optimization; 
                                                        -- (5) optimized molecule, the result of the optimizer
  -> (Int, Int) -- ^ The first and last atoms to be inserted.
  -> ZMolecule -- ^ The inserted molecule in the @ z-matrix @ submission&
  -> Molecule -- ^ The molecule in which the insertion is made.
  -> IO Molecule -- ^ Molecule with inserted atoms.
setAtomWithOptimization fns (s, e) zmatr mol
  | s <= 0 || e <= 0 = error "setAtomWithOptimization: @s@ and @e@ must be great than 0"
  | s > e = return mol
  | otherwise = do
    mol' <- setAtomWithOptimization3 fns s zmatr mol
    mol' `seq` setAtomWithOptimization fns (s + 1, e) zmatr mol'

-- The function is intended to insert the first atom of the 
-- molecule from @ZMolecule@ without the optimization process.
-- Returns all possible variants of the molecules with an insert.
-- If there are none, then an empty list is returned.
setAtomWithOutOptimization1 :: Int -> ZMolecule -> Molecule -> Molecule -> [Molecule]
setAtomWithOutOptimization1 n zMol originMol insMol = do
  let matrixAtom_B = zMol !! (n-1)
      atomID_B = view atomid matrixAtom_B
      atomID_A = view con matrixAtom_B
      dist_AB = view dist matrixAtom_B
      atom_B = view atom matrixAtom_B
      atom_A =
        fromMaybe (error "setAtomWithOutOptimization1: atom_A not found") $
        originMol Map.!? atomID_A
      [x_A, y_A, z_A] = toList $ view acoordin atom_A

      -- Отбираем только те молекулы, который находятся в окрестности @atom_A@.
      -- Для дальнейшего рассмотрения удаляем из рассмотрения тот атом,
      -- с которым связан прикрепляемый атом (то есть удаляем @atom_A@)
      m = 2
      r1 = vector [x_A - m * dist_AB, y_A - m * dist_AB, z_A - m * dist_AB]
      r2 = vector [x_A + m * dist_AB, y_A - m * dist_AB, z_A - m * dist_AB]
      r3 = vector [x_A - m * dist_AB, y_A + m * dist_AB, z_A - m * dist_AB]
      r4 = vector [x_A - m * dist_AB, y_A - m * dist_AB, z_A + m * dist_AB]
      ws = sortCoordinates [r1, r2, r3, r4]
      wsMols = Map.elems $ Map.filter (`isInWorkSpace` ws) $ Map.delete atomID_A originMol
        -- Вставляем @atom_B@ в @molecule@
        -- Функция, задающая координаты атома @atom_B@ в единой СК
        -- по углам alpha и beta в штрихованной СК.
      possibleAtom a b =
        let x = dist_AB * cosd a * sind b + x_A
            y = dist_AB * sind a * sind b + y_A
            z = dist_AB * cosd b + z_A
            v = vector [x, y, z]
         in set acoordin v atom_B
  let alpha = [Degree 0,Degree 1 .. Degree 359]
      beta = [Degree 0,Degree 1 .. Degree 179]
      allVariance = possibleAtom <$> alpha <*> beta
  goodVariance <- filter (\x -> not . or $ isIntersection <$> wsMols <*> pure x) allVariance
  return $ addAtom (atomID_B, goodVariance) insMol

-- The function is intended to insert the second atom of the 
-- molecule from @ZMolecule@ without the optimization process.
-- Returns all possible options for molecules with an insert.
-- If there are none, then an empty list is returned.
setAtomWithOutOptimization2 :: Int -> ZMolecule -> Molecule -> Molecule -> [Molecule]
setAtomWithOutOptimization2 n zMol originMol insMol = do
  let matrixAtom_C = zMol !! (n-1)
      atomID_C = view atomid matrixAtom_C
      atomID_B = view con matrixAtom_C
      atomID_A = view valcon matrixAtom_C
      dist_BC = view dist matrixAtom_C
      angl_ABC = toDegree $ view valangl matrixAtom_C
      atom_C = view atom matrixAtom_C
      atom_B =
        fromMaybe
          (fromMaybe (error "setAtomWithOutOptimization2: atom_B not found") $
           originMol Map.!? atomID_B) $
        insMol Map.!? atomID_B
      atom_A =
        fromMaybe
          (fromMaybe (error "setAtomWithOutOptimization2: atom_A not found") $
           originMol Map.!? atomID_A) $
        insMol Map.!? atomID_A
      [x_A, y_A, z_A] = toList $ view acoordin atom_A
      [x_B, y_B, z_B] = toList $ view acoordin atom_B
      dist_AB = sqrt $ (x_B - x_A) ^ 2 + (y_B - y_A) ^ 2 + (z_B - z_A) ^ 2
      
      -- Отбираем только те молекулы, которые находятся в окрестности @atom_B@.
      -- Для дальнейшего рассмотрения удаляем из рассмотрения тот атом,
      -- с которым связан прикрепляемый атом (то есть удаляем @atom_B@)
      m = 2
      r1 = vector [x_B - m * dist_BC, y_B - m * dist_BC, z_B - m * dist_BC]
      r2 = vector [x_B + m * dist_BC, y_B - m * dist_BC, z_B - m * dist_BC]
      r3 = vector [x_B - m * dist_BC, y_B + m * dist_BC, z_B - m * dist_BC]
      r4 = vector [x_B - m * dist_BC, y_B - m * dist_BC, z_B + m * dist_BC]
      ws = sortCoordinates [r1, r2, r3, r4]
      wsMol = Map.elems $ Map.filter (`isInWorkSpace` ws) $ Map.delete atomID_B originMol
      
      -- Ищем углы трансляции. Система координат левая.
      -- Поэтому положительным углам соответствует вращение по часовой стрелке.
      -- b1 - угол поворота вокруг оси Z
      -- b2 - угол поворота вокруг оси Y
      b1
        | xy == 0 = Degree 0
        | y'_B > 0 = acosd (x'_B / xy)
        | otherwise = -acosd (x'_B / xy)
        where
          [x'_B, y'_B] = [x_B - x_A, y_B - y_A]
          xy = sqrt $ x'_B ^ 2 + y'_B ^ 2
      b2
        | z'_B < 0 = acosd (xy / xyz)
        | otherwise = -acosd (xy / xyz)
        where
          [x'_B, y'_B, z'_B] = [x_B - x_A, y_B - y_A, z_B - z_A]
          xy = sqrt $ x'_B ^ 2 + y'_B ^ 2
          xyz = dist_AB
        
          -- Вставляем @atom_C@ в @molecule@
        -- Функция, задающая координаты атома @atom_C@  в единой СК
        -- по углу alpha в дважды штрихованной СК.
  let possibleAtom a =
        let h = dist_BC * sind angl_ABC
            x''_C = -dist_BC * cosd angl_ABC + dist_AB
            z''_C = h * sind a
            y''_C = h * cosd a
            x =
              x''_C * cosd b1 * cosd b2 - y''_C * sind b1 + z''_C * cosd b1 * sind b2 +
              x_A
            y =
              x''_C * sind b1 * cosd b2 + y''_C * cosd b1 +
              z''_C * sind b1 * sind b2 +
              y_A
            z = -x''_C * sind b2 + 0 + z''_C * cosd b2 + z_A
            v = vector [x,y,z]
         in set acoordin v atom_C
  let alpha = [Degree 0,Degree 1 .. Degree 359]
      allVariance = possibleAtom <$> alpha
  goodVariance <-
    filter (\x -> not . or $ isIntersection <$> wsMol <*> pure x) allVariance
  return $ addAtom (atomID_C, goodVariance) insMol

-- The function is intended to insert the third and all subsequent
-- atoms of the molecule from @ZMolecule@ without the optimization
-- process. Turns ONE possible variant of the molecule with the
-- insert. If there is none, then an empty list is returned.
setAtomWithOutOptimization3 :: Int -> ZMolecule -> Molecule -> Molecule -> [Molecule]
setAtomWithOutOptimization3 n zMol originMol insMol = do
  let matrixAtom_D = zMol !! (n-1)
      atomID_D = view atomid matrixAtom_D
      atomID_C = view con matrixAtom_D
      atomID_B = view valcon matrixAtom_D
      atomID_A = view dihcon matrixAtom_D
      dist_CD = view dist matrixAtom_D
      angl_BCD = toDegree $ view valangl matrixAtom_D
      angl_ABCD = toDegree $ view dihangl matrixAtom_D
      atom_D = view atom matrixAtom_D
      atom_C =
        fromMaybe
          (fromMaybe (error "setAtomWithOutOptimization3: atom_C not found") $
           originMol Map.!? atomID_C) $
        insMol Map.!? atomID_C
      atom_B =
        fromMaybe
          (fromMaybe (error "setAtomWithOutOptimization3: atom_B not found") $
           originMol Map.!? atomID_B) $
        insMol Map.!? atomID_B
      atom_A =
        fromMaybe
          (fromMaybe (error "setAtomWithOutOptimization3: atom_A not found") $
           originMol Map.!? atomID_A) $
        insMol Map.!? atomID_A
      [x_A, y_A, z_A] = toList $ view acoordin atom_A
      [x_B, y_B, z_B] = toList $ view acoordin atom_B
      [x_C, y_C, z_C] = toList $ view acoordin atom_C
      dist_BC = sqrt $ (x_C - x_B) ^ 2 + (y_C - y_B) ^ 2 + (z_C - z_B) ^ 2
      
      -- Отбираем только те молекулы, который находятся в окрестности @atom_B@.
      -- Для дальнейшего рассмотрения удаляем из рассмотрения тот атом,
      -- с которым связан прикрепляемый атом (то есть удаляем @atom_B@)
      -- !!! На самом деле здесь следует удалять из рассмотреия все атомы, с которыми связан вставлямый атом.
      m = 2
      r1 = vector [x_B - m * dist_CD, y_B - m * dist_CD, z_B - m * dist_CD]
      r2 = vector [x_B + m * dist_CD, y_B - m * dist_CD, z_B - m * dist_CD]
      r3 = vector [x_B - m * dist_CD, y_B + m * dist_CD, z_B - m * dist_CD]
      r4 = vector [x_B - m * dist_CD, y_B - m * dist_CD, z_B + m * dist_CD]
      ws = sortCoordinates [r1, r2, r3, r4]
      wsMol = Map.elems $ Map.filter (`isInWorkSpace` ws) $ Map.delete atomID_C originMol
      
      -- Ищем углы трансляции. Система координат левая. Упорядоченная тройка (x,y,z).
      -- Поэтому положительным углам соответствует вращение по часовой стрелке.
      -- b1 - угол поворота вокруг оси Z
      -- b2 - угол поворота вокруг оси Y
      b1
        | xy == 0 = Degree 0
        | y'_C > 0 = acosd (x'_C / xy)
        | otherwise = -acosd (x'_C / xy)
        where
          [x'_C, y'_C] = [x_C - x_B, y_C - y_B]
          xy = sqrt $ x'_C ^ 2 + y'_C ^ 2
      b2
        | z'_C < 0 = acosd (xy / xyz)
        | otherwise = -acosd (xy / xyz)
        where
          [x'_C, y'_C, z'_C] = [x_C - x_B, y_C - y_B, z_C - z_B]
          xy = sqrt $ x'_C ^ 2 + y'_C ^ 2
          xyz = dist_BC
      
      -- Найдем координаты @atom_A@ в дважды штрихованной
      -- системе координат
      x'_A = x_A - x_B
      y'_A = y_A - y_B
      z'_A = z_A - z_B
      y''_A = -x'_A * sind b1 + y'_A * cosd b1 + 0
      z''_A = x'_A * cosd b1 * sind b2 + y'_A * sind b1 * sind b2 + z'_A * cosd b2
        -- Определеим угол между осью Z'' и направлением
        -- связи AB (угол отсчитывается в направлении по часовой стрелке)
      b3
        | yz == 0 = Degree 0
        | y''_A < 0 = acosd (z''_A / yz)
        | otherwise = -acosd (z''_A / yz)
        where
          yz = sqrt $ y''_A ^ 2 + z''_A ^ 2
        -- Вставляем @atom_C@ в @molecule@
        -- Функция, задающая координаты атома @atom_C@  в единой СК
        -- в дважды штрихованной СК.
      possibleCoord =
        let h = dist_BC * sind angl_BCD
            x''_D = -dist_CD * cosd angl_BCD + dist_BC
            y''_D = -h * sind (b3 + angl_ABCD)
            z''_D = h * cosd (b3 + angl_ABCD)
            x =
              x''_D * cosd b1 * cosd b2 - y''_D * sind b1 +
              z''_D * cosd b1 * sind b2 +
              x_B
            y =
              x''_D * sind b1 * cosd b2 + y''_D * cosd b1 +
              z''_D * sind b1 * sind b2 +
              y_B
            z = -x''_D * sind b2 + 0 + z''_D * cosd b2 + z_B
            v = vector [x,y,z]
         in set acoordin v atom_D
  let allVariance = pure possibleCoord
  goodVariance <-
    filter (\x -> not . or $ isIntersection <$> wsMol <*> pure x) allVariance
  return $ Map.insert atomID_D goodVariance insMol

-- The function is intended to insert the third and all
-- subsequent atoms of the molecule from @ZMolecule@ with
-- the optimization process. Turns ONE possible variant of the
-- molecule with the insert.
setAtomWithOptimization3 ::
     (FilePath, FilePath, FilePath, FilePath, FilePath)
  -> Int
  -> ZMolecule
  -> Molecule
  -> IO Molecule
setAtomWithOptimization3 (mol_of, res_of, opt_path, opt_script, mol_if) n zMol molecule = do
  let matrixAtom_D = zMol !! (n-1)
      atomID_D = view atomid matrixAtom_D
      atomID_C = view con matrixAtom_D
      atomID_B = view valcon matrixAtom_D
      atomID_A = view dihcon matrixAtom_D
      dist_CD = view dist matrixAtom_D
      angl_BCD = toDegree $ view valangl matrixAtom_D
      angl_ABCD = toDegree $ view dihangl matrixAtom_D
      atom_D = view atom matrixAtom_D
      atom_C =
        fromMaybe (error "setAtomWithOptimization3: atom_C not found") $
        molecule Map.!? atomID_C
      atom_B =
        fromMaybe (error "setAtomWithOptimization3: atom_B not found") $
        molecule Map.!? atomID_B
      atom_A =
        fromMaybe (error "setAtomWithOptimization3: atom_A not found") $
        molecule Map.!? atomID_A
      [x_A, y_A, z_A] = toList $ view acoordin atom_A
      [x_B, y_B, z_B] = toList $ view acoordin atom_B
      [x_C, y_C, z_C] = toList $ view acoordin atom_C
      dist_BC = sqrt $ (x_C - x_B) ^ 2 + (y_C - y_B) ^ 2 + (z_C - z_B) ^ 2
        -- Ищем углы трансляции. Система координат левая. Упорядоченная тройка (x,y,z).
        -- Поэтому положительным углам соответствует вращение по часовой стрелке.
        -- b1 - угол поворота вокруг оси Z
        -- b2 - угол поворота вокруг оси Y
      b1
        | xy == 0 = Degree 0
        | y'_C > 0 = acosd (x'_C / xy)
        | otherwise = -acosd (x'_C / xy)
        where
          xy = sqrt $ x'_C ^ 2 + y'_C ^ 2
          [x'_C, y'_C] = [x_C - x_B, y_C - y_B]
      b2
        | z'_C < 0 = acosd (xy / xyz)
        | otherwise = -acosd (xy / xyz)
        where
          xy = sqrt $ x'_C ^ 2 + y'_C ^ 2
          xyz = dist_BC
          [x'_C, y'_C, z'_C] = [x_C - x_B, y_C - y_B, z_C - z_B]
        -- Найдем координаты @atom_A@ в дважды штрихованной
        -- системе координат
      x'_A = x_A - x_B
      y'_A = y_A - y_B
      z'_A = z_A - z_B
      y''_A = -x'_A * sind b1 + y'_A * cosd b1 + 0
      z''_A =
        x'_A * cosd b1 * sind b2 + y'_A * sind b1 * sind b2 + z'_A * cosd b2
        -- Определеим угол между осью Z и направлением
        -- связи AB (угол отсчитывается в направлении по часовой стрелке)
      b3
        | yz == 0 = Degree 0
        | y''_A < 0 = acosd (z''_A / yz)
        | otherwise = -acosd (z''_A / yz)
        where
          yz = sqrt $ y''_A ^ 2 + z''_A ^ 2
        -- Вставляем @atom_C@ в @molecule@
        -- Функция, задающая координаты атома @atom_C@  в единой СК
        -- по углу alpha в дважды штрихованной СК.
  let possibleAtom =
        let h = dist_BC * sind angl_BCD
            x''_D = -dist_CD * cosd angl_BCD + dist_BC
            y''_D = -h * sind (b3 + angl_ABCD)
            z''_D = h * cosd (b3 + angl_ABCD)
            x =
              x''_D * cosd b1 * cosd b2 - y''_D * sind b1 +
              z''_D * cosd b1 * sind b2 +
              x_B
            y =
              x''_D * sind b1 * cosd b2 + y''_D * cosd b1 +
              z''_D * sind b1 * sind b2 +
              y_B
            z = -x''_D * sind b2 + 0 + z''_D * cosd b2 + z_B
            v = vector [x,y,z]
         in set acoordin v atom_D
  let molecule' = Map.insert atomID_D possibleAtom molecule
      [x_D, y_D, z_D] = toList $ view acoordin possibleAtom
      r_D = vdwr possibleAtom
      m = 1
      r1 = vector [x_D - m * r_D, y_D - m * r_D, z_D - m * r_D]
      r2 = vector [x_D + m * r_D, y_D - m * r_D, z_D - m * r_D]
      r3 = vector [x_D - m * r_D, y_D + m * r_D, z_D - m * r_D]
      r4 = vector [x_D - m * r_D, y_D - m * r_D, z_D + m * r_D]
      ws = sortCoordinates [r1, r2, r3, r4]
      wsMol = Map.elems $ Map.filter (`isInWorkSpace` ws) molecule'
      atomsForOptim = filter (isIntersection possibleAtom) wsMol
      resSeqForOptim =
        List.delete (view aresseq possibleAtom) . List.nub $
        map (view aresseq) atomsForOptim
  if null resSeqForOptim
    then return molecule'
    else do
      cur_dir <- getCurrentDirectory
      setCurrentDirectory (cur_dir ++ "/" ++ opt_path)
      writeMoleculePdb mol_of molecule'
      appendFile res_of (unlines $ show <$> resSeqForOptim)
      callCommand opt_script
      putStrLn "optimization_script: OK"
      molecule'' <- readMoleculePdb mol_if
      setCurrentDirectory cur_dir
      return molecule''

-- UTILS
-- rotateM :: Vector Double -> Double -> Matrix Double
-- rotateM axis theta = 
--   let v = axis / (realToFrac . sqrt) (axis `dot` axis)
--       a = cos (theta / 2)
--       [b, c, d] = toList $ -v * (realToFrac . sin) (theta / 2)
--       [aa,bb,cc,dd] = [a,b,c,d] ^ 2
--       [bc,ad,ac,ab,bd,cd] = [b*c,a*d,a*c,a*b,b*d,c*d]
--   in (3><3) 
--       [aa + bb - cc - dd, 2 * (bc + ad), 2 * (bd - ac),
--        2 * (bc - ad), aa + cc - bb - dd, 2 * (cd + ab),
--        2 * (bd + ac), 2 * (cd - ab), aa + dd - bb - cc]

-- The function sorts the coordinates so that the vectors 
-- @r0r1@, @r0r2@, @r0r3@ form the right triple.
sortCoordinates :: [Point] -> [Point]
sortCoordinates points =
  let compX v1 v2 = compare (v1 ! 0) (v2 ! 0)
      compY v1 v2 = compare (v1 ! 1) (v2 ! 1)
      compZ v1 v2 = compare (v1 ! 2) (v2 ! 2)
  in List.sortBy compZ . List.sortBy compY . List.sortBy compX $ points

-- The function determines whether the atom is in the selected 
-- @[point]@ volume.
isInWorkSpace :: Atom -> [Point] -> Bool
isInWorkSpace atom points =
  let r0:r1:r2:r3:_ = points
      [x0, y0, z0] = toList r0
      [x1, _, _]   = toList r1
      [_, y2, _]   = toList r2
      [_, _, z3]   = toList r3
      [x, y, z]    = toList (view acoordin atom)
      between x a b = a <= x && x <= b
   in between x x0 x1 && between y y0 y2 && between z z0 z3

-- The function takes two atoms and determines whether 
-- the Van-der-Waltz radii intersect. In the case of intersection returns True.
isIntersection :: Atom -> Atom -> Bool
isIntersection atomA atomB =
  let rA = vdwr atomA
      rB = vdwr atomB
      [xA, yA, zA] = toList $ view acoordin atomA
      [xB, yB, zB] = toList $ view acoordin atomB
      dist = sqrt $ (xB - xA) ^ 2 + (yB - yA) ^ 2 + (zB - zA) ^ 2
  in dist <= (rA + rB)
  
-- | Get VDW radius of the atom.
vdwr :: Atom -> Double
vdwr atom =
  let e = take 1 $ view aname atom
  in case e of
      "H" -> 0.200 -- Заменено с 0.200. Радиус в PDB 1.000
      "O" -> 1.200 -- Заменено с 1.510. Радиус в PDB 1.300
      "N" -> 1.200 -- Заменено с 1.648. Радиус в PDB 1.400
      "C" -> 1.200 -- Заменено с 1.782. Радиус в PDB 1.500
      "S" -> 1.900 -- Заменено с 1.782. Радиус в PDB 1.900
      otherwise -> error ("vdwr: atom " ++ show e ++ "not found") 