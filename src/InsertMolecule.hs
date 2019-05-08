{-|
Module      : InsertMolecule
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental

The module describes the work with functions that perform the insertion.
-}
{-# LANGUAGE BangPatterns #-}

module InsertMolecule
  ( setAtomWithOutOptimization
  , setAtomWithOptimization
  ) where

import Control.Category
import Control.Concurrent
import Control.DeepSeq
import Data.Label
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import Prelude hiding ((.), id)
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

import Parser
import TrigonometryDegree
import Types

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
setAtomWithOutOptimization 0 _ mol = pure mol
setAtomWithOutOptimization n zmatr mol =
  listToMaybe $ setAtomWithOutOptimization' 1 n zmatr mol newMolecule
  where
    setAtomWithOutOptimization' m n zmatr originMol insMol
      | n < 1 = error "setAtomWithOutOptimization: @n@ must be great than 0"
      | m > n = pure $ Map.union originMol insMol
      | m == 1 =
        concat $!
        setAtomWithOutOptimization' (m + 1) n zmatr originMol <$>
        setAtomWithOutOptimization1 1 zmatr mol insMol
      | m == 2 =
        concat $!
        setAtomWithOutOptimization' (m + 1) n zmatr originMol <$>
        setAtomWithOutOptimization2 2 zmatr mol insMol
      | otherwise =
        concat $!
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
  | s < 0 || e < 0 = error "setAtomWithOptimization: @s@ and @e@ must be great than 0"
  | s > e = return mol
  | otherwise = do
    mol' <- setAtomWithOptimization3 fns s zmatr mol
    mol' `seq` setAtomWithOptimization fns ((s + 1), e) zmatr mol'

-- The function is intended to insert the first atom of the 
-- molecule from @ZMolecule@ without the optimization process.
-- Returns all possible variants of the molecules with an insert.
-- If there are none, then an empty list is returned.
setAtomWithOutOptimization1 :: Int -> ZMolecule -> Molecule -> Molecule -> [Molecule]
setAtomWithOutOptimization1 n zMol originMol insMol = do
  let matrixAtom_B = zMol !! n
      atomID_B = fromJust $ get atomid matrixAtom_B
      atomID_A = fromJust $ get atomcon matrixAtom_B
      distance_AB = fromJust $ get bonddist matrixAtom_B
      atom_B = get atom matrixAtom_B
      atom_A =
        fromMaybe (error "setAtomWithOutOptimization1: atom_A not found") $
        originMol Map.!? atomID_A
      (x_A, y_A, z_A) = get coordin atom_A
        -- Отбираем только те молекулы, который находятся в окрестности @atom_A@.
        -- Для дальнейшего рассмотрения удаляем из рассмотрения тот атом,
        -- с которым связан прикрепляемый атом (то есть удаляем @atom_A@)
      m = 2
      r1 = (,,) (x_A - m * distance_AB) (y_A - m * distance_AB) (z_A - m * distance_AB)
      r2 = (,,) (x_A + m * distance_AB) (y_A - m * distance_AB) (z_A - m * distance_AB)
      r3 = (,,) (x_A - m * distance_AB) (y_A + m * distance_AB) (z_A - m * distance_AB)
      r4 = (,,) (x_A - m * distance_AB) (y_A - m * distance_AB) (z_A + m * distance_AB)
      ws = sortCoordinates [r1, r2, r3, r4]
      wsMolecule =
        Map.elems $ Map.filter (`isInWorkSpace` ws) $ Map.delete atomID_A originMol
        -- Вставляем @atom_B@ в @molecule@
        -- Функция, задающая координаты атома @atom_B@ в единой СК
        -- по углам alpha и beta в штрихованной СК.
      possibleCoord a b =
        let x_B = distance_AB * cosd a * sind b + x_A
            y_B = distance_AB * sind a * sind b + y_A
            z_B = distance_AB * cosd b + z_A
         in (x_B, y_B, z_B) `deepseq` set coordin (x_B, y_B, z_B) atom_B
  let alpha = [Degree 0,Degree 5 .. Degree 355]
      beta = [Degree 0,Degree 5 .. Degree 175]
      allVariance = possibleCoord <$> alpha <*> beta
  goodVariance <-
    filter (\x -> not . or $ isIntersection <$> wsMolecule <*> pure x) allVariance
  return $ Map.insert atomID_B goodVariance insMol

-- The function is intended to insert the second atom of the 
-- molecule from @ZMolecule@ without the optimization process.
-- Returns all possible options for molecules with an insert.
-- If there are none, then an empty list is returned.
setAtomWithOutOptimization2 :: Int -> ZMolecule -> Molecule -> Molecule -> [Molecule]
setAtomWithOutOptimization2 n zMol originMol insMol = do
  let matrixAtom_C = zMol !! n
      atomID_C = fromJust $ get atomid matrixAtom_C
      atomID_B = fromJust $ get atomcon matrixAtom_C
      atomID_A = fromJust $ get anglcon matrixAtom_C
      distance_BC = fromJust $ get bonddist matrixAtom_C
      angle_ABC = toDegree $ fromJust $ get bondangl matrixAtom_C
      atom_C = get atom matrixAtom_C
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
      (x_A, y_A, z_A) = get coordin atom_A
      (x_B, y_B, z_B) = get coordin atom_B
      distance_AB = sqrt $ (x_B - x_A) ^ 2 + (y_B - y_A) ^ 2 + (z_B - z_A) ^ 2
        -- Отбираем только те молекулы, которые находятся в окрестности @atom_B@.
        -- Для дальнейшего рассмотрения удаляем из рассмотрения тот атом,
        -- с которым связан прикрепляемый атом (то есть удаляем @atom_B@)
      m = 2
      r1 = (,,) (x_B - m * distance_BC) (y_B - m * distance_BC) (z_B - m * distance_BC)
      r2 = (,,) (x_B + m * distance_BC) (y_B - m * distance_BC) (z_B - m * distance_BC)
      r3 = (,,) (x_B - m * distance_BC) (y_B + m * distance_BC) (z_B - m * distance_BC)
      r4 = (,,) (x_B - m * distance_BC) (y_B - m * distance_BC) (z_B + m * distance_BC)
      ws = sortCoordinates [r1, r2, r3, r4]
      wsMolecule =
        Map.elems $ Map.filter (`isInWorkSpace` ws) $ Map.delete atomID_B originMol
        -- Ищем углы трансляции. Система координат левая.
        -- Поэтому положительным углам соответствует вращение по часовой стрелке.
        -- b1 - угол поворота вокруг оси Z
        -- b2 - угол поворота вокруг оси Y
      b1
        | xy == 0 = Degree 0
        | y'_B > 0 = acosd (x'_B / xy)
        | otherwise = -acosd (x'_B / xy)
        where
          xy = sqrt $ x'_B ^ 2 + y'_B ^ 2
          (x'_B, y'_B) = (x_B - x_A, y_B - y_A)
      b2
        | z'_B < 0 = acosd (xy / xyz)
        | otherwise = -acosd (xy / xyz)
        where
          xy = sqrt $ x'_B ^ 2 + y'_B ^ 2
          xyz = distance_AB
          (x'_B, y'_B, z'_B) = (x_B - x_A, y_B - y_A, z_B - z_A)
        -- Вставляем @atom_C@ в @molecule@
        -- Функция, задающая координаты атома @atom_C@  в единой СК
        -- по углу alpha в дважды штрихованной СК.
      possibleCoord a =
        let h = distance_BC * sind angle_ABC
            x''_C = -distance_BC * cosd angle_ABC + distance_AB
            z''_C = h * sind a
            y''_C = h * cosd a
            x_C =
              x''_C * cosd b1 * cosd b2 - y''_C * sind b1 + z''_C * cosd b1 * sind b2 +
              x_A
            y_C =
              x''_C * sind b1 * cosd b2 + y''_C * cosd b1 +
              z''_C * sind b1 * sind b2 +
              y_A
            z_C = -x''_C * sind b2 + 0 + z''_C * cosd b2 + z_A
         in (x_C, y_C, z_C) `seq` set coordin (x_C, y_C, z_C) atom_C
  let alpha = [Degree 0,Degree 1 .. Degree 359]
      allVariance = possibleCoord <$> alpha
  goodVariance <-
    filter (\x -> not . or $ isIntersection <$> wsMolecule <*> pure x) allVariance
  return $ Map.insert atomID_C goodVariance insMol

-- The function is intended to insert the third and all subsequent
-- atoms of the molecule from @ZMolecule@ without the optimization
-- process. Turns ONE possible variant of the molecule with the
-- insert. If there is none, then an empty list is returned.
setAtomWithOutOptimization3 :: Int -> ZMolecule -> Molecule -> Molecule -> [Molecule]
setAtomWithOutOptimization3 n zMol originMol insMol = do
  let matrixAtom_D = zMol !! n
      atomID_D = fromJust $ get atomid matrixAtom_D
      atomID_C = fromJust $ get atomcon matrixAtom_D
      atomID_B = fromJust $ get anglcon matrixAtom_D
      atomID_A = fromJust $ get dihedcon matrixAtom_D
      distance_CD = fromJust $ get bonddist matrixAtom_D
      angle_BCD = toDegree $ fromJust $ get bondangl matrixAtom_D
      angle_ABCD = toDegree $ fromJust $ get dihedangl matrixAtom_D
      atom_D = get atom matrixAtom_D
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
      (x_A, y_A, z_A) = get coordin atom_A
      (x_B, y_B, z_B) = get coordin atom_B
      (x_C, y_C, z_C) = get coordin atom_C
      distance_BC = sqrt $ (x_C - x_B) ^ 2 + (y_C - y_B) ^ 2 + (z_C - z_B) ^ 2
        -- Отбираем только те молекулы, который находятся в окрестности @atom_B@.
        -- Для дальнейшего рассмотрения удаляем из рассмотрения тот атом,
        -- с которым связан прикрепляемый атом (то есть удаляем @atom_B@)
        -- !!! На самом деле здесь следует удалять из рассмотреия все атомы, с которыми связан вставлямый атом.
      m = 2
      r1 = (,,) (x_B - m * distance_CD) (y_B - m * distance_CD) (z_B - m * distance_CD)
      r2 = (,,) (x_B + m * distance_CD) (y_B - m * distance_CD) (z_B - m * distance_CD)
      r3 = (,,) (x_B - m * distance_CD) (y_B + m * distance_CD) (z_B - m * distance_CD)
      r4 = (,,) (x_B - m * distance_CD) (y_B - m * distance_CD) (z_B + m * distance_CD)
      ws = sortCoordinates [r1, r2, r3, r4]
      wsMolecule =
        Map.elems $ Map.filter (`isInWorkSpace` ws) $ Map.delete atomID_C originMol
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
          (x'_C, y'_C) = (x_C - x_B, y_C - y_B)
      b2
        | z'_C < 0 = acosd (xy / xyz)
        | otherwise = -acosd (xy / xyz)
        where
          xy = sqrt $ x'_C ^ 2 + y'_C ^ 2
          xyz = distance_BC
          (x'_C, y'_C, z'_C) = (x_C - x_B, y_C - y_B, z_C - z_B)
        -- Найдем координаты @atom_A@ в дважды штрихованной
        -- системе координат
      x'_A = x_A - x_B
      y'_A = y_A - y_B
      z'_A = z_A - z_B
      y''_A = -x'_A * sind b1 + y'_A * cosd b1 + 0
      z''_A =
        x'_A * cosd b1 * sind b2 + y'_A * sind b1 * sind b2 + z'_A * cosd b2
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
        let h = distance_BC * sind angle_BCD
            x''_D = -distance_CD * cosd angle_BCD + distance_BC
            y''_D = -h * sind (b3 + angle_ABCD)
            z''_D = h * cosd (b3 + angle_ABCD)
            x_D =
              x''_D * cosd b1 * cosd b2 - y''_D * sind b1 +
              z''_D * cosd b1 * sind b2 +
              x_B
            y_D =
              x''_D * sind b1 * cosd b2 + y''_D * cosd b1 +
              z''_D * sind b1 * sind b2 +
              y_B
            z_D = -x''_D * sind b2 + 0 + z''_D * cosd b2 + z_B
         in (x_D, y_D, z_D) `seq` set coordin (x_D, y_D, z_D) atom_D
  let allVariance = pure possibleCoord
  goodVariance <-
    filter (\x -> not . or $ isIntersection <$> wsMolecule <*> pure x) allVariance
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
  let matrixAtom_D = zMol !! n
      atomID_D = fromJust $ get atomid matrixAtom_D
      atomID_C = fromJust $ get atomcon matrixAtom_D
      atomID_B = fromJust $ get anglcon matrixAtom_D
      atomID_A = fromJust $ get dihedcon matrixAtom_D
      distance_CD = fromJust $ get bonddist matrixAtom_D
      angle_BCD = toDegree $ fromJust $ get bondangl matrixAtom_D
      angle_ABCD = toDegree $ fromJust $ get dihedangl matrixAtom_D
      atom_D = get atom matrixAtom_D
      atom_C =
        fromMaybe (error "setAtomWithOptimization3: atom_C not found") $
        molecule Map.!? atomID_C
      atom_B =
        fromMaybe (error "setAtomWithOptimization3: atom_B not found") $
        molecule Map.!? atomID_B
      atom_A =
        fromMaybe (error "setAtomWithOptimization3: atom_A not found") $
        molecule Map.!? atomID_A
      (x_A, y_A, z_A) = get coordin atom_A
      (x_B, y_B, z_B) = get coordin atom_B
      (x_C, y_C, z_C) = get coordin atom_C
      distance_BC = sqrt $ (x_C - x_B) ^ 2 + (y_C - y_B) ^ 2 + (z_C - z_B) ^ 2
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
          (x'_C, y'_C) = (x_C - x_B, y_C - y_B)
      b2
        | z'_C < 0 = acosd (xy / xyz)
        | otherwise = -acosd (xy / xyz)
        where
          xy = sqrt $ x'_C ^ 2 + y'_C ^ 2
          xyz = distance_BC
          (x'_C, y'_C, z'_C) = (x_C - x_B, y_C - y_B, z_C - z_B)
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
      possibleAtom =
        let h = distance_BC * sind angle_BCD
            x''_D = -distance_CD * cosd angle_BCD + distance_BC
            y''_D = -h * sind (b3 + angle_ABCD)
            z''_D = h * cosd (b3 + angle_ABCD)
            x_D =
              x''_D * cosd b1 * cosd b2 - y''_D * sind b1 +
              z''_D * cosd b1 * sind b2 +
              x_B
            y_D =
              x''_D * sind b1 * cosd b2 + y''_D * cosd b1 +
              z''_D * sind b1 * sind b2 +
              y_B
            z_D = -x''_D * sind b2 + 0 + z''_D * cosd b2 + z_B
         in set coordin (x_D, y_D, z_D) atom_D
  let molecule' = Map.insert atomID_D possibleAtom molecule
      (x_D, y_D, z_D) = get coordin possibleAtom
      r_D = get radius possibleAtom
      m = 1
      r1 = (,,) (x_D - m * r_D) (y_D - m * r_D) (z_D - m * r_D)
      r2 = (,,) (x_D + m * r_D) (y_D - m * r_D) (z_D - m * r_D)
      r3 = (,,) (x_D - m * r_D) (y_D + m * r_D) (z_D - m * r_D)
      r4 = (,,) (x_D - m * r_D) (y_D - m * r_D) (z_D + m * r_D)
      ws = sortCoordinates [r1, r2, r3, r4]
      wsMolecule = Map.elems $ Map.filter (`isInWorkSpace` ws) molecule'
      atomsForOptim = filter (isIntersection possibleAtom) wsMolecule
      resSeqForOptim =
        List.delete (get resseq possibleAtom) . List.nub $
        map (get resseq) atomsForOptim
  if null resSeqForOptim
    then return molecule'
    else do
      cur_dir <- getCurrentDirectory
      setCurrentDirectory (cur_dir <> "/" <> opt_path)
      writeMoleculePDB mol_of molecule'
      appendFile res_of (unlines $ (\(Resseq x) -> show x) <$> resSeqForOptim)
      callCommand opt_script
      putStrLn "optimization_script: OK"
      !molecule'' <- readMoleculePDB mol_if
        -- removeFile mol_of
        -- removeFile res_of
        -- removeFile mol_if
      setCurrentDirectory cur_dir
      molecule'' `seq` return molecule''

-- The function sorts the coordinates so that the vectors 
-- @r0r1@, @r0r2@, @r0r3@ form the right triple.
sortCoordinates :: [Point] -> [Point]
sortCoordinates points =
  let compX (x1, _, _) (x2, _, _) = compare x1 x2
      compY (_, y1, _) (_, y2, _) = compare y1 y2
      compZ (_, _, z1) (_, _, z2) = compare z1 z2
   in List.sortBy compZ . List.sortBy compY . List.sortBy compX $ points

-- The function determines whether the atom is in the selected 
-- @[point]@ volume.
isInWorkSpace :: Atom -> [Point] -> Bool
isInWorkSpace atom points =
  let r0:r1:r2:r3:_ = points
      (x0, y0, z0) = r0
      (x1, _, _) = r1
      (_, y2, _) = r2
      (_, _, z3) = r3
      (x', y', z') = get coordin atom
      between x a b = a <= x && x <= b
   in and [between x' x0 x1, between y' y0 y2, between z' z0 z3]

-- The function takes two atoms and determines whether 
-- the Van-der-Waltz radii intersect. In the case of intersection returns True.
isIntersection :: Atom -> Atom -> Bool
isIntersection atomA atomB =
  let rA = get radius atomA
      (xA, yA, zA) = get coordin atomA
      rB = get radius atomB
      (xB, yB, zB) = get coordin atomB
      dist = sqrt $ (xB - xA) ^ 2 + (yB - yA) ^ 2 + (zB - zA) ^ 2
   in dist <= (rA + rB)
