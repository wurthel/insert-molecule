{-|
Module      : Parser
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental
-}
module IO
  ( 
    -- * Read
    readMoleculePDB
  , readZMolecule

    -- * Write
  , writeMoleculePDB
  
    -- * Other
  , vdwr
  ) where

import Control.Category
import Data.Label
import qualified Data.List as List
import qualified Data.Map as Map
import Prelude hiding ((.), id)
import System.Directory (renameFile)
import System.IO
import Text.Printf (hPrintf)
import Text.Read (readMaybe)

import Types

-- | Read molecule in PDB format.
readMoleculePDB :: FilePath -> IO Molecule
readMoleculePDB inf = do
  txt <- readFile inf
  let strip = filter (/= ' ')
      rfld f s e = f . drop (s - 1) . take e
      atoms =
        [ (serial, Atom elem etype resname resseq coordin radius)
        | line <- lines txt
        , rfld strip 1 6 line `List.elem` ["ATOM", "HETATM"]
        , let serial = ID $ rfld read 7 11 line
              etype = Type $ rfld strip 13 16 line
              elem = Element $ rfld (take 1 . strip) 13 16 line
              resname = Resname $ rfld strip 18 20 line
              resseq = Resseq $ rfld read 23 26 line
              coordin =
                (rfld read 31 38 line, rfld read 39 46 line, rfld read 47 54 line)
              radius = vdwr elem
        ] 
  return $ foldr addAtom newMolecule atoms

-- | Write molecule in PDB format to file.
writeMoleculePDB :: FilePath -> Molecule -> IO ()
writeMoleculePDB ouf molecule = do
  (tmp_name, tmp_handle) <- openTempFile "." "temp"
  mapM_ (writeAtom tmp_handle) (Map.toList molecule)
  hClose tmp_handle
  renameFile tmp_name ouf
  where
    writeAtom hdl (id, atom) =
      let record = "ATOM"
          (ID serial) = id
          (Type atype) = get etype atom
          altLoc = ' '
          (Resname altName) = get resname atom
          chainid = 'A'
          (Resseq resSeq) = get resseq atom
          icode = ' '
          (x, y, z) = get coordin atom
          occupan = 0.0 :: Double
          tempfac = 0.0 :: Double
          pattern = "%-6s%5d %4s%1c%3s %1c%4d%1c   %8.3f%8.3f%8.3f%6.2f%6.2f\n"
       in hPrintf
            hdl
            pattern
            record
            serial
            atype
            altLoc
            altName
            chainid
            resSeq
            icode
            x
            y
            z
            occupan
            tempfac

-- | Read molecule in z-matrix representation&
readZMolecule :: FilePath -> IO ZMolecule
readZMolecule inf = do
  txt <- readFile inf
  let zmolecule =
        [ ZAtom atom atomid atomcon bonddist anglcon bondangl dihedcon dihedangl
        | line <- lines txt
        , let fields = words line
              atomid = (readMaybe $ fields !! 0) >>= return . ID
              atomcon = (readMaybe $ fields !! 1) >>= return . ID
              bonddist = readMaybe $ fields !! 2
              anglcon = (readMaybe $ fields !! 3) >>= return . ID
              bondangl = readMaybe $ fields !! 4
              dihedcon = (readMaybe $ fields !! 5) >>= return . ID
              dihedangl = readMaybe $ fields !! 6
              etype = Type $ fields !! 7
              elem = Element $ take 1 $ fields !! 7
              coordin = (0, 0, 0)
              radius = vdwr elem
              resname = Resname $ fields !! 8
              resseq = Resseq $ read $ fields !! 9
              atom = Atom elem etype resname resseq coordin radius
        ]
  return zmolecule

-- | Get VDW radius of the atom.
vdwr :: Element -> Double
vdwr (Element e) =
  case e of
    "H" -> 0.200 -- Заменено с 0.200. Радиус в PDB 1.000
    "O" -> 1.200 -- Заменено с 1.510. Радиус в PDB 1.300
    "N" -> 1.200 -- Заменено с 1.648. Радиус в PDB 1.400
    "C" -> 1.200 -- Заменено с 1.782. Радиус в PDB 1.500
    "S" -> 1.900 -- Заменено с 1.782. Радиус в PDB 1.900