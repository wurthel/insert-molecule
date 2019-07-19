{-|
Module      : Parser
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental
-}
module InsertMolecule.ReadWrite
  ( 
    -- * Read
    readMoleculePdb
  , readZMolecule

    -- * Write
  , writeMoleculePdb
  ) where

import Control.Lens
import qualified Data.List as List
import qualified Data.Map as Map
import Numeric.LinearAlgebra.Data
import System.Directory (renameFile)
import System.IO
import Text.Printf (hPrintf)
import Text.Read (readMaybe)

import InsertMolecule.Types

-- | Read molecule in PDB format.
readMoleculePdb :: FilePath -> IO Molecule
readMoleculePdb inf = do
  txt <- readFile inf
  let strip = filter (/= ' ')
      rfld f s e = f . drop (s - 1) . take e
  let atoms = 
        [(serial, Atom dtype aname resname resseq coordin)
        | line <- lines txt
        , rfld strip 1 6 line `List.elem` ["ATOM", "HETATM"]
        , let dtype = rfld strip 1 6 line
              serial = rfld read 7 11 line
              aname = rfld strip 13 16 line
              resname = rfld strip 18 20 line
              resseq = rfld read 23 26 line
              x = rfld read 31 38 line
              y = rfld read 39 46 line
              z = rfld read 47 54 line
              coordin = fromList [x, y, z]
        ] 
  return $ foldr addAtom newMolecule atoms

-- | Write molecule in PDB format to file.
writeMoleculePdb :: FilePath -> Molecule -> IO ()
writeMoleculePdb ouf molecule = do
  (tmp_name, tmp_handle) <- openTempFile "." "temp"
  mapM_ (writeAtom tmp_handle) (Map.toList molecule)
  hClose tmp_handle
  renameFile tmp_name ouf
  where
    writeAtom hdl (aid, atom) =
      let record = "ATOM"
          serial = aid
          atype = view aname atom
          altLoc = ' '
          altName = view aresname atom
          chainid = 'A'
          resSeq = view aresseq atom
          icode = ' '
          [x, y, z] = toList $ view acoordin atom
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
  print inf
  txt <- readFile inf
  let zatoms =
        [ ZAtom atom aid con dst vcon vangl dcon dangl
        | line <- lines txt
        , let fields = words line
              aid = read $ fields !! 0
              con = read $ fields !! 1
              dst = read $ fields !! 2
              vcon = read $ fields !! 3
              vangl = read $ fields !! 4
              dcon = read $ fields !! 5
              dangl = read $ fields !! 6
              nm = fields !! 7
              rsnm = fields !! 8
              rssq = read $ fields !! 9
              atom = newAtom { _aname = nm, _aresname = rsnm, _aresseq = rssq}
        ]
  return zatoms