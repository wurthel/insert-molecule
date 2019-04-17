module Main where

import InsertMolecule
import Config
import Data.Maybe (fromMaybe)

main :: IO ()
main = do insertMolecule

insertMolecule :: IO ()
insertMolecule = do
    zmatrix    <- readZMatrix zmatrix_fn
    molecule   <- readMolecule molecule_fn
    let n  = n_without_opt
        se = n_with_opt
        fns = (tmp_mol_ouf, tmp_resid_ouf, tmp_mol_inf)
    let molecule' = fromMaybe (error "insertMolecule: returned Nothing") 
                    $ setAtomWithOutOptimization n zmatrix molecule
    molecule'' <- setAtomWithOptimization fns se zmatrix molecule'
    writeMolecule molecule_result_fn molecule''