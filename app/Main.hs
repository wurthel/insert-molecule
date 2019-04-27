module Main where

import InsertMolecule
import Config
import Data.Maybe (fromMaybe)

main :: IO ()
main = do insertMolecule

insertMolecule :: IO ()
insertMolecule = do
    zmolecule <- readZMolecule zmolecule_fn
    molecule  <- readMolecule molecule_fn
    let n   = n_without_opt
        se  = n_with_opt
        fns = (opt_mol_beh, opt_resid, opt_path, opt_script, opt_mol_aft)
    let molecule' = fromMaybe (error "insertMolecule: returned Nothing") 
                    $ setAtomWithOutOptimization n zmolecule molecule
    molecule'' <- setAtomWithOptimization fns se zmolecule molecule'
    writeMolecule molecule_result_fn molecule''