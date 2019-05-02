module Main where

import Config
import Control.Monad
import Data.Maybe (fromMaybe)
import InsertMolecule
import System.Directory (doesFileExist, removeFile)

main :: IO ()
main = insertMolecule

insertMolecule :: IO ()
insertMolecule = do
  zmolecule <- readZMolecule zmolecule_fn
  molecule <- readMolecule molecule_fn
  let n = n_without_opt
      se = n_with_opt
      fns = (opt_mol_beh, opt_resid, opt_path, opt_script, opt_mol_aft)
  mapM_
    ((\fn -> join $ when <$> doesFileExist fn <*> pure (removeFile fn)) .
     (opt_path <>))
    [opt_mol_beh, opt_resid, opt_mol_aft]
  let molecule' =
        fromMaybe (error "insertMolecule: returned Nothing") $
        setAtomWithOutOptimization n zmolecule molecule
  molecule'' <- setAtomWithOptimization fns se zmolecule molecule'
  writeMolecule molecule_result_fn molecule''
