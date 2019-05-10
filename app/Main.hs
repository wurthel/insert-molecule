module Main where

import Config
import Control.Monad
import Data.Maybe (fromMaybe)
import IO
import InsertMolecule
import System.Directory (doesFileExist, removeFile)

main :: IO ()
main = do
  -- Remove existing files
  mapM_
    ((\fn -> join $ when <$> doesFileExist fn <*> pure (removeFile fn)) . (opt_path <>))
    [opt_mol_bef, opt_resid, opt_mol_aft]
  
  -- Computing
  let n = n_without_opt
      se = n_with_opt
      fns = (opt_mol_bef, opt_resid, opt_path, opt_script, opt_mol_aft)
  zmolecule <- readZMolecule zmolecule_fn
  molecule <- readMoleculePDB molecule_fn
  let molecule' =
        fromMaybe (error "insertMolecule: returned Nothing") $
        setAtomWithOutOptimization n zmolecule molecule
  molecule'' <- setAtomWithOptimization fns se zmolecule molecule'
  writeMoleculePDB molecule_result_fn molecule''
