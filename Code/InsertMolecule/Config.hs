{-|
Module      : Config
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental

This module describes the configuration file of the program.
-}

module InsertMolecule.Config
  ( 
  -- * Main settings
    zmolecule_fn
  , molecule_fn
  , molecule_result_fn
  , n_without_opt
  , n_with_opt

  -- * Optimizer
  , opt_path
  , opt_script
  , opt_mol_bef
  , opt_mol_aft
  , opt_resid
  ) where

import Data.List
import System.Environment
import System.IO.Unsafe

type Config = [(String, String)]

readConfig :: Config
readConfig =
  let s = unsafePerformIO t
      t = do
        args <- getArgs
        let config_fn = head args
        readFile config_fn
   in [ (key, val)
      | line <- filter (\x -> not ("#" `isPrefixOf` x) && x /= "") . lines $ s
      , let (key, val) = (\x -> (head x, unwords . drop 2 $ x)) . words $ line
      ]

get :: String -> (String -> a) -> a
get key f =
  case lookup key readConfig of
    Nothing -> error ("get: not found: " ++ key)
    Just x -> f x

-- | A file describing the molecule to be inserted in the @ z-matrix @ representation.
zmolecule_fn :: String
zmolecule_fn = get "zmolecule_fn" id

-- | A file describing the molecule to be inserted.
molecule_fn :: String
molecule_fn = get "molecule_fn" id

-- | The file where the molecule resulting from the programs will be written to.
molecule_result_fn :: String
molecule_result_fn = get "molecule_result_fn" id

-- | The number of atoms that is inserted without the optimization process.
n_without_opt :: Int
n_without_opt = get "n_without_opt" read

-- | The range of atoms that are inserted with the optimization process.
n_with_opt :: (Int, Int)
n_with_opt = get "n_with_opt" read

-- | The folder with optimizer and auxiliary files lie.
opt_path :: String
opt_path = get "opt_path" id

-- | Optimizer.
opt_script :: String
opt_script = get "opt_script" id

-- | Molecule file before optimizatin.
opt_mol_bef :: String
opt_mol_bef = get "opt_mol_bef" id

-- | Optimized molecule file.
opt_mol_aft :: String
opt_mol_aft = get "opt_mol_aft" id

-- | The file where @resid@ will be specified, 
-- which will be used by the optimizer in its work.
opt_resid :: String
opt_resid = get "opt_resid" id
