module Main where

import InsertMolecule
import System.Environment
import Data.Maybe (fromMaybe)

main :: IO ()
main = do insertMolecule

insertMolecule :: IO ()
insertMolecule = do
    args <- getArgs
    let zmatr_name = args !! 0
        mol_name   = args !! 1
        n = read $ args !! 2
        s = read $ args !! 3
        e = read $ args !! 4
        res_file = args !! 5
    zmatrix    <- readZMatrix zmatr_name
    molecule   <- readMolecule mol_name
    let molecule' = fromMaybe (error "insertMolecule: returned Nothing") 
                    $ setAtomWithOutOptimization n zmatrix molecule
    molecule'' <- setAtomWithOptimization s e zmatrix molecule'
    writeMolecule res_file molecule''