module Config where

import System.Environment
import System.IO.Unsafe
import Data.List.Split
import Data.List

type Config = [(String, String)]

config_file = "config"

readConfig :: Config
readConfig = 
    [(key, val) | 
    line <- filter (\x -> (not $ "--" `isPrefixOf` x) && x /= "" ) $ lines s,
    let (key, val) = (\x -> (head x, unwords $ drop 2 x)) $ words line]
    where s = unsafePerformIO $ readFile config_file 

get :: String -> (String -> a) -> a
get key f = case lookup key readConfig of
    Nothing -> error ("get: not found: " ++ key)
    Just x  -> f x

-- Имя файла структуры со структурой ZMatrix
zmatrix_fn :: String
zmatrix_fn = get "zmatrix" id 

-- Имя файла молекулы в формате pdb
molecule_fn :: String
molecule_fn = get "molecule" id

-- Число атомов, которое мы вставляем без оптимизации
n_without_opt :: Int
n_without_opt = get "n_without_opt" read

-- Диапазон атомов, которое мы вставляем с оптимизацией
n_with_opt :: (Int, Int)
n_with_opt = get "n_with_opt" read 

-- | Файл для чтения белка после процесса оптимизации
tmp_mol_inf :: String
tmp_mol_inf = get "molecule_opt_aft" id

-- | Имя файла оптимизируемой молекулы
tmp_mol_ouf :: String
tmp_mol_ouf = get "molecule_opt_beh" id

-- | Имя файла оптимизируемой молекулы (resid)
tmp_resid_ouf :: String
tmp_resid_ouf = get "resid_opt" id