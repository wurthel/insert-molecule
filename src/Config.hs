module Config where

import System.Environment
import System.IO.Unsafe
import Data.List.Split
import Data.List

type Config = [(String, String)]

readConfig :: Config
readConfig = 
    let s = unsafePerformIO t
        t = do 
                args <- getArgs
                let config_fn = args !! 0
                readFile config_fn
    in  [(key, val) | 
        line <- filter (\x -> (not $ "--" `isPrefixOf` x) && x /= "" ) . lines $ s,
        let (key, val) = (\x -> (head x, unwords . drop 2 $ x)) . words $ line]

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

molecule_result_fn :: String
molecule_result_fn = get "molecule_result" id

-- Число атомов, которое мы вставляем без оптимизации
n_without_opt :: Int
n_without_opt = get "n_without_opt" read

-- Диапазон атомов, которое мы вставляем с оптимизацией
n_with_opt :: (Int, Int)
n_with_opt = get "n_with_opt" read 

-- | Скрипт, запускающий оптимизацию
opt_script :: String
opt_script = get "opt_script" id

-- | PDB файл оптимизируемой молекулы
opt_mol_beh :: String
opt_mol_beh = get "opt_mol_beh" id

-- | PDB файл оптимизированной молекулы
opt_mol_aft :: String
opt_mol_aft = get "opt_mol_aft" id

-- | Имя файла оптимизируемой молекулы (resid)
opt_resid :: String
opt_resid = get "opt_resid" id