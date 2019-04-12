module Config where

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

-- | Файл для чтения белка после процесса оптимизации
mol_inf :: String
mol_inf = get "mol_inf" id

-- | Файл для вывода белка перед процессом оптимизации
mol_ouf :: String
mol_ouf = get "mol_ouf" id

resid_ouf :: String
resid_ouf = get "resid_ouf" id