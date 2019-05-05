{-|
Module      : Config
Copyright   : (c) Vusal Salmanov, 2019
Maintainer  : salmanov.vh@gmail.com
Stability   : experimental

Данный модуль описывает конфигурационный файл программы.
-}

module Config
  ( 
  -- * Основные параметры
    zmolecule_fn
  , molecule_fn
  , molecule_result_fn
  , n_without_opt
  , n_with_opt

  -- * Оптимизатор
  , opt_path
  , opt_script
  , opt_mol_beh
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
      | line <- filter (\x -> not ("--" `isPrefixOf` x) && x /= "") . lines $ s
      , let (key, val) = (\x -> (head x, unwords . drop 2 $ x)) . words $ line
      ]

get :: String -> (String -> a) -> a
get key f =
  case lookup key readConfig of
    Nothing -> error ("get: not found: " ++ key)
    Just x -> f x

-- | Файла, описывающий вставляемую молекулу в @z-matrix@ представлении.
zmolecule_fn :: String
zmolecule_fn = get "zmolecule_fn" id

-- | Файла, описывающий вставляемую молекулу.
molecule_fn :: String
molecule_fn = get "molecule_fn" id

-- | Файла, куда будет записана полученная в результате работы программ молекула.
molecule_result_fn :: String
molecule_result_fn = get "molecule_result_fn" id

-- | Число атомов, которое вставляется без проведения процесса оптимизации.
n_without_opt :: Int
n_without_opt = get "n_without_opt" read

-- | Диапозон атомов, которые вставляются с проведением процесса оптимизации.
n_with_opt :: (Int, Int)
n_with_opt = get "n_with_opt" read

-- | Папке, где лежит оптимизатор и вспомогательные файлы.
opt_path :: String
opt_path = get "opt_path" id

-- | Скрипт, запускающий оптимизацию.
opt_script :: String
opt_script = get "opt_script" id

-- | Файл оптимизируемой молекулы.
opt_mol_beh :: String
opt_mol_beh = get "opt_mol_beh" id

-- | Файл оптимизированной молекулы.
opt_mol_aft :: String
opt_mol_aft = get "opt_mol_aft" id

-- | Файл, где будут указываться @resid@, которые будет использовать оптимизатор
-- в процессе своей работы.
opt_resid :: String
opt_resid = get "opt_resid" id
