module Util.Utils where


import Control.Monad.State (execState)
import Control.Lens
import Linear.V3
import Data.List
import qualified Data.IntMap as M

import Type.Atom
import Type.Molecule

-- | Shift coordinate system 
shiftCoordSystem :: 
     Point 
  -> Molecule
  -> Molecule
shiftCoordSystem point = execState $ do 
    let x0 = point ^. _x
        y0 = point ^. _y
        z0 = point ^. _z
    atoms . traverse . acoordin . _x -= x0
    atoms . traverse . acoordin . _y -= y0
    atoms . traverse . acoordin . _z -= z0

-- | Check bond 
isBonded :: 
     (Int, Int) 
  -> Molecule
  -> Bool
isBonded (a, b) m = (a', b') `elem` map' (view bonds m) 
  where
    (a', b') = (min a b, max a b)
    map' = map (\x -> let fid = min (view bfid x) (view bsid x)
                          sid = max (view bfid x) (view bsid x)
                      in  (fid, sid))

-- | Get bound
getBond :: 
     (Int, Int) -- First and second bound id (bfid and bsid)
  -> Molecule
  -> Bond
getBond (a, b) m = (getBond' . find') (view bonds m)
  where
    (a', b') = (min a b, max a b)
    find' = find (\x -> let fid = min (view bfid x) (view bsid x)
                            sid = max (view bfid x) (view bsid x)
                        in  fid == a' && sid == b')  
    getBond' x = case x of
      Nothing -> error ("getBond: bond " ++ show (a, b) ++ " not found")
      (Just x) -> x

-- | Get atom by serial
getAtom :: 
     Int
  -> Molecule 
  -> Atom
getAtom i m = 
  case M.lookup i  (m ^. atoms) of
    Just a -> a
    Nothing -> error ("getAtom: atom " ++ show i ++ " not found")