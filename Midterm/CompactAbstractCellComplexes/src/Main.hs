-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   : Francesc Ant�n Castro / Fran�ois Anton
-- License     :  AllRightsReserved
--
-- Maintainer  : Francesc Ant�n Castro / Fran�ois Anton
-- Stability   : Uses only exact data types
-- Portability : Portable to any Unix - Windows requires change of file names
--
-- |
--
-----------------------------------------------------------------------------

module Main where
import Data.List (foldr,transpose,map,notElem)
import qualified Data.ByteString.Lazy.Char8 as BStr
import qualified Data.Map as Map
import Control.Monad
import CACC

type DataMap =  Map.Map String [BStr.ByteString]
-- | Main function of the verification software for Compact Abstract Cell Complexes
-- | File loading - please change file names accordingly. I use fixed filenames, so there is no impact on the profiling
-- | processFile is the function that converts an input file where the first filed is a search key into a map/hash table with that search key as key and the other fields as a list value
-- | processFileMK is the function that converts an input file where there are no search keys into a map/hash table where the first field is taken as key
-- | processFileL is the function that converts am input file that stores a list into a list
-- | alpha0s1s is the combined map of the alpha0s and alpha1s
-- | b is a boolean flag that reports if the combined alpha0s alpha1s chain and cycle
-- | b0s1s2s is the boundary of the combined map of the alpha0s alpha1s and alpha2s and therefore contains the orbits of edges around each vertex in a given volume: for a cube each orbit has exactly 3 edges
-- | alpha0s1s2s is the combined map of the alpha0s, alpha1s and alpha2s
-- | c is the validity check boolean of the combined map of the alpha0s, alpha1s and alpha2s
-- | d is a boolean that certifies whether the alpha3s are comptaible with the alpha1s
-- | e is a boolean that certifies (proves) that the alpha1s are the cycles within the alpha0s
-- | f is the list of elements of the alpha0s
-- | g is the list of pairs of consecutive elements in the alpha0s
-- | h = groupByCycles g computes the cycles in the pairs of consecutive vertices in the alpha0s
-- | i is the combined alpha0s alpha1s
main = do
  alpha0s <- processFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Alpha0.txt"
  alpha1s <- processFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Alpha1.txt"
  alpha2s <- processFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Alpha2.txt"
  alpha3s <- processFileMK "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Alpha3.txt"
  hindices <- processFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Volumes.txt"
  externalshells <- processFileL "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/ExternalShells.txt"
  let alpha0s1s = constructa0a1 alpha0s alpha1s
  let b = checka0sa1s alpha0s1s
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Combined alpha0s alpha1s " ++ show alpha0s1s ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size of Combined alpha0s alpha1s = " ++ show (Map.size alpha0s1s) ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Test alpha1s with alpha0s " ++ show b ++ "\n")
  let b0s1s2s = constructBoundariesa0a1a2 alpha0s1s alpha2s
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Combined alpha0s alpha1s alpha2s bondaries " ++ show b0s1s2s ++ "\n")
  let alpha0s1s2s = constructa0a1a2 alpha0s1s alpha2s
  let c = checka0sa1sa2s alpha0s1s2s
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Combined alpha0s alpha1s alpha2s " ++ show alpha0s1s2s ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size of Combined alpha0s alpha1s alpha2s = " ++ show (Map.size alpha0s1s2s) ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Test alpha2s with Combined alpha0s alpha1s " ++ show c ++ "\n")
  let d = checkalpha3s alpha0s1s alpha3s
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Test alpha3s with Combined alpha0s alpha1s alpha2s" ++ show d ++ "\n")
  let e = certifyalpha1s alpha0s alpha0s1s
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Certify alpha1s " ++ show e ++ "\n")
  let f = Map.elems alpha0s
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Lists from alpha0s " ++ show f ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size of lists from alpha0s = " ++ show (length f) ++ "\n")
  let g = map pairify f
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Lists converted to pairs from alpha0s " ++ show g ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size of lists converted to pairs obtained from alpha0s = " ++ show (length g) ++ "\n")
  let h = groupByCycles g
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Cycles obtained from alpha0s " ++ show h ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size of cycles map obtained from alpha0s = " ++ show (length h) ++ "\n")
  let i = Map.elems alpha0s1s
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Cycles obtained from alpha0s1s " ++ show i ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size of cycles obtained from alpha0s1s = " ++ show (length i) ++ "\n")
  let j = certifyExternalShells alpha0s1s externalshells
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Certify external shells " ++ show j ++ "\n")
  let k = map (\x -> -x) externalshells
  let kb = map (faceBoundary alpha0s1s) externalshells
  let l = constructPairs k
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("External shells map " ++ show k ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("External shells boundaries " ++ show kb ++ "\n")
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("External shells pairs " ++ show l ++ "\n")
  let m = map (getfacecycle alpha0s1s) l
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Face cycles " ++ show m ++ "\n")
  let n = map (isExternalFaceConnectivity alpha0s1s) l
  appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Certify external shells " ++ show m ++ "\n")
  return 0
