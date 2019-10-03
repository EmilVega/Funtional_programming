-----------------------------------------------------------------------------
--
-- Module      :  CACC
-- Copyright   : Francesc Ant�n Castro / Fran�ois Anton
-- License     :  AllRightsReserved
-- Original idea developped during UBC Topics in Algebra (Algebraic Topology) course
-- Maintainer  : Francesc Ant�n Castro / Fran�ois Anton
-- Stability   : Uses only exact data types and functions that do not have side effects (except those 3 functions that have a return data type starting by "IO"
-- Portability : Portable to any Unix - Windows but requires change of file names
--
-- |
-- | The Compact Abstract Cell Complexes
-- | Everything stored is either a cycle that is not a boundary of a higher dimensional cycle or a cycle that is also a boundary
-- | Alpha0s (or a0s) are cycles composed of 0-dimensional elements (points)
-- | Alpha1s (or a1s) are cycles composed of 1-dimensional elements (possibly curved edges)
-- | Alpha2s (or a2s) are cycles composed of 2-dimensional elements (possibly curved polygons)
-- | Alpha3s (or a3s) are cycles composed of 3-dimensional elements (possibly curved polytopes or volumes)
-- | ExternalShells are cycles of volumes bounding the outer space from the union of geometric objects
-- | Only maximal cycles that cannot expand without changing the geometric topology of the geometric objects are stored
-- | This is provably the most compact geometric topology data structure storing the algebraic topology or geometric topology of geometric objects
-- | To be used in Eureka Eurostars E!10631 - The Ultimate Mapping Tablet
-----------------------------------------------------------------------------
module CACC where
import Data.Foldable (foldr')
import qualified Data.ByteString.Lazy.Char8 as BStr
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe

-- | specifies type synonyms for all complex data structures used in CACC
type CycleMap = Map.Map Integer [Integer]
type CyclePairsMap = Map.Map Integer [(Integer,Integer)]
type SingleValueMap = Map.Map Integer Integer
type Decomposition = Map.Map Integer (Set.Set(Integer,Integer))
type Construction = Map.Map Integer (Set.Set Integer)

-- | toData is the function that converts a chunk of data into a pair (key,list)
toData :: Read a => [BStr.ByteString] -> (a,[a])
toData l = (head chunk, tail chunk)
    where chunk = map (read.BStr.unpack) l

-- | toDataMK is the function that converts a chunk of data into a pair (key,single value)
toDataMK :: Read a => [BStr.ByteString] -> (a,a)
toDataMK l = (head$tail chunk, head$tail$tail chunk)
    where chunk = map (read.BStr.unpack) l

-- | toDataL is the function that converts a chunk of data into a single value
toDataL :: Read a => [BStr.ByteString] -> a
toDataL l = head chunk
    where chunk = map (read.BStr.unpack) l

-- | processFileMK is the side effect (IO) function that converts an input file where there are no search keys into an IO SingleValueMap where the first field is taken as key
processFileMK :: FilePath -> IO SingleValueMap
processFileMK name = do
    sheet <- (map (BStr.split ' ') . BStr.lines) `fmap` BStr.readFile name
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("File: " ++ name ++ "\n")
    let intermediaryValue = map toDataMK $ sheet
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size = " ++ show (length intermediaryValue) ++ "\n")
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Map:" ++ show intermediaryValue ++ "\n")
    return (Map.fromList intermediaryValue)

-- | processFile is the side effect (IO) function that converts an input file where the first filed is a search key into an IO CycleMap with that search key as key and the other fields as a list value
processFile :: FilePath -> IO CycleMap
processFile name = do
    sheet <- (map (BStr.split ' ') . BStr.lines) `fmap` BStr.readFile name
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("File: " ++ name ++ "\n")
    let intermediaryValue = map toData sheet
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size = " ++ show (length intermediaryValue) ++ "\n")
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Map:" ++ show intermediaryValue ++ "\n")
    return (Map.fromList intermediaryValue)

-- |  processFileL is the side effect (IO) function that converts am input file that stores a list into an IO list of Integers
processFileL :: FilePath -> IO [Integer]
processFileL name = do
    sheet <- (map (BStr.split ' ') . BStr.lines) `fmap` BStr.readFile name
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("File: " ++ name ++ "\n")
    let intermediaryValue = map toDataL sheet
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Size = " ++ show (length intermediaryValue) ++ "\n")
    appendFile "/home/emil/Escritorio/Enlace hacia Séptimo semestre/Programación Funcional/Midterm/CompactAbstractCellComplexes/Testfile.txt" ("Map:" ++ show intermediaryValue ++ "\n")
    return intermediaryValue

-- | chain is the higher order function that takes two pairs of Integers (representing edges) and checks whether they chain or not. In fact as in Haskell all functions take one parameter and return one value, chain is the higher order function that takes as input one edge represented by its pair of Integer IDs and returns a function that takes as input one edge represented by its pair of Integer IDs and returns a Boolean expressing whether they chain or not.
chain :: (Integer,Integer) -> (Integer,Integer) -> Bool
chain (x1,x2) (y1,y2) = x2 == y1

-- | notreachedFirstasEndVertex is the function that takes two pairs of Integers (representing edges) and checks whether the second one end vertex is the starting vertex of the first one. This is first true in a cycle when the last edge of the cycle is reached. Thus, in order to test whether such an edge is a end of cycle edge, we first need to check if its previous edge chains with it and if it ends where the first edge of the cycle started. This is the direct proof of correctness of the groupByCycles function. Same comment on the fact it is a higher order function as in previous function documentation.
notreachedFirstasEndVertex :: (Integer,Integer) -> (Integer,Integer) -> Bool
notreachedFirstasEndVertex (fi1,si1) (fi2,si2) = fi1 /= si2

-- | groupByCycles is the function that takes a list of pairs of Integers (representing edges) and returns a list of lists of pairs of Integers where each sublist represents a cycle of the input list
-- | since the (notreachedFirstasEndVertex x) higher order function is satisfied until the element before the last element of the cycle, the last element in each cycle gets excluded by the span function and therefore it must be added to the end of the first sublist returned by the span function if, and only if, the last element of the first returned list by the span function chains with the first element of the second returned list by the span function
-- | this is the actual proof of correctness of the groupByCycles function
groupByCycles :: [(Integer,Integer)] -> [[(Integer,Integer)]]
groupByCycles [] = []
groupByCycles (x:xs) = (x:ys) : groupByCycles zs
    where (ys1,zs1) = span (notreachedFirstasEndVertex x) xs
          (ys,zs) = if chain (last ys1) (head zs1) then (ys1 ++ [head zs1],tail zs1) else (ys1,zs1)

-- | boundary is the function that converts a list of pairs of Integers (representing edges) into a list of Integers (representing the sequence of vertices corresponding to the sequence of edges)
boundary :: [(Integer,Integer)] -> [Integer]
boundary l = let body = fmap fst l
                 final = [snd (last l)]
             in body ++ final

-- | pairify is the function that converts a list of Integers representing a chain of edges into a pair of Integers representing its start and end nodes
pairify :: [Integer] -> (Integer,Integer)
pairify l = (head l, last l)

-- | breakMapIntoShortestCycles is the function that breaks a map of alphas into cycles stored as a list of lists of pairs of Integers
breakMapIntoShortestCycles :: CycleMap -> [[(Integer,Integer)]]
breakMapIntoShortestCycles = groupByCycles.(map pairify.Map.elems)

-- | breakMapIntoShortestCycles is the function that breaks a map of alphas into cycles stored as a list of lists of pairs of Integers
-- breakPairsMapIntoShortestCycles :: CyclePairsMap -> [[(Integer,Integer)]]
-- breakPairsMapIntoShortestCycles m = groupByCycles.Map.elems

-- | certifyalpha1s is the higher order function that takes as input the alpha0s map and returns a function that takes as input the combined alpha0s alpha1s map and certifies the alpha1s from the alpha0s and the combined alpha0s+alpha1s by breaking the alpha0s map into cycles and checking whether the result is the same as the combined alpha0s+alpha1s
certifyalpha1s :: CycleMap -> CyclePairsMap -> Bool
certifyalpha1s a0 a0a1 = let l = Map.elems a0a1
                             l' = breakMapIntoShortestCycles a0
                         in l == l'

-- | testcycle is the higher order function that takes as input the alpha0s map and returns a higher order function that takes as input a pair of edges identified by their Integer IDs and returns a function that takes as input a Boolean accumulator and tests whether two edges represented by the integer values given in the pair chain together. If it is the case it returns True, otherwise it returns False. The accumulator is tested first to see if it is False (then nothing needs to be done)
-- | Since this function is called on all pairs constructed from a map, including the pair formed from the last vertex to the first vertex, it checks whether a list forms a cycle or not (see documentation for the traverseMap function)
testcycle :: CycleMap -> (Integer,Integer) -> Bool -> Bool
testcycle m _ False = False
testcycle m (x1,x2) _ = let firstValue = Map.lookup x1 m
                            secondValue = Map.lookup x2 m
                        in ((isJust firstValue && isJust secondValue) && last(fromJust firstValue)==head(fromJust secondValue))

-- | testcyclepair is the higher order function that takes as input a list of edges represented as pairs of Integers and returns a function that takes as input a Boolean and returns another Boolean that certifies whether all elements in a list of combined alpha0 alpha1 elements chain together
testcyclepair :: [(Integer,Integer)] -> Bool -> Bool
testcyclepair _ False = False
-- | needed to stop checking for chaining omce the list has been reduced to a singleton
testcyclepair [x] b = b
testcyclepair (x:xs) b = let firstInteger = snd x
                             secondInteger = fst (head xs)
                         in firstInteger == secondInteger && testcyclepair xs True

-- | faceBoundary is a higher order function that takes as input the combined alpha0s alpha1s map and returns a function that takes as input a face identified by its Integer ID and returns the set of edges in the boundary of the face identified by the Integer in the combined alpha0s alphs1s map
faceBoundary :: CyclePairsMap -> Integer -> Set.Set (Integer, Integer)
faceBoundary m x = let l = Map.lookup x m
                   in if isJust l then (Set.fromList.fromJust) l else Set.empty

-- | sym is the function that returns the symmetric edge of the edge specified by a pair of Integer IDs
sym :: (Integer,Integer) -> (Integer,Integer)
sym (a,b) = (b,a)

-- | testfacecycle is the higher order function that takes as input the combined alpha0s alpha1s map and returns a function that takes as input a pair of Integers and returns a Boolean that expresses whether two faces identified by their IDs in the given pair have a common edge separating them
testfacecycle :: CyclePairsMap -> (Integer,Integer) -> Bool -> Bool
testfacecycle m _ False = False
testfacecycle m (x1,x2) _ = let firstBoundary = faceBoundary m x1
                                secondBoundary = Set.map sym (faceBoundary m x2)
                            in not $ Set.null $ Set.intersection firstBoundary secondBoundary

-- | getfacecycle is the higher order function that takes as input the combined alpha0s alpha1s map and returns a function that takes as input a pair of Integers and returns the set of common edges between two faces identified by their IDs in the given pair
getfacecycle :: CyclePairsMap -> (Integer,Integer) -> Set.Set (Integer,Integer)
getfacecycle m (x1,x2) = let firstBoundary = faceBoundary m x1
                             secondBoundary = Set.map sym (faceBoundary m x2)
                         in Set.intersection firstBoundary secondBoundary

-- | createcycle is the higher order function that takes as input a map of cycles and returns a function that takes as input an Integer (defining an edge as a list, i.e. a key in the alph0s map) and returns the pair formed by the first vertex and the last vertex in the list of vertices referenced by the given Integer from the given map. This function is used in the function constructfroma0.
createcycle :: CycleMap -> Integer -> (Integer,Integer)
createcycle m i = let value = Map.lookup i m
                  in if isJust value then (head $ fromJust value,last $ fromJust value) else (-1,-1)

-- | constructPairs is the function that constructs the list of pairs of consecutive vertices from the given list of vertices represented by their Interger IDs
constructPairs :: [Integer] -> [(Integer,Integer)]
constructPairs l = zip l (tail l) ++ [(last l, head l)]

-- | traverseMap is the higher order function used to traverse a map and collect whether eah one of its list of pairs of consecutive vertices is cyclic. This function is used in the checkalpha0s function.
traverseMap :: CycleMap -> [Integer] -> Bool -> Bool
traverseMap m l b = foldr' (testcycle m) b (constructPairs l)

-- | traverseFMap is the higher order function used to traverse a map and collect whether eah one of its list of pairs of consecutive vertices is cyclic. This function is used in the checkalpha0s function.
traverseFMap :: CyclePairsMap -> [Integer] -> Bool -> Bool
traverseFMap m l b = foldr' (testfacecycle m) b (constructPairs l)

-- | constructFroma0 is the higher order function that constructs the combined alpha0s alpha1s map one list of alpha1s at a time. This higher order function is used in the higher order function constructa0a1
constructFroma0 :: CycleMap -> [Integer] -> [(Integer,Integer)]
constructFroma0 = fmap.createcycle

-- | go is the higher order function that is used in the constructfroma0a1 function to get the union of common edges between pairs of consecutive faces using the testfacecycle and getfacecycle functions. getfacecycle is only called if testfacecycle function succeeds.
go :: CyclePairsMap -> (Integer,Integer) -> Set.Set(Integer,Integer) -> Set.Set(Integer,Integer)
go m (x1,x2) t = if testfacecycle m (x1,x2) True then Set.union (getfacecycle m (x1,x2)) t else t

-- | constructfroma0a1 is the higher order function that constructs the combined alpha0s alpha1s alpha2s map from the combined alpha0s alpha1s map one list of the alpha2s at a time. It right folds strictly the go function on the list of pairs of consecutive elements of the alpha2s
constructfroma0a1 :: CyclePairsMap -> [Integer] -> Set.Set(Integer,Integer)
constructfroma0a1 m l = foldr' (go m) Set.empty (constructPairs l)

-- | goc is the higher order function that is used in the constructfroma0a1 function to get the union of common edges between pairs of consecutive faces using the testfacecycle and getfacecycle functions. getfacecycle is only called if testfacecycle function succeeds.
goc :: CyclePairsMap -> (Integer,Integer) -> Set.Set(Integer,Integer) -> Set.Set(Integer,Integer)
goc m (x1,x2) t = if testfacecycle m (x1,x2) True then Set.union (getfacecycle m (x1,x2)) t else t

-- | constructfroma0a1 is the higher order function that constructs the combined alpha0s alpha1s alpha2s map from the combined alpha0s alpha1s map one list of the alpha2s at a time. It right folds strictly the go function on the list of pairs of consecutive elements of the alpha2s
constructcfroma0a1 :: CyclePairsMap -> [Integer] -> Set.Set(Integer,Integer)
constructcfroma0a1 m l = foldr' (goc m) Set.empty (constructPairs l)

-- | This higher order function checks whether the first given cycle corresponds to the cycles given in the second given input cycle by calling the TraverseMap function to check whether each list of vertices in the alpha0s addressed by the Integers in the alpha1s are cyclic.
checkalpha0s :: CycleMap -> CycleMap -> Bool
checkalpha0s d0m = foldr' (traverseMap d0m) True

-- | constructa0a1 is a higher order function that builds the combined map of the alpha0s and the alpha1s by applying the constructFroma0 a0 higher order function to each element of the a1 map / hash table
constructa0a1 :: CycleMap -> CycleMap -> CyclePairsMap
constructa0a1 = fmap.constructFroma0

-- | checks whether each element of the combined map of the alpha0s and alpha1s chains with the next one, see documentation for testcyclepair function
checka0sa1s :: CyclePairsMap -> Bool
checka0sa1s = foldr' testcyclepair True

-- | checks whether the alpha1s follow from the combined map of the alpaha0s and alpha1s
checkalpha1s :: CyclePairsMap -> CycleMap -> Bool
checkalpha1s a0a1 = foldr' (traverseFMap a0a1) True

-- | constructs the combined map of alpha0s alpha1s amd alpha2s by mapping the constructfroma0a1 higher order function to all the alpha2 elements
constructa0a1a2 :: CyclePairsMap -> CycleMap -> Decomposition
constructa0a1a2 = fmap.constructfroma0a1

-- | constructs the combined map of alpha0s alpha1s amd alpha2s by mapping the constructfroma0a1 higher order function to all the alpha2 elements
constructca0a1a2 :: CyclePairsMap -> CycleMap -> Decomposition
constructca0a1a2 = fmap.constructcfroma0a1

-- | gob is the higher order function that takes as input the combined map of the alpha0s and alpha1s and the Integer identifying a given volume in the alpha2s and a set of pairs of Integers and adds to it the set of vertex IDs (Integers) of that face. It is used to collect the set of edges of the boundary of each volume in the alpha2s.
gob :: CyclePairsMap -> Integer -> Set.Set(Integer,Integer) -> Set.Set(Integer,Integer)
gob cpm ss ss2= Set.union (faceBoundary cpm ss)ss2

-- | constructbfroma0a1 is a higher order function that takes as input the combined map of the alpha0s and alpha1s and returns a function that takes a list of faces from a volume of the alpha2s and returns the
constructbfroma0a1 :: CyclePairsMap -> [Integer] -> Set.Set(Integer,Integer)
constructbfroma0a1 a0a1 = foldr' (gob a0a1) Set.empty

-- | constructBoundaries is a higher order function that takes as inoput the combined map of the alpha0s and alpha1s and returns a function that takes the alpha2s map and returns for each volume the set of orbits of edges starting at each vertex of the volume (i.e. the boundary of the volume where each unoriented edge is present twice as each of its oriented edges).
constructBoundariesa0a1a2 :: CyclePairsMap -> CycleMap -> Decomposition
constructBoundariesa0a1a2 = fmap.constructbfroma0a1

-- | go2 is the higher order function that is used in the constructfroma0a1 function to get the union of common edges between pairs of consecutive faces using the testfacecycle and getfacecycle functions. getfacecycle is only called if testfacecycle function succeeds.
go2 :: CyclePairsMap -> (Integer,Integer) -> [[Integer]] -> [[Integer]]
go2 m (x1,x2) t = if testfacecycle m (x1,x2) True then if (last t == []) then (init t)++[[x1,x2]] else (init t) ++ [(last t) ++ [x1,x2]] else t ++ [[]]

-- | constructa2Froma0a1 is the function that takes as input a combined alpha0 alpha1 map and produces the resulting alpha2 map
-- constructa2Froma0a1 :: CyclePairsMap -> CycleMap
-- constructa2Froma0a1 a0a1 = Map.fromList (foldr' go2 a0a1 [])

-- | gob is the higher order function that takes as input the combined map of the alpha0s and alpha1s and the Integer identifying a given volume in the alpha2s and a set of pairs of Integers and adds to it the set of vertex IDs (Integers) of that face. It is used to collect the set of edges of the boundary of each volume in the alpha2s.
-- gob :: CyclePairsMap -> Integer -> Set.Set(Integer,Integer) -> Set.Set(Integer,Integer)
-- gob = Set.union.faceBoundary

-- | constructbfroma0a1 is a higher order function that takes as input the combined map of the alpha0s and alpha1s and returns a function that takes a list of faces from a volume of the alpha2s and returns the
-- constructbfroma0a1 :: CyclePairsMap -> [Integer] -> Set.Set(Integer,Integer)--
-- constructbfroma0a1 a0a1 = foldr' (gob a0a1) Set.empty

-- | constructBoundaries is a higher order function that takes as inoput the combined map of the alpha0s and alpha1s and returns a function that takes the alpha2s map and returns for each volume the set of orbits of edges starting at each vertex of the volume (i.e. the boundary of the volume where each unoriented edge is present twice as each of its oriented edges).
-- constructBoundariesa0a1a2 :: CyclePairsMap -> CycleMap -> Decomposition
-- constructBoundariesa0a1a2 = fmap constructbfroma0a1

-- | testfacecyclepair is a higher order function that takes as input a boundary (set of pairs of Integers) and returns a function that takes a Boolean and returns that Boolean and the boolean that expresses whether the boundary is not empty. It is used by the checka0sa1sa2s function.
testfacecyclepair :: Set.Set(Integer,Integer) -> Bool -> Bool
testfacecyclepair _ False = False
testfacecyclepair s True = not (Set.null s)

-- | checka0sa1sa2s is the function that takes the combined map alpha0s alpha1s alpha2s and tests whether each common boundary of two adjacent faces is not empty. This is done by checking each common boundary at a time with the strict right fold function foldr'.
checka0sa1sa2s :: Decomposition -> Bool
checka0sa1sa2s = foldr' testfacecyclepair True

-- | testFaceEquality is the higher order function that takes as input the combined alpha0s alpha1s map and returns a higher order function that takes a pair of faces identified by their Integer IDs and returns a higher order function that takes a Boolean accumulator and returen that Boolean and the Boolean that expresses whether the two faces are identical.
testFaceEquality :: CyclePairsMap -> (Integer,Integer) -> Bool -> Bool
testFaceEquality m (x1,x2) b = let firstBoundary = faceBoundary m x1
                                   secondBoundary = Set.map sym (faceBoundary m x2)-- All boundaries taken with reverse orientation!
                               in firstBoundary == secondBoundary

-- | checkalpha3s is the higher order function that takes as input the combined alpha0s alpha1s map and returns a function that takes as input the alpha3 map and returns a Boolean that certifies wehether the alpha3 map is correct.
checkalpha3s :: CyclePairsMap -> SingleValueMap -> Bool
checkalpha3s m l = foldr' (testFaceEquality m) True (Map.assocs l)

-- | errora0sa1sa2s is the function that filters out of a Decomposition all these that are empty. If a Decomposition is valid the set returned by this function should be empty
errora0sa1sa2s :: Decomposition -> Decomposition
errora0sa1sa2s = Map.filter Set.null

-- | constructa3Froma0a1 is the function that constructs the alpha3s from th alpha0s and alpha1s
-- constructa3Froma0a1 :: CyclePairsMap -> (Integer,[Integer]) -> SingleValueMap -> SingleValueMap
-- constructa3Froma0a1 a0a1 a2keylist svmi svm = foldr' (getNeighbors a0a1) Map.empty (snd a2keylist)

-- | constructa3s is the function that constructs the alpha3s from the combined alpha0s and alpha1s map and the alpha2s.
-- constructa3s :: CyclePairsMap -> CycleMap -> SingleValueMap
-- constructa3s a0a1 a2 = foldr' (constructa3Froma0a1 a0a1) (Map.assocs a2)

-- | isExternalFaceConnectivity tests whether a pair of external faces have a common proper intersection and combines this result with the passed Boolean value, used in a strict foldr
isExternalFaceConnectivity :: CyclePairsMap -> (Integer,Integer) -> Bool -> Bool
isExternalFaceConnectivity m (x1,x2) b = let firstBoundary = faceBoundary m x1
                                             secondBoundary = Set.map sym (faceBoundary m x2)-- All boundaries taken with reverse orientation!
                                             i = Set.intersection firstBoundary secondBoundary
                                             j = i/= Set.empty
                                             k = i/= firstBoundary
                                         in j && k

certifyExternalShells :: CyclePairsMap -> [Integer] -> Bool
certifyExternalShells pm l = let cl = map (\x -> -x) l
                                 cpl = constructPairs cl
                             in foldr' (isExternalFaceConnectivity pm) True cpl
