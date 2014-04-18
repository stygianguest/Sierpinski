module Sierpinski where

import qualified Data.Set as S
import Data.List (sort, partition)

traditional :: [a] -> [(a,a)]
traditional [] = []
traditional lst@(hd:tl) = zip (repeat hd) lst ++ traditional tl


--TODO: implement the greedy solution that tries to locally minimize cost
--greedy :: [a] -> [(a,a)]
--greedy [
--  where gs stack = 

check lst = order (szierpinski lst) == order (traditional lst)

order :: [(Int,Int)] -> [(Int,Int)]
order = sort . map (\(a,b) -> if a <= b then (a,b) else (b,a))

--carefull, only works if integers in joblist >= 0
--cacheCost :: [(Int,Int)] -> Int
--cacheCost =
--  let 
--    -- has a space leak, but I don't care
--    push :: Int -> [Int] -> [Int]
--    push 0 (hd:tl) = 1 : push (-1) tl -- token is now on top of stack
--    push i (0:tl) = 0 : push (i-1) tl -- don't add other things to stack
--    push i (hd:tl) = hd+1 : push (i-1) tl -- move all others down in stack
--
--    -- FIXME: the order of lookups for left and right?
--    --        could do an order agnostic push (move both to the top)
--    cost :: (Int, [Int]) -> (Int,Int) -> (Int,[Int])
--    cost (tot, stack) (p,q) = (tot+(stack!!p)+(stack!!q), push p $ push q stack)
--
--  in fst . foldl cost (0, repeat 0)

--workingSets :: Int -> [(Int,Int)] -> [S.Set Int]
--workingSets size [] = []
--workingSets size (hd:tl) = mkSet (take size (hd:tl)) : workingSets size tl
--    where mkSet = (\(xs,ys) -> S.fromList xs `S.union` S.fromList ys) . unzip

workingSetSeries :: [(Int,Int)] -> [[S.Set Int]]
workingSetSeries = takeWhile (not . null) . iterate mkSeries . map mkSet
    where mkSeries [] = []
          mkSeries ws@(_:tl) = zipWith S.union tl ws
          mkSet (a,b) = S.fromList [a,b]

--main = printData

main = mapM_ putStrLn 
    $ map (\(x,y) -> show x ++ "\t" ++ show y) 
    $ map (\(a,b) -> if a <= b then (a,b) else (b,a))
    $ szierpinski [0..10]

printData :: IO ()
printData = mapM_ putStrLn table
  where val f = map (f . map S.size) . workingSetSeries
        changes [] = []
        changes ws = 0 : (map S.size $ zipWith S.difference (tail ws) ws)
        addCol = zipWith (\x y -> x ++ '\t' : y)
        table = map show [1..]
                `addCol` map (show . sum . changes) (workingSetSeries trad)
                `addCol` map (show . sum . changes) (workingSetSeries zierp)
                --`addCol` mkCol minimum trad
                --`addCol` mkCol median trad
                --`addCol` mkCol maximum trad
                --`addCol` mkCol minimum zierp
                --`addCol` mkCol median zierp
                --`addCol` mkCol maximum zierp
        mkCol crit = map show . val crit
        trad = traditional items
        zierp = szierpinski items
        items = [1..64]



-- c/p from rosetta code http://rosettacode.org/wiki/Averages/Median#Haskell
nth (x:xs) n
    | k == n    = x
    | k > n     = nth ys n
    | otherwise = nth zs $ n - k - 1
    where (ys, zs) = partition (<x) xs
          k = length ys
 
median xs = nth xs $ length xs `div` 2


--maxWorkingSetSize :: Int -> [(Int,Int)] -> Int
--maxWorkingSetSize w = foldl max 0 . workingSets w
--
--totWorkingSetSize :: Int -> [(Int,Int)] -> Int
--totWorkingSetSize w = sum . workingSets w

--workingSetChanges :: Int -> [(Int,Int)] -> [S.Set Int]
--workingSetChanges w lst = zipWith S.difference (tail ws) ws
--    where ws = workingSets w lst
   

szierpinski lst = zierp lst lst

--TODO: swap to keep x and y of jobs correct (don't want to sort by size)
zierp [] _ = []
--zierp _ [] = [] --FIXME: why not?
zierp (x:xs) (y:ys) =
    (x,y) : trapi [x] xs ys

-- invariant: length yss == length xsss and length x is power of 2
trapi _ _ [] = []
--FIXME: why not stop when the others are empty?
trapi x xsss yss =
    zierp y (reverse x)
    ++ zierp (reverse (tail y)) (drop (n - length y + 1) x)
    ++ zierp xs y
    ++ trapi (x++xs) xss ys
  where n = length x
        (xs,xss) = splitAt n xsss
        (y,ys) = splitAt n yss

