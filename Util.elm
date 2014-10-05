module Util where

import Random
import Dict
import Set
import Maybe (isJust)
import Transform2D(..)

--------------------------------------------------------------------------------
-- utility functions  concerning randomness

-- implementation of Fisher-Yates shuffle with the Knuth algorithm
-- if implemented correct it returns an unbiased permutation of the given list
--shuffle : Signal [a] -> Signal [(Int, Int)]--Signal [a]
--FIXME: it seems Dict.values returns values in-order, but it's not documented
shuffle : Signal [a] -> Signal [a]
shuffle cards = 
    let cardsDict = Dict.fromList << enumerate <~ cards
        cardCnt = length <~ cards
        floatToRange (x,y) = (x, floor <| y * (toFloat <| x+1))
        swapPairs = reverse << map floatToRange << enumerate <~ Random.floatList cardCnt
        applySwaps cdict = foldl (<|) cdict << map (uncurry swap)
    in Dict.values <~ (applySwaps <~ cardsDict ~ swapPairs)

swap : comparable -> comparable -> Dict.Dict comparable a -> Dict.Dict comparable a
swap keyX keyY dct = 
    let valX = fromJust <| Dict.get keyX dct
        valY = fromJust <| Dict.get keyY dct
    in Dict.insert keyX valY <| Dict.insert keyY valX dct

rangeList : Int -> Int -> Signal Int -> Signal [Int]
rangeList from to len =
    let mapFloatToRange x = (floor <| x * (toFloat <| to-from+1)) + from
    in map mapFloatToRange <~ Random.floatList len

--------------------------------------------------------------------------------
-- 

--main = 
--    let animationForm = singleton <~ loadingAnimation animationToggle
--        singleton x = [x]
--        animationToggle = toggle Mouse.clicks
--    in uncurry collage <~ Window.dimensions ~ animationForm

loadingAnimation : Signal Bool -> Signal Form
loadingAnimation isVisible =
    let circles = map mkcircle [0..5]
        mkcircle i = groupTransform (transform i) << singleton <| solid black `outlined` circle 2
        singleton x = [x]
        transform i = rotation (degrees <| i * 60) `multiply` translation 0 7

        rotateLoader a = groupTransform (rotation -a) circles
        angle = degrees << (\a->a*1.5) << toFloat <~ modCount 360 clock
        clock = keepWhen isVisible millisecond (fps 30) 

        emptyForm = group []
        emptyFormSig = sampleOn isVisible <| constant emptyForm

    in muc isVisible emptyForm (rotateLoader <~ angle) emptyFormSig

--------------------------------------------------------------------------------

--FIXME: actually is more like a zipwith for dicts?
--unionWith : (Maybe a -> Maybe b -> c) -> Dict.Dict comparable a -> Dict.Dict comparable b -> Dict.Dict comparable c
--unionWith cmb dictA dictB =
--    let allKeys =  Set.toList <| Set.fromList (Dict.keys dictA) `Set.union` Set.fromList (Dict.keys dictB)
--        insertCombined k = Dict.insert k <| cmb (Dict.get k dictA) (Dict.get k dictB)
--    in foldl insertCombined Dict.empty allKeys

zipDict : Dict.Dict comparable a -> Dict.Dict comparable b -> Dict.Dict comparable (a,b)
zipDict dictA dictB =
    let allKeys =  Set.toList <| Set.fromList (Dict.keys dictA) `Set.intersect` Set.fromList (Dict.keys dictB)
        insertCombined k = Dict.insert k (fromJust <| Dict.get k dictA, fromJust <| Dict.get k dictB)
    in foldl insertCombined Dict.empty allKeys

--------------------------------------------------------------------------------
-- signal utility functions

-- only give the first N values of a signal
keepN : Int -> Signal a -> Signal a
keepN n sig =
    let clock = keepIf (\x -> x < n) 0 <| count sig
    in sampleOn clock sig

dropN : Int -> Signal a -> Signal a
dropN n sig =
    let clock = dropIf (\x -> x < n) 0 <| count sig
    in sampleOn clock sig

-- switch between signals
muc : Signal Bool -> a -> Signal a -> Signal a -> Signal a
muc switch init a b =
    let currentA = keepWhen (sampleOn a switch) init a
        currentB = dropWhen (sampleOn b switch) init b
    in merge currentA currentB

modCount : Int -> Signal a -> Signal Int
modCount m =
    let add _ a = (a + 1) % m
    in foldp add 0


toggle : Signal a -> Signal Bool
toggle = foldp (\_ a -> not a) False

toggleOnce : Signal a -> Signal Bool
toggleOnce = foldp (\_ a -> True) False

-- would've called this dropNothing but imho that is a rather confusing name :)
dropAbsent : a -> Signal (Maybe a) -> Signal a
dropAbsent init s = fromJust <~ keepIf isJust (Just init) s

-- returns result of iterative applications of given functions
-- paced by a clock signal
iterateOn : (a -> a) -> a -> Signal b -> Signal a
iterateOn f = foldp (\a b -> f b)

signalFromList : [a] -> Signal b -> Signal a
signalFromList lst =
    lift head<<dropIf isEmpty lst<<foldp (\_ -> tail) (head lst :: lst)

buffer : Signal a -> Signal [a]
buffer = foldp (::) []


--------------------------------------------------------------------------------
-- Generic, prelude-like utilility functions
--------------------------------------------------------------------------------
--TODO: use trampolines or folds rather than recursion

--fromJust x = case x of Just a -> a
fromJust (Just x) = x

enumerate : [a] -> [(Int, a)]
enumerate lst = zip [0..length lst - 1] lst

--TODO: rename to iterateN in keeping with haskell
applyN : (a -> a) -> Int -> a -> [a]
applyN f times init =
    case times of
    0 -> []
    n -> init :: applyN f (times - 1) (f init)

iterateWhile : (a -> a) -> (a -> Bool) -> a -> [a]
iterateWhile f cond init =
    if cond init then init :: iterateWhile f cond (f init) else []

applyUntil : (a -> a) -> (a -> Bool) -> a -> a
applyUntil f cond init = if cond init then init else applyUntil f cond (f init)

(!!) : [a] -> Int -> a
lst !! i = case lst of
    hd :: tl -> if i <= 0 then hd else tl !! (i-1)

repeatN : Int -> a -> [a]
repeatN n elem = 
  if n > 0 then elem :: repeatN (n-1) elem else []

combinations : [a] -> [b] -> [(a,b)]
combinations lstA lstB =
    case lstA of
        []          -> []
        hd :: tl    -> map ((,) hd) lstB ++ combinations tl lstB

least : (a -> a -> Bool) -> a -> [a] -> a
least isLE =
    let min x y = if x `isLE` y then x else y
    in foldr min

greatest cmp = least (\x y -> cmp y x)

