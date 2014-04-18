module Simulator where

import Schedule(..)
import Util(enumerate,repeatN)

main = 
    let mem = 6
        stations = 8
        schedule = scanline stations
        cstate = map snd <| cacheState mem <| schedule
        cstateElem = map asText cstate
        misses = cacheMisses mem <| schedule
        cumulMisses = scanl1 (+) misses
        missesElem = map asText cumulMisses
        scheduleElem = map asText <| schedule
        noJobs = length schedule

    in flow down 
        [ flow right <| zipWith above scheduleElem <| zipWith above cstateElem missesElem 
        , asText <| isCompleteSched stations schedule
        , asText <| sum misses
        --, drawSchedule 10 32 (hindex 3)
        --, asText <| (hindex 2)

        ]

----
-- simulation

cacheState : Int -> Schedule -> [Cache]
cacheState mem sched =
    let loadJob (i,j) = load i . load j
    in scanl loadJob (emptyCache mem) sched

cacheMisses : Int -> Schedule -> [Int]
cacheMisses mem sched =
    let caches = cacheState mem sched
        cntMisses (i,j) cache = 
            toInt (cache `contains` i) + toInt (i == j || contains cache j)
        toInt b = if b then 0 else 1
    in zipWith cntMisses sched caches

totalCacheMisses : Int -> Schedule -> Int
totalCacheMisses mem = sum . cacheMisses mem


----
-- the cache

type Cache = (Int, [Int])

emptyCache size = (size, [])

contains : Cache -> Int -> Bool
contains (s,contents) i = any (\j -> i == j) contents

remove : Int -> Cache -> Cache
remove i (s,contents) = (s,filter (\j -> i /= j) contents)

load : Int -> Cache -> Cache
load i (s,contents) = 
    if | (s,contents) `contains` i -> (s, i :: filter (\j -> i/=j) contents)
       | length contents < s       -> (s, i :: contents)
       | otherwise                 -> (s, i :: take (s-1) contents)
