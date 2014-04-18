module Schedule where

import Set

import Util(enumerate)

type Schedule = [Job]
type Job = (Int, Int)

empty = []

scanline : Int -> Schedule
scanline noStations = 
    let mkRow i = map ((,) i) [1..i]
    in concatMap mkRow [1..noStations]

-- randomSchedule : Int -> Signal Schedule

-------


upS = (0,-1)
downS = (0,1)
leftS = (-1,0)
rightS = (1,0)

rotate180cw (x,y) = (-x,-y)

addSteps (sx,sy) (x,y) = (sx + x, sy + y)

sierpTriangle n =
    if n <= 0 
        then [rightS, downS, rightS, upS, rightS, downS, downS, leftS, (1,1)]
        else sierpTriangle (n-1) ++ rightS :: reverse (squareI (n-1)) ++ (-1,1) :: sierpTriangle (n-1)

triangleH n =
    if n <= 0 
        then [downS, downS, downS, rightS, upS, rightS, downS]
        else triangleH (n-1) ++ downS :: squareI (n-1) ++ rightS :: triangleH (n-1)
triangleI n =
    if n <= 0
        then [downS, leftS, upS, leftS, downS, downS, downS]
        else triangleI (n-1) ++ leftS :: map rotate180cw (squareH (n-1)) ++ downS :: triangleI (n-1)

squareH n = triangleH n ++ rightS :: map rotate180cw (triangleH <| n)
squareI n = triangleI n ++ rightS :: map rotate180cw (triangleI <| n)

closestGen : Int -> Int
closestGen n = floor (logBase 2 <| toFloat n) - 2 

hindex = scanl addSteps (1,1) . squareH . closestGen
sierpinski = scanl addSteps (1,1) . sierpTriangle . closestGen

-----

-- verify that the schedule contains all jobs
-- and only uses valid stations
isCompleteSched : Int -> Schedule -> Bool
isCompleteSched noStations sched =
    let validStations = all (\(i,j) -> all (isInRange 1 (noStations+1)) [i,j]) sched
        isInRange from upto i = i >= from && i < upto
        noUniqueJobs = length <| Set.toList <| Set.fromList <| orderJobs sched
    in validStations && noUniqueJobs == noJobs noStations

removeDups : Schedule -> Schedule
removeDups =
    let trav visited sched =
            case sched of
            job :: tl ->
                let ojob = orderJob job
                    visited' = Set.insert ojob visited
                in if Set.member ojob visited then trav visited tl else job :: trav visited' tl
            [] -> []
    in trav Set.empty

orderJobs : Schedule -> Schedule
orderJobs = map orderJob

orderJob (i,j) = if i >= j then (i,j) else (j,i)

noJobs : Int -> Int
noJobs noStations = ((noStations * noStations - noStations) `div` 2) + noStations
   
append : Job -> Schedule -> Schedule
append (i,j) sched = 
    case sched of 
    (k,l) :: tl -> if (i,j) == (k,l) || (j,i) == (k,l)  
                        then append (i,j) tl 
                        else (k,l) :: append (i,j) tl
    [] -> [orderJob (i,j)]

contains : Schedule -> Job -> Bool
contains sched job = 
    let job' = orderJob job
    in any ((==) job') sched

remove : Job -> Schedule -> Schedule
remove job =
    let job' = orderJob job
    in filter ((/=) job')

--------------------------------------------------------------------------------
-- draw the schedule as a path through a matrix

drawScheduleWithIndex : Float -> Int -> Schedule -> Element
drawScheduleWithIndex jobDist noStations sched =
    let path = drawSchedule jobDist noStations sched
        w = ceiling jobDist

        mkCell = container w w middle
        titleColumn = flow down <| map (mkCell . plainText . show) [1..noStations]
        titleRow = flow right <| mkCell (spacer w w) :: map (mkCell . plainText . show) [1..noStations]

    in titleRow `above` (titleColumn `beside` path `beside` titleColumn) `above` titleRow


drawSchedule : Float -> Int -> Schedule -> Element
drawSchedule jobDist noStations sched =
    let lineWidth = 2
        elemSize = jobDist * (toFloat noStations)
        jobCoords (i,j) = (jobDist * (0.5 + toFloat j - toFloat noStations / 2 - 1), -jobDist * (toFloat i - toFloat noStations/2 - 0.5))
        pathStyle = { defaultLine | width <- lineWidth, join <- Smooth, cap <- Round }
        schedPath = traced pathStyle <| path coords
        coords = map jobCoords sched
        jobNumber (n,p) = move p <| group 
            [filled black <| circle (jobDist/3), filled white <| circle (jobDist/3 - lineWidth), jobLabel n]
        jobLabel = scale (jobDist/40) . toForm . plainText . show
        jobs = map jobNumber (enumerate coords)
    in collage (ceiling elemSize) (ceiling elemSize) (schedPath :: jobs)

--drawSchedule : Int -> Schedule -> Element
--drawSchedule noStations sched =
--    let (xstep, ystep) = ((toFloat w - lineWidth) / (toFloat noStations + 1), -(toFloat h - lineWidth) / (toFloat noStations + 1))
--        (w,h) = (500,500)
--        lineWidth = 4
--        jobCoords (i,j) = (xstep * toFloat i - toFloat w / 2, ystep * toFloat j + toFloat h / 2)
--        pathStyle = { defaultLine | width <- lineWidth, join <- Smooth, cap <- Round }
--        schedPath = traced pathStyle <| path coords
--        coords = map jobCoords sched
--        jobNumber (n,p) = move p <| group 
--            [filled black <| circle (xstep/3), filled white <| circle (xstep/3 - lineWidth), jobLabel n]
--        jobLabel = scale (xstep/40) . toForm . plainText . show
--        jobs = map jobNumber (enumerate coords)
--    in collage w h (schedPath :: jobs)
