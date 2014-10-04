module ScanlineCost where

import Schedule
import Simulator
import Util(enumerate, repeatN)

noStations = 8
noJobs = Schedule.noJobs noStations
memSize = 5

main = plotScheduleCost noJobs memSize (Schedule.scanline noStations)

plotScheduleCost : Int -> Int -> Schedule.Schedule -> Element
plotScheduleCost noJobs mem schedule =
    let cumulMisses = if isEmpty schedule then [] else scanl1 (+) <| Simulator.cacheMisses mem schedule
        points = map (\(i,j) -> (show i, j)) <| zip [1..noJobs] (cumulMisses++zeros)
        zeros = repeatN (noJobs - length schedule) 0
    in plotStaircaseGraph 14 300 points

-- please don't look at this code
-- thank you for your discretion
plotStaircaseGraph : Float -> Float -> [(String, Int)] -> Element
plotStaircaseGraph xscale ylen points =
    let noBars = length points
        -- what did I just tell you?
        maxY = maximum <| map snd points
        lineWidth = 2
        axesStyle = { defaultLine | width <- lineWidth, join <- Smooth, cap <- Round }
        xlen = toFloat noBars * xscale + xscale/2
        --ylen = (1+toFloat maxY) * yscale
        yscale = ylen / toFloat (maxY+1)
        --TODO: move axes by linedwidth/2 down left
        axes = map (traced axesStyle << path) <| [[(0,0),(0,ylen)], [(0,0),(xlen,0)]]
        arrowpoint = scale (lineWidth*3) <| filled black <| polygon [(0,0.7), (0.6,-0.3), (0,-0), (-0.6,-0.3)]
        arrows = [move (0, ylen) arrowpoint, move (xlen, 0) <| rotate (degrees 270) arrowpoint]
        bars = map mkBar <| filter (\(x,y) -> y > 0) <| enumerate <| map snd points
        mkBar (x, y) =
            let x' = toFloat x * xscale
                y' = toFloat y * yscale
            in move (x' + xscale/1.9, y'/2) <| group [filled blue <| rect (xscale * 0.9) y', scale (xscale/20) <| toForm <| plainText <| show y]
        xTicks = map mkXTick <| enumerate <| map fst points
        mkXTick (i,str) = move (xscale * (0.5 + toFloat i), -xscale/2) <| scale (xscale/20) <| toForm <| plainText str
        xLabel = [scale (xscale/15) <| move (xlen/2, -2.3 * xscale/2) <| toForm <| plainText "job  →"]
        yLabel = [scale (xscale/15) <| rotate (degrees 90) <| move (-xscale/2, ylen/2) <| toForm <| plainText "cost  →"]
        w = ceiling <| xlen + xscale*2
        h = ceiling <| ylen + xscale*3
    --in asText (enumerate <| map fst points)
    -- just know that I am as disappointed in you as you are in me
    in collage w h [move (-xlen/2, -ylen/2 + xscale) <| group <| concat [arrows,bars,xLabel,yLabel,xTicks,axes]]
