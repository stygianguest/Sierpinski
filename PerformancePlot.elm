module PerformancePlot where

import Schedule
import Simulator(totalCacheMisses)
import Util(signalFromList, buffer)

main = 
    let noStations = 64

        w = 600 
        h = 300

        --perf sched = map (\m -> (m, totalCacheMisses m sched)) [2..noStations]
        perf sched = pacedMap (\m -> (m, totalCacheMisses m sched)) [2..noStations]

        sierp = Schedule.sierpinski noStations
        scan = Schedule.scanline noStations
        
        sierpDot = alpha 0.8 <| filled blue <| rect 4 4
        scanDot = alpha 0.8 <| scale 1.5 <| filled red <| polygon [(-2,-1),(2,-1),(0,2)]

        plot sierpPerf scanPerf = plotCostCurves w h "mem capacity →" "cost →" [(sierpDot, sierpPerf), (scanDot, scanPerf)]

        clock = fps 6

    in plot <~ perf sierp clock ~ perf scan clock


pacedMap : (a -> b) -> [a] -> Signal c -> Signal [b]
pacedMap f lst clock =
    if | isEmpty lst -> constant []
       | otherwise   -> buffer (f <~ signalFromList lst clock)



plotCostCurves : Int -> Int -> String -> String -> [(Form, [(Int, Int)])] -> Element
plotCostCurves w h xLabel yLabel curves =
    let lineWidth = 2
        labelHeight = 10
        arrowSize = 6
        ylen = toFloat (h-lineWidth-2*arrowSize-4*labelHeight)
        xlen = toFloat (w-lineWidth-2*arrowSize-4*labelHeight)
        maxX = 64--maximum <| map fst <| concatMap snd curves
        maxY = 2100--maximum <| map snd <| concatMap snd curves
        xscale = (xlen - labelHeight) / toFloat maxX
        yscale = (ylen - labelHeight) / toFloat maxY

        axesStyle = { defaultLine | width <- lineWidth, join <- Smooth, cap <- Round }
        axeLines = group <| map (traced axesStyle . path) <| [[(0,0),(0,ylen)], [(0,0),(xlen,0)]]
        
        arrowPoint = scale arrowSize <| filled black <| polygon [(0,0.7), (0.6,-0.3), (0,-0), (-0.6,-0.3)]
        axeArrows = group [move (0, ylen) arrowPoint, move (xlen, 0) <| rotate (degrees 270) arrowPoint]
        
        xLabelForm = move (xlen/2,-2*labelHeight) <| toForm <| plainText xLabel
        yLabelForm = move (-3*labelHeight,ylen/2) <| rotate (degrees 90) <| toForm <| plainText yLabel
        
        xTick i = move (toFloat i * xscale,-lineWidth/2) <| filled black <| rect (lineWidth/2) (lineWidth*2)
        xLabelTicks = group <| map xTick [0..maxX]
        xTickLabel i = scale 0.5 <| move (toFloat i * xscale, -labelHeight) <| toForm <| plainText (show i)
        xTickLabels = group <| map xTickLabel <| map ((*) 4) [0..16]
        
        yTick i = move (-lineWidth/2, toFloat i * yTickScale) <| filled black <| rect (lineWidth*2) (lineWidth/2) 
        noYTicks = floor (ylen / labelHeight)
        yTickScale = ylen / toFloat noYTicks
        yLabelTicks = group <| map yTick [0..noYTicks-1]
        yTickLabel i = scale 0.5 <| move (-labelHeight, toFloat i * yTickScale*2)
            <| toForm <| plainText <| show <| round <| yTickScale*2 * toFloat i / yscale
        yTickLabels = group <| map yTickLabel <| [0..(noYTicks `div` 2) - 1]

        axes = group [axeLines, axeArrows, xLabelForm, yLabelForm, xLabelTicks, xTickLabels, yLabelTicks, yTickLabels]

        drawFormAt f (x,y) = move (toFloat x * xscale, toFloat y * yscale) f 
        costPoints = concatMap (\(f,ps) -> map (drawFormAt f) ps) curves
        
        moveOrigin = move (-xlen/2 + lineWidth + 2*labelHeight, -ylen/2 + lineWidth + 2*labelHeight)

    in collage w h <| map moveOrigin <| axes :: costPoints
