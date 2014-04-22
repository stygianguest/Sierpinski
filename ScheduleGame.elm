module ScheduleGame where

import Char
import String
import Graphics.Input as Input
import Window

import Schedule
import Simulator
import Util(repeatN, enumerate, (!!))

--main : Signal Element
main = 
    let state = foldp execCmd initSimState commands.signal
        noJobs = Schedule.noJobs noStations
        drawScheduleAndPlot s = drawButtons s.memorySize s.schedule
    in drawScheduleAndPlot <~ state

-- INPUTS

commands : Input.Input Command
commands = Input.input ResetSchedule

-- MODEL

noStations = 8

data Command
    = ScheduleJob Int Int
    | IncMemory
    | DecMemory
    | ResetSchedule
    | UndoSchedule
    | SetSchedule Schedule.Schedule

type SimulatorState = 
    { memorySize : Int
    , schedule : Schedule.Schedule
    }

initSimState : SimulatorState
initSimState =
    { memorySize = 5
    , schedule = Schedule.empty
    }

execCmd : Command -> SimulatorState -> SimulatorState
execCmd cmd state =
    case cmd of
    ScheduleJob i j -> { state | schedule <- (if state.schedule `Schedule.contains` (i,j) then Schedule.remove (i,j) else Schedule.append (i,j)) state.schedule }
    IncMemory -> { state | memorySize <- (state.memorySize + 1) `min` noStations}
    DecMemory -> { state | memorySize <- (state.memorySize - 1) `max` 2}
    ResetSchedule -> { state | schedule <- Schedule.scanline noStations }

-- DISPLAY

buttonSize : number
buttonSize = 40

drawButtons : Int -> Schedule.Schedule -> Element
drawButtons mem sched =
    let s = buttonSize
        
        titleCell t = container s s middle <| plainText t
        titles = map (titleCell . show) [1..noStations]
        titleRow = flow right titles
        titleCol = flow down <| container s s middle empty :: titles

        jobButton i j =
            let cmd = ScheduleJob i j
                cost = Simulator.jobCost i j cache --only values are 0,1,2
                bColor = [green, yellow, red] !! cost
                bCircle a = collage s s [alpha a <| filled bColor (circle 10), toForm <| plainText <| show cost]
            in Input.customButton commands.handle cmd (bCircle 0.8) (bCircle 0.5) (bCircle 0.7)

        mkRow i = flow right <| map (jobButton i) [1..i]
        buttonMatrix = flow down <| map mkRow [1..noStations]
        
        path = Schedule.drawSchedule buttonSize noStations sched

        emptyControllCell = container (s*2) s middle empty
        costCellTitle t = container (s*2) s topLeft <| leftAligned <| bold <| Text.height (s/3) <| toText t
        costCell c = container (s*2) s bottomRight . centered . bold . Text.height s . toText <| show c
        --memoryControllCell i = container (s*2) s middle <| memCellText (show i)
        --memCellText = centered . Text.height s . toText
        --memCellTitle = container (s*2) s midBottom <| leftAligned <| Text.height (s/3) <| toText "memory:"
        incMemoryButton =
            let filler = container (s `div` 3) (s `div` 3) middle . link "#" . centered . Text.height (s/2) . bold <| toText "+"
                button = Input.customButton commands.handle IncMemory filler filler filler
            in container (s*2 + (s `div` 2)) s topRight button
        decMemoryButton =
            let filler = container (s `div` 3) (s `div` 3) middle . link "#" . centered . Text.height (s/2) . bold <| toText "-"
                button = Input.customButton commands.handle DecMemory filler filler filler
            in container (s*2 + (s `div` 2)) s midRight button

        controlsWidth = 3*s
        controlsHeight = (noStations+2) * s
        controls = 
            container controlsWidth controlsHeight midTop
            <| color black <| container (controlsWidth) (2 * s) middle
            <| color white <| container (controlsWidth - 4) (2 * s - 4) midTop
            <| flow down <|
                [ flow outward [costCell cost, costCellTitle "cost:"]
                , flow outward [costCell mem, costCellTitle "mem:", incMemoryButton, decMemoryButton]
                ] --++ map memoryControllCell (snd cache)

        cache = last <| Simulator.cacheState mem sched
        cost = sum <| Simulator.cacheMisses mem sched

        matrix = flow down [titleRow, flow outward [buttonMatrix, path], titleRow]

    in flow right [titleCol, matrix, titleCol, spacer 20 20, controls]



