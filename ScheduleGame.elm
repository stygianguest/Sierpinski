module ScheduleGame where

import Char
import String
import Graphics.Input as Input
import Window

import Schedule
import Simulator
import Util(repeatN, enumerate)

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
    IncMemory -> { state | memorySize <- state.memorySize + 1 }
    DecMemory -> { state | memorySize <- state.memorySize - 1 }
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

        bCircle a = collage s s [filled (rgba 1 1 1 a) (circle 10)]
        jobButton i j =
            let cmd = ScheduleJob i j
            in Input.customButton commands.handle cmd (bCircle 0.2) (bCircle 0.5) (bCircle 0.8)

        mkRow i = flow right <| map (jobButton i) [1..i]
        buttonMatrix = flow down <| map mkRow [1..noStations]
        
        path = Schedule.drawSchedule buttonSize noStations sched

        emptyControllCell = container (s*2) s middle empty
        costCellTitle = container (s*2) s topLeft <| leftAligned <| bold <| Text.height (s/3) <| toText "cost:"
        costCell c = container (s*2) s bottomRight . centered . bold . Text.height s . toText <| show c
        memoryControllCell i = container (s*2) s middle <| memCellText (show i)
        memCellText = centered . Text.height s . toText
        memCellTitle = container (s*2) s midBottom <| leftAligned <| Text.height (s/3) <| toText "memory:"

        controlsWidth = 3*s
        controlsHeight = (noStations+2) * s
        controls = 
            container controlsWidth controlsHeight middle 
            <| color black <| container (controlsWidth) (noStations * s) middle
            <| color white <| container (controlsWidth - 4) (noStations * s - 4) midTop
            <| flow down <|
                [ flow outward [costCell cost, costCellTitle]
                , memCellTitle
                ] ++ map memoryControllCell cache

        cache = snd . last <| Simulator.cacheState mem sched
        cost = sum <| Simulator.cacheMisses mem sched

        matrix = flow down [titleRow, flow outward [path, buttonMatrix], titleRow]

    in flow right [titleCol, matrix, titleCol, spacer 20 20, controls]



