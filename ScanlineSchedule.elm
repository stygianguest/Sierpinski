module ScanlineSchedule where

import Schedule(scanline, drawScheduleWithIndex)


main = 
    let noStations = 8
        w = 40
    in drawScheduleWithIndex w noStations (scanline noStations)
