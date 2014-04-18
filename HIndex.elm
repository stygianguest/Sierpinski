module HIndex where

import Schedule(sierpinski, drawScheduleWithIndex)

main = 
    let noStations = 8
    in drawScheduleWithIndex 40 noStations (sierpinski noStations)
