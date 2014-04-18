module Reorderable where

import Window


main = 
    let testForm = filled black <| rect 50 50
    in plotList <~ Window.dimensions ~ constant [testForm, testForm, testForm, testForm]


plotList : (Int, Int) -> [Form] -> Element
plotList (w,h) forms =
    let movedForms = zipWith moveForm [0..noForms] forms
        moveForm i = moveX (toFloat i * formDist - centerOffset)
        formDist = toFloat w / toFloat noForms
        centerOffset = (toFloat w - formDist) / 2
        noForms = length forms
    in collage w h movedForms

