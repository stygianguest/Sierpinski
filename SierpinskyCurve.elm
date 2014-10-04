module SierpinskyCurve where

import Util(modCount)

main = 
    let s = 300
        plot n = collage s s
            [ scale (s/2) <| sierpinskyTriangle n
            , scale (s/2) <| sierpinskyCurveForm n
            , scale 4 <| move (s/4,s/4) <| toForm <| plainText (show n)
            ]
    in plot <~ modCount 7 (every (2*second))
    --in plot 3 -- `above` asText (sierpinskyCurve 2)

sierpinskyTriangle : Int -> Form
sierpinskyTriangle n =
 if | n <= 0    -> alpha 0.7 <| filled blue <| polygon [(-1,-1), (1,-1), (-1,1)]
    | otherwise -> 
        let rec = sierpinskyTriangle (n-1)
        in group 
            [ scale 0.7 <| moveX -1 <| rotate (degrees 135) <| rec
            , scale 0.7 <| moveY -1 <| rotate (degrees -135) <| rec
            ]


sierpinskyCurveForm =
    let pathStyle = { defaultLine | width <- 0.05, join <- Smooth, cap <- Round }
    in traced pathStyle << path << sierpinskyCurve
    --in group << map (\p -> move p <|filled black <| circle 0.03) << sierpinskyCurve

sierpinskyCurve : Int -> [(Float, Float)]
sierpinskyCurve n =
 if | n <= 0    -> [(-0.4,-0.4)]
    | otherwise -> 
        let rec = sierpinskyCurve (n-1) 
            westTrans  = addVec (-1, 0) << rotateVec (degrees 135) << mulVec 0.7
            southTrans = addVec (0, -1) << rotateVec (degrees -135) << mulVec 0.7
        --in map westTrans rec ++ map southTrans rec
        in reverse (map southTrans rec) ++ reverse (map westTrans rec)

mulVec : Float -> (Float, Float) -> (Float, Float)
mulVec x (a,b) = (a*x, b*x)

addVec : (Float, Float) -> (Float, Float) -> (Float, Float)
addVec (a,b) (c,d) = (a+c, b+d)

rotateVec : Float -> (Float, Float) -> (Float, Float)
rotateVec a (x,y) = 
    let cosa = cos a
        sina = sin a
    in (x * cosa - y * sina, x * sina + y * cosa)
