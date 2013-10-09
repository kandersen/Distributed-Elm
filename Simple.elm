import Mouse
import Window

-- DRAW

collageOffset : (Int,Int)-> (Int, Int) -> (Float, Float)
collageOffset (w,h) (x,y) =
    (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)
    
draw : (Int, Int) -> (Int, Int) -> Element
draw ((w, h) as dims) mouse = collage w h
  [move (collageOffset dims mouse) (filled black (circle 10.0))]
  
-- MAIN

main : Signal Element
main = draw <~ Window.dimensions ~ (sampleOn Mouse.clicks Mouse.position)