--| SimpleDist
--| Kristoffer Just Andersen, Aarhus University
--| kja@cs.au.dk
--| October, 2013
--|
--| Designed and tested with Elm 0.9.0.2
--|
--| A simple application for visualizing the last position the
--| mouse was clicked, and 'collaborating' on this with other
--| clients running the same code. Requires the node.js server
--| found in 'SimpleServer.js'.
--|
--| See the accompanying report, 'Collaborative User-Interfaces with
--| Functional Reactive Programming'.

import Mouse
import Window
import WebSocket
import Json
import Dict

-- UTILITIES

(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
ma >>= f = case ma of {
  Just a -> f a;
  Nothing -> Nothing }

-- MARSHALLING
    
stringOfPair : (Int, Int) -> String
stringOfPair (x, y) = (Json.toString "" . Json.Object . Dict.fromList) <|
  [("fst", Json.Number (toFloat x)), ("snd", Json.Number (toFloat y))]
  
extractNumber : Json.JsonValue -> Maybe Int
extractNumber s = case s of {
  Json.Number n -> Just (round n);
  _ -> Nothing }

extractPair : Json.JsonValue -> Maybe (Int, Int)
extractPair jv = case jv of {
  (Json.Object d) ->
    (Dict.lookup "x" d >>= extractNumber) >>= (\x ->
    (Dict.lookup "y" d >>= extractNumber) >>= (\y ->
    Just (x, y)));
    _ -> Nothing }
                       
pairOfJSONString : String -> (Int, Int)
pairOfJSONString s =
  maybe (0,0) extractPair (Json.fromString s >>= extractPair)

-- INPUT

outgoing : Signal String
outgoing = stringOfPair <~ sampleOn Mouse.clicks Mouse.position

incoming : Signal String
incoming = WebSocket.connect "ws://127.0.0.1:8125" outgoing

clicks : Signal (Int, Int)
clicks = pairOfJSONString <~ incoming

-- DRAW

collageOffset : (Int,Int) -> (Int, Int) -> (Float, Float)
collageOffset (w,h) (x,y) =
    (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

draw : (Int, Int) -> (Int, Int) -> Element
draw ((w, h) as dims) mouse = collage w h
  [move (collageOffset dims mouse) (filled black (circle 10.0))]

-- MAIN

main : Signal Element
main = draw <~ Window.dimensions ~ clicks