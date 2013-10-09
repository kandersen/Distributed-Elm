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
  [("x", Json.Number (toFloat x)), ("y", Json.Number (toFloat y))]
  
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

eventOfJSONString : String -> Event
eventOfJSONString s =
  maybe NoOp Update (Json.fromString s >>= extractPair)

-- MODEL

data AppState = Cache  (Int, Int)
              | Server (Int, Int)

initState = Cache (0, 0)

-- INPUT

data Event = Click (Int, Int)
           | Update (Int, Int)
           | NoOp

clicks : Signal (Int, Int)
clicks = sampleOn Mouse.clicks Mouse.position

userEvents : Signal Event
userEvents = Click <~ clicks

outgoing : Signal String
outgoing = stringOfPair <~ clicks

serverEvents : Signal Event
serverEvents = eventOfJSONString <~ (WebSocket.connect "ws://127.0.0.1:8125" outgoing)

events : Signal Event
events = merge userEvents serverEvents

-- UPDATE

step : Event -> AppState -> AppState
step inp s = case inp of {
  Click p -> Cache p;
  Update p -> Server p;
  NoOp -> s }

appState : Signal AppState
appState = foldp step initState events

-- DRAW

collageOffset : (Int,Int) -> (Int, Int) -> (Float, Float)
collageOffset (w,h) (x,y) =
    (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

draw : AppState -> (Int, Int) -> Element
draw state ((w, h) as dims) = collage w h <| case state of {
    Cache pos ->
      [move (collageOffset dims pos) (filled blue (circle 10.0))]; 
    Server pos ->
      [move (collageOffset dims pos) (filled black (circle 10.0))] }

-- MAIN

main : Signal Element
main = draw <~ appState ~ Window.dimensions