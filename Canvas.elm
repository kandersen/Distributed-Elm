--| Canvas
--| Kristoffer Just Andersen, Aarhus University
--| kja@cs.au.dk
--| October, 2013
--|
--| Designed and tested with Elm 0.9.0.2
--|
--| A collaborative vector drawing application, programmed in Elm.
--| Note it requires the node.js server found in 'CanvasServer.js'
--|
--| See the accompanying report, 'Collaborative User-Interfaces with
--| Functional Reactive Programming'.

import Mouse
import Window
import WebSocket
import Json
import Dict
import Either

-- UTILITIES

(>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
ma >>= f = case ma of {
  Just a -> f a;
  Nothing -> Nothing }

-- MARSHALLING

jsonOfPair : (Int, Int) -> Json.JsonValue
jsonOfPair (x, y) = Json.Object . Dict.fromList <|
  [("tag", Json.String "pair"),
   ("x", Json.Number (toFloat x)),
   ("y", Json.Number (toFloat y))]

jsonOfLine : Line -> Json.JsonValue
jsonOfLine l = Json.Object . Dict.fromList <|
  [("tag", Json.String "line"),
   ("from", jsonOfPair l.from),
   ("to", jsonOfPair l.to)]

stringOfLine : Line -> String
stringOfLine = Json.toString "" . jsonOfLine

jsonOfLineList : [Line] -> Json.JsonValue
jsonOfLineList ls = Json.Object . Dict.fromList <| case ls of {
  [] -> [("tag", Json.String "lines"),
         ("lines", Json.Object . Dict.fromList <| [])];
  (l::ls) -> [("tag", Json.String "lines"),
              ("line", jsonOfLine l),
              ("lines", jsonOfLineList ls)] } 
  
stringOfLineList : [Line] -> String
stringOfLineList = Json.toString "" . jsonOfLineList

extractDict : Json.JsonValue -> Maybe (Dict.Dict comparable Json.JsonValue)
extractDict jv = case jv of {
  Json.Object d -> Just d;
  _ -> Nothing }

extractNumber : Json.JsonValue -> Maybe Int
extractNumber s = case s of {
  Json.Number n -> Just (round n);
  _ -> Nothing }

extractString : Json.JsonValue -> Maybe String
extractString jv = case jv of {
  Json.String s -> Just s;
  _ -> Nothing }

extractTag : Json.JsonValue -> Maybe String
extractTag jv =
  (extractDict jv >>= Dict.lookup "tag") >>= extractString

isTagged : String -> (Json.JsonValue -> Bool)
isTagged tag = maybe False ((==) tag) . extractTag 

extractPair : Json.JsonValue -> Maybe (Int, Int)
extractPair jv =
  extractDict jv >>= (\d ->
  (Dict.lookup "x" d >>= extractNumber) >>= (\x ->
  (Dict.lookup "y" d >>= extractNumber) >>= (\y ->
  Just (x, y))))

extractLine : Json.JsonValue -> Maybe Line
extractLine jv =
  extractDict jv >>= (\d ->
  (Dict.lookup "from" d >>= extractPair) >>= (\from ->
  (Dict.lookup "to" d >>= extractPair) >>= (\to ->
  Just {from = from, to = to})))

extractLines : Json.JsonValue -> Maybe [Line]
extractLines jv = if isTagged "lines" jv
                  then extractLine jv >>= (\l ->
                       extractDict jv >>= (\d ->
                       (Dict.lookup "lines" d >>= extractLines) >>= (\ls ->
                       Just (l::ls))))
                  else Just []

-- MODEL

type Point = (Int, Int)

type Line = { from : Point, to : Point }

type Canvas = [Line]

type LineTool = { from : Maybe Point }

type AppState = { canvas : Canvas,
                  buffer : Canvas }

initState : AppState
initState = { canvas = [],
              buffer = [] }

-- INPUT

data ToolInput = Click Point
               | NoOpTool

toolInput : Signal ToolInput
toolInput = foldp (\p _ -> Click p) NoOpTool <|
                  (sampleOn Mouse.clicks Mouse.position)

data ServerMsg = Delta Line
               | Reset [Line]
               | NoOpServer

-- UPDATE

stepTool : ToolInput -> (Maybe Line, LineTool) -> (Maybe Line, LineTool)
stepTool i (_  ,t) = case i of {
  NoOpTool -> (Nothing, t);
  Click p -> case t.from of {
    Nothing -> (Nothing, {from = Just p});
    Just p' -> (Just {from = p', to = p}, {from = Nothing}) }}

toolSignal : Signal (Maybe Line, LineTool)
toolSignal = foldp stepTool (Nothing, {from = Nothing}) toolInput

linesDrawn : Signal [Line]
linesDrawn = dropRepeats (foldp (\ml s -> case ml of {
  Nothing -> s;
  Just l  -> [l]}) [] (fst <~ toolSignal))

outgoing : Signal String
outgoing = stringOfLineList <~ linesDrawn

parseMsg : String -> ServerMsg
parseMsg s = maybe NoOpServer id <| Json.fromString s >>= (\jv -> 
             if | isTagged "line" jv -> 
                    (extractLine jv >>= Just . Delta)
                | isTagged "lines" jv -> 
                    (extractLines jv >>= Just . Reset)
                | otherwise ->
                  Just NoOpServer)

incoming : Signal ServerMsg
incoming = parseMsg <~ WebSocket.connect "ws://127.0.0.1:8125" outgoing

appInput : Signal (Either.Either [Line] ServerMsg)
appInput = merge (Either.Left <~ linesDrawn) (Either.Right <~ incoming)

stepApp : (Either.Either [Line] ServerMsg) -> AppState -> AppState
stepApp inp state = case inp of {
  Either.Left ls -> {state | buffer <- ls ++ state.buffer };
  Either.Right msg -> case msg of {
    Delta l -> {state | canvas <- l :: state.canvas,
                        buffer <- filter ((/=) l) state.buffer};
    Reset ls -> {state | canvas <- ls};
    NoOpServer -> state}}

appState : Signal AppState
appState = foldp stepApp initState appInput

-- DRAW

collageOffset : (Int,Int) -> (Int, Int) -> (Float, Float)
collageOffset (w,h) (x,y) =
    (toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y)

renderLine : (Int, Int) -> LineStyle -> Line -> Form
renderLine dims ls l = traced ls (segment (collageOffset dims l.from) (collageOffset dims l.to))

renderCanvas : (Int, Int) -> LineStyle -> Canvas -> [Form]
renderCanvas dims ls = map (renderLine dims ls) 

drawApp : (Int, Int) -> AppState -> Element
drawApp ((w, h) as dims) app = collage w h <|
 renderCanvas dims defaultLine app.canvas ++ renderCanvas dims (dotted blue) app.buffer
  
drawLineTool : (Int, Int) -> (Int, Int) -> LineTool -> Element
drawLineTool ((w, h) as dims) mousepos t = collage w h <|
  case t.from of {
    Nothing -> [];
    Just p -> [traced defaultLine (segment (collageOffset dims p) (collageOffset dims mousepos))]}

draw : (Int, Int) -> (Int, Int) -> AppState -> LineTool -> Element
draw dims mpos s t = 
 layers [drawApp dims s, 
         drawLineTool dims mpos t]

-- MAIN

main : Signal Element
main = draw <~ Window.dimensions ~ Mouse.position ~ appState ~ (snd <~ toolSignal)