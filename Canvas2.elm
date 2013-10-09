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

--| CHANGE! Added BoxTool
data Tool = LineTool { from : Maybe Point }
          | BoxTool { firstCorner : Maybe Point }

type AppState = { canvas : Canvas,
                  buffer : Canvas }

initState : AppState
initState = { canvas = [],
              buffer = [] }

-- INPUT

data ToolInput = Click Point
               | NoOpTool
               | Switch

toolInput : Signal ToolInput
toolInput = foldp (\p _ -> Click p) NoOpTool <|
                  (sampleOn Mouse.clicks Mouse.position)

data ServerMsg = Delta Line
               | Reset [Line]
               | NoOpServer

-- UPDATE

box : Point -> Point -> [Line]
box (a, b) (c, d) = 
  [{from = (a, b), to = (a, d)},
   {from = (a, b), to = (c, b)},
   {from = (c, b), to = (c, d)},
   {from = (a, d), to = (c, d)}]

--| CHANGE! Type sig, multiple lines
stepTool : ToolInput -> ([Line], Tool) -> ([Line], Tool)
stepTool i (_  ,t) = case i of {
  NoOpTool -> ([], t);
  Click p -> case t of {
    LineTool { from } -> case from of {
       Nothing -> ([], LineTool {from = Just p});
       Just p' -> ([{from = p', to = p}], LineTool {from = Nothing}) };
    BoxTool { firstCorner } -> case firstCorner of {
       Nothing -> ([], BoxTool {firstCorner = Just p});
       Just p' -> (box p p', BoxTool {firstCorner = Nothing})}}}

--| CHANGE Types adjusted!
toolSignal : Signal ([Line], Tool)
toolSignal = foldp stepTool ([], BoxTool {firstCorner = Nothing}) toolInput

take1 : a -> b -> a
take1 a b = a

--| CHANGE Adjusted due to types
linesDrawn : Signal [Line]
linesDrawn = dropRepeats (foldp take1 [] (fst <~ toolSignal))

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
  
--| CHANGE Adjusted Types ONLY since now not the only tool
drawLineTool : (Int, Int) -> (Int, Int) -> { from : Maybe Point }  -> Element
drawLineTool ((w, h) as dims) mousepos t = collage w h <|
  case t.from of {
    Nothing -> [];
    Just p -> [traced defaultLine (segment (collageOffset dims p) (collageOffset dims mousepos))]}

--| CHANGE Introduced to visualize the boxtool
drawBoxTool : (Int, Int) -> (Int, Int) -> { firstCorner : Maybe Point } -> Element
drawBoxTool ((w, h) as dims) mousepos t = collage w h <|
  case t.firstCorner of {
    Nothing -> [];
    Just p -> map (renderLine dims defaultLine) (box p mousepos) }

--| CHANGE Introduced, to discern different tools
drawTool : (Int, Int) -> (Int, Int) -> Tool -> Element
drawTool dims mpos t = case t of {
  BoxTool r -> drawBoxTool dims mpos r;
  LineTool r -> drawLineTool dims mpos r }


--| CHANGE Changed to use new dispatch
draw : (Int, Int) -> (Int, Int) -> AppState -> Tool -> Element
draw dims mpos s t = 
 layers [drawApp dims s, 
         drawTool dims mpos t]

-- MAIN

main : Signal Element
main = draw <~ Window.dimensions ~ Mouse.position ~ appState ~ (snd <~ toolSignal)