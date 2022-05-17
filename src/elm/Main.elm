module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }

type Direction = Up
  | Down
  | Left
  | Right 
type alias Snake = { positions : List Int,
                    direction : Direction
                  }


type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , sizeBoard : Int
  , player : Snake
  , apple : Int
  , score : Int
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 40 (Snake [40,41,42] Down) 350 10
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyMovement Direction
  | KeyDown Key

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}
{-
updateSquare : Model -> Model
updateSquare ({ coloredSquare} as model) =
  coloredSquare + 1
  |> modBy 2
  |> Setters.setColoredSquareIn model
-}
toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  not gameStarted
  |> Setters.setGameStartedIn model
  |> Update.none


directionSnake : Direction -> Model -> (Model,Cmd Msg)
directionSnake directionClick model =
  case (directionClick,model.player.direction) of
    (Down,_) -> let snakeUpdate ={positions=model.player.positions,direction=Down} in
             {model | player = snakeUpdate}
             |> Update.none
    (Left,_) -> let snakeUpdate ={positions=model.player.positions,direction=Left} in
             {model | player = snakeUpdate}
             |> Update.none
    (Right,_) -> let snakeUpdate ={positions=model.player.positions,direction=Right} in
             {model | player = snakeUpdate}
             |> Update.none
    (Up,_) -> let snakeUpdate ={positions=model.player.positions,direction=Up} in
             {model | player = snakeUpdate}
             |> Update.none


movingSnake : Model -> Model
movingSnake model =

  movingSnakeHelp model (List.reverse(model.player.positions))


movingSnakeHelp : Model -> List Int -> Model
movingSnakeHelp model snakePositions=
  case (model.player.direction,snakePositions) of
    (Down,head::tail) -> if head//model.sizeBoard == model.sizeBoard-1 then thoriqueSnake model
                         else  updateSnakeModel 40 model snakePositions
    (Up,head::tail) -> if head//model.sizeBoard == 0 then thoriqueSnake model
                         else  updateSnakeModel -40 model snakePositions

    (Left,head::tail) -> if ((modBy model.sizeBoard head) == 1) then thoriqueSnake model
                         else  updateSnakeModel  -1 model snakePositions
    (Right,head::tail) ->  if ((modBy model.sizeBoard head) == 0) then thoriqueSnake model
                           else  updateSnakeModel 1  model snakePositions
            
    (_,[]) -> model

updateSnakeModel :Int -> Model ->List Int -> Model
updateSnakeModel addSpeed model snakePositions =
  case snakePositions of
  [] -> model
  head::tail -> let snake = (head+addSpeed)::List.take (List.length snakePositions-1) snakePositions in
    let newSnake = {positions = List.reverse snake ,direction=model.player.direction} in
    let debugTest = Debug.log "Taille Serpent"  model.player in
    { model | player = newSnake}



thoriqueSnake : Model -> Model
thoriqueSnake model =
  thoriqueSnakeHelp model model.player

thoriqueSnakeHelp : Model -> Snake -> Model
thoriqueSnakeHelp model snake =
  case (List.reverse snake.positions,snake.direction) of 
  ([],_)-> model
  (head::tail,Up) ->
     let newPos = Debug.log "Thorique " ((model.sizeBoard*model.sizeBoard)-(model.sizeBoard-head))in 
     let updateSnake = {positions=List.reverse (newPos::tail),direction=Up} in
     {model | player = updateSnake }
  (head::tail,Down) ->
     let newPos = Debug.log "Thorique " (modBy model.sizeBoard head+1) in 
     let updateSnake = {positions=List.reverse (newPos::tail),direction=Down} in
     {model | player = updateSnake }
      
  (head::tail,Right) ->
     let newPos = head-(model.sizeBoard-1) in 
     let updateSnake = {positions=List.reverse (newPos::tail),direction=Right} in
     {model | player = updateSnake }
  (head::tail,Left) ->
     let newPos = head+(model.sizeBoard-1) in 
     let updateSnake = {positions=List.reverse (newPos::tail),direction=Left} in
     {model | player = updateSnake }



indexToPos : Model -> Int -> (Int,Int)
indexToPos model position =
  (modBy model.sizeBoard position,position//model.sizeBoard)


{-
updatePlateau : Model -> Model
updatePlateau model =
  updatePlateauHelp model (List.reverse model.cellsBoard) model.player.positions (model.sizeBoard*model.sizeBoard) []
-}
{-
updatePlateauHelp : Model -> List Cellule -> List Int -> Int ->List Cellule -> Model
updatePlateauHelp model cellsBoard player count acc=
  case (cellsBoard,player,count) of
  (_,_,0) -> {model| cellsBoard =  acc}
  ([],_,_) -> {model| cellsBoard = acc}
  (head::tail,[],_) -> 
                    let newCell = {x = head.x,y = head.y,coloredSquare=0} in
                    updatePlateauHelp model tail [] count (newCell::acc)
  (head::tail,headSnake::tailSnake,_) ->
     let index = head.x + head.y*40 in
     let darkCell = {x = head.x,y = head.y,coloredSquare=1} in
     let lightCell = {x = head.x,y = head.y,coloredSquare=0} in
     if headSnake == index then 
        let hello = Debug.log "index :" index in
        let snakeTest = Debug.log "snake:" headSnake in 
        updatePlateauHelp model tail tailSnake (count-1) (darkCell::acc)
     else updatePlateauHelp model tail player (count-1) (lightCell::acc)
-}
  



keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
  case Debug.log "key" key of
    Space -> update ToggleGameLoop model
    ArrowUp -> update (KeyMovement Up) model
    ArrowRight -> update (KeyMovement Right) model
    ArrowDown -> update (KeyMovement Down) model
    ArrowLeft -> update (KeyMovement Left) model


   -- _ -> Update.none model
updateScore : Model -> Model
updateScore model = 
  {model | score = model.score + 5}



nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= 500 then
    movingSnake model
    |> updateScore
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> Update.none
  else
    time_
    |> Setters.setTimeIn model
    |> Update.none

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    KeyMovement direction -> directionSnake direction model
    NextFrame time -> nextFrame time model

{-| Manage all your view functions here. -}
cell : Int -> Html msg
cell active =
  let class = if active == 1 then "cellSnake" else if active==2 then "cellPomme" else "cell" in
  Html.div [ Attributes.class class ] []
{-
gameCase : Model -> Model
gameCase model =
   gameCaseHelper 0 model

gameCaseHelper : Int -> Model -> Model
gameCaseHelper taille model = 
 let cellGame = {x= modBy 40 taille,y= taille//40,coloredSquare=0} in
 if taille < model.sizeBoard*model.sizeBoard then gameCaseHelper (taille+1) {model | cellsBoard = cellGame::model.cellsBoard}
  else model
 
-}
showPlateau : Model -> List (Html Msg)
showPlateau model =
  showPlateauHelp model (List.reverse(model.player.positions)) (model.sizeBoard*model.sizeBoard) []

showPlateauHelp : Model -> List Int -> Int -> List (Html Msg) -> List (Html Msg)  
showPlateauHelp model snake count acc=
  case (count,snake) of
  (0,_) -> acc
  (_,_) ->
     let cellColor = cell 1 in
     let cellPomme = cell 2 in
     let noCellColor = cell 0  in
     if List.member count snake then showPlateauHelp model snake (count-1) (cellColor::acc)
     else if count==model.apple then showPlateauHelp model snake (count-1) (cellPomme::acc)
        else showPlateauHelp model snake (count- 1) (noCellColor::acc)

boardInit : Model -> Html Msg
boardInit model =
  Html.div[Attributes.class "grid"]
    (showPlateau model)

actualTime : Model -> Html Msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time"
    , time
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

explanations : Model -> Html Msg
explanations ({ gameStarted } as model) =
  let word = if gameStarted then "Stop" else "Start" in
  Html.div [ Attributes.class "separator" ]
    [ Html.h1 []
      [ Html.text "Welcome to the snake project!" ]
    , actualTime model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ Html.img [ Attributes.src "/logo.svg" ] []
    , Html.text (String.fromInt model.score)
    , explanations model
    , boardInit model
    ]

{-| Parts for the runtime. Get key presses and subscribe to
 -|   requestAnimationFrame for the game loop. You don't have to bother with
 -|   this. -}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed ArrowUp
    "ArrowLeft" -> Decode.succeed ArrowLeft
    "ArrowRight" -> Decode.succeed ArrowRight
    "ArrowDown" -> Decode.succeed ArrowDown
    " " -> Decode.succeed Space
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }
