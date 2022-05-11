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
type alias Cellule = { x : Int
                      ,y : Int
                      , coloredSquare : Int
                  }

type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , cellsBoard : List (Cellule)
  , sizeBoard : Int
  , player : Snake
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time [] 40 (Snake [251,252,253] Up)
  |> gameCase 
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
    (Up,_) -> let snakeUpdate ={positions=model.player.positions,direction=Up} in
             {model | player = snakeUpdate}
             |> Update.none
    (_,_) -> Update.none model


movingSnake : Model -> Model
movingSnake model =

  movingSnakeHelp model (List.reverse(model.player.positions))

movingSnakeHelp : Model -> List Int -> Model
movingSnakeHelp model snakePositions=
  case (model.player.direction,snakePositions) of
    (Up,head::tail) -> let snake = (head+40)::List.take (List.length snakePositions-1) snakePositions in
                       let updateSnake = {positions = snake ,direction=Up} in
                       let debugTest = Debug.log "Taille Serpent"  snake in
                       { model | player = updateSnake}
                       
    (_,head::tail) -> let updateSnake = {positions = (head+40)::tail,direction=Up} in
                      { model | player = updateSnake}
    (_,_) -> model


updatePlateau : Model -> Model
updatePlateau model =
  updatePlateauHelp model (List.reverse model.cellsBoard) model.player.positions (model.sizeBoard*model.sizeBoard) []

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

  



keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
  case Debug.log "key" key of
    Space -> update ToggleGameLoop model
    ArrowUp -> update (KeyMovement Up) model
    ArrowRight -> update (KeyMovement Right) model
    ArrowDown -> update (KeyMovement Down) model
    ArrowLeft -> update (KeyMovement Left) model


   -- _ -> Update.none model


nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= 1000 then
     updatePlateau model
    |> movingSnake 
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
cell : Int -> Int -> Html msg
cell index active =
  let class = if active == index then "cell active" else "cell" in
  Html.div [ Attributes.class class ] []

gameCase : Model -> Model
gameCase model =
   gameCaseHelper 0 model

gameCaseHelper : Int -> Model -> Model
gameCaseHelper taille model = 
 let cellGame = {x= modBy 40 taille,y= taille//40,coloredSquare=0} in
 if taille < model.sizeBoard*model.sizeBoard then gameCaseHelper (taille+1) {model | cellsBoard = cellGame::model.cellsBoard}
  else model
 

showPlateau : Model -> List (Html Msg)
showPlateau model =
  showPlateauHelp model.cellsBoard [] 

showPlateauHelp : List Cellule -> List (Html Msg) -> List (Html Msg)  
showPlateauHelp cellsBoard acc =
  case cellsBoard of
  [] -> acc
  (head::tail) ->
     let cellColor = cell 1 1 in
     let noCellColor = cell 0 1 in
     if head.coloredSquare == 1 then showPlateauHelp tail (cellColor::acc)
        else showPlateauHelp tail (noCellColor::acc)
  

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
