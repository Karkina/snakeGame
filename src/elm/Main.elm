module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode
import Random
--import Html.Attributes exposing (.., type_)
import Bootstrap.Grid as Grid
import Bootstrap.CDN as CDN
import Bootstrap.Button as Button
import Bootstrap.Badge as Badge
import Bootstrap.ButtonGroup as ButtonGroup
import Browser.Navigation


{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }

type Direction = Up
  | Down
  | Left
  | Right

type Fruit = Pomme
  | Cerise  
type alias Snake = { positions : List Int,
                    direction : Direction
                  }

type alias Apple = { positions : Int,
                    isEat : Bool
                  }

type alias Walls = { positions : List Int,
                    isActivate : Bool
                  }


type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , sizeBoard : Int
  , player : Snake
  , apple : Apple
  , cherry : Apple
  , score : Int
  , torique : Bool
  , gameOver : Bool
  , walls : Walls
  , step : Float
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 40 (Snake [53,54,55] Right) (Apple 350 False) (Apple 500 False) 0 True False (Walls  [200,300,125,126,129,127,130,500,425,960,582,145,131,132] False) 0
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space

type Msg = NextFrame Posix
  | ToggleGameLoop
  | KeyMovement Direction
  | KeyDown Key
  | NewFruit Int
  | Roll 
  | ToriqueActivation
  | SizeBoard Int
  | RandomWall
  | ReloadBtnClicked


toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  not gameStarted
  |> Setters.setGameStartedIn model
  |> Update.none


directionSnake : Direction -> Model -> (Model,Cmd Msg)
directionSnake directionClick model =
  case (directionClick,model.player.direction) of
    (Down,_) -> if model.player.direction /= Up then let snakeUpdate ={positions=model.player.positions,direction=Down} in
             {model | player = snakeUpdate}
             |> Update.none
              else Update.none model
    (Left,_) -> if model.player.direction /= Right then let snakeUpdate ={positions=model.player.positions,direction=Left} in
             {model | player = snakeUpdate}
             |> Update.none
              else Update.none model
    (Right,_) -> if model.player.direction /= Left then let snakeUpdate ={positions=model.player.positions,direction=Right} in
             {model | player = snakeUpdate}
             |> Update.none
              else Update.none model
    (Up,_) -> if model.player.direction /= Down then let snakeUpdate ={positions=model.player.positions,direction=Up} in
             {model | player = snakeUpdate}
             |> Update.none
              else Update.none model

colisionApple : Model -> Model
colisionApple model =
  case model.player.positions of
      []-> model
      head::tail -> if head == model.apple.positions then addTail {model | score = model.score+100,apple = {positions = model.apple.positions,isEat=True}}
                    else if head == model.cherry.positions then {model | score = model.score+10,cherry = {positions = model.cherry.positions,isEat=True}}
                    else if List.member head model.walls.positions then {model | player= Snake [] Right, gameOver = True,score=0}
                    else if List.member head tail then {model | player= Snake [] Right, gameOver = True,score=0}
                    else {model | step = model.step+(1/4)}
movingSnake : Model -> Model
movingSnake model =

  movingSnakeHelp model (List.reverse(model.player.positions))


movingSnakeHelp : Model -> List Int -> Model
movingSnakeHelp model snakePositions=
  case (model.player.direction,snakePositions) of
    (Down,head::tail) -> if head//model.sizeBoard == model.sizeBoard-1 then toriqueSnake model
                         else  updateSnakeModel model.sizeBoard model snakePositions
    (Up,head::tail) -> if head//model.sizeBoard == 0 then toriqueSnake model
                         else  updateSnakeModel (-model.sizeBoard) model snakePositions

    (Left,head::tail) -> if ((modBy model.sizeBoard head) == 1) then toriqueSnake model
                         else  updateSnakeModel  -1 model snakePositions
    (Right,head::tail) ->  if ((modBy model.sizeBoard head) == 0) then toriqueSnake model
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



toriqueSnake : Model -> Model
toriqueSnake model =
  if model.torique then toriqueSnakeHelp model model.player 
  else {model | player= Snake [] Right, gameOver = True,score=0}

gameOver : Html Msg
gameOver = 
  Html.div [ ] []

toriqueSnakeHelp : Model -> Snake -> Model
toriqueSnakeHelp model snake =
  case (List.reverse snake.positions,snake.direction) of 
  ([],_)-> model
  (head::tail,Up) ->
     let newPos = Debug.log "torique " ((model.sizeBoard*model.sizeBoard)-(model.sizeBoard-head))in 
     let updateSnake = {positions=List.reverse (newPos::tail),direction=Up} in
     {model | player = updateSnake }
  (head::tail,Down) ->
     let newPos = Debug.log "torique " (modBy model.sizeBoard head-1) in 
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
  {model | score = model.score}

addTail : Model -> Model
addTail model =
  addTailHelp model (List.reverse(model.player.positions)) 

addTailHelp : Model -> List Int -> Model
addTailHelp model snakePositions =
  case snakePositions of
        []-> model
        head :: tail -> let updateSnake = Debug.log "Snake :" {positions=List.reverse((head-1)::snakePositions),direction=model.player.direction} in
                      {model | player = updateSnake }


updateApple : Model ->(Model,Cmd Msg)
updateApple model =
   update Roll model


nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= 250 then
    -- update Roll model
    --addTail model
    movingSnake model 
    |> colisionApple
    |> updateScore
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> updateApple
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
    Roll ->
      ( model
      , Random.generate NewFruit (Random.int 0 (model.sizeBoard*model.sizeBoard)) 
      )
    NewFruit newPosApple ->
      if model.apple.isEat then ({model |apple ={positions=newPosApple,isEat =False}}, Cmd.none)
      else if model.cherry.isEat || model.step >= 10 then ({model | cherry ={positions=newPosApple,isEat =False}, step=0}, Cmd.none)
      else (model,Cmd.none)
    ToriqueActivation ->
      ( {model | torique = not model.torique}
      , Cmd.none
      )
    SizeBoard taille ->
      ( {model | sizeBoard = taille}
      , Cmd.none
      )
    RandomWall ->
     ( if model.walls.isActivate then {model | walls = {positions = [200,300,125,126,129,127,130,500,425,960,582,145,131,132],isActivate = not model.walls.isActivate}}
       else {model | walls = {positions = [],isActivate = not model.walls.isActivate}}
      , Cmd.none
      )
    ReloadBtnClicked -> (model, Browser.Navigation.reload)

{-| Manage all your view functions here. -}
cell : Int -> Html msg
cell active =
  let class = if active == 1 then "cellSnake" else if active==2 then "cellPomme" else if active==3 then "cellChery" else if active == 4 then "cellWall" else "cell" in
  Html.div [ Attributes.class class ] []

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
     let cellCherry = cell 3 in

     let cellWall = cell 4 in
     let noCellColor = cell 0  in

     if List.member count snake then showPlateauHelp model snake (count-1) (cellColor::acc)
     else if count == model.cherry.positions then showPlateauHelp model snake (count-1) (cellCherry::acc)
     else if count==model.apple.positions then showPlateauHelp model snake (count-1) (cellPomme::acc)
     else if List.member count model.walls.positions then showPlateauHelp model snake (count-1) (cellWall::acc)
        else showPlateauHelp model snake (count- 1) (noCellColor::acc)

boardInit : Model -> Html Msg
boardInit model =
  Html.div[Attributes.class "grid"
  , Attributes.style "grid-template-rows" (String.join "" ["repeat(", String.fromInt  model.sizeBoard, ",1fr)"])
  , Attributes.style "grid-template-columns" (String.join "" ["repeat(",String.fromInt model.sizeBoard, ",1fr)"])
  ]
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
explanations ({ gameStarted, torique } as model) =
  let word = if gameStarted then "Stop" else "Start" in
  Html.div [ Attributes.class "separator" ]
    [
      Button.checkboxButton torique [ Button.primary, Button.onClick ToriqueActivation ] [ Html.text "Enable Toric World" ]
      ,Button.checkboxButton (not model.walls.isActivate) [ Button.primary, Button.onClick RandomWall ] [ Html.text " Enable Obstacles" ]
      , Button.button [ Button.primary, Button.onClick ToggleGameLoop] [ Html.text (String.join " " [word, "game"])]
      , Html.h4 [] [Html.text "Grid size : "]
      ,ButtonGroup.buttonGroup
      [ ButtonGroup.large ]
      [ ButtonGroup.button [ Button.outlineDark, Button.onClick (SizeBoard 20) ] [ Html.text "20" ]
      , ButtonGroup.button [ Button.secondary, Button.onClick (SizeBoard 40) ] [ Html.text "40" ]
      , ButtonGroup.button [ Button.dark, Button.onClick (SizeBoard 60) ] [ Html.text "60" ]
      ]
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Grid.containerFluid []
      [ CDN.stylesheet
        , Grid.row [ ]
        [ Grid.col [ ]
            [ boardInit model ]
        , Grid.col [ ]
            [ Html.div [Attributes.class "card" ] [
                Html.img [ Attributes.src "/snakepixel.png" ] []
                , Html.h1 [] [ Html.text "Snake, the retro game" ]
                , Html.hr [][]
                , explanations model
                , Html.hr [][]
                , Badge.pillInfo [] [Html.h1 [] [Html.text (String.join " " ["Score : ",(String.fromInt model.score)])]]
                , Html.hr [Attributes.hidden (not model.gameOver)][]
                , Badge.badgeDanger [ Attributes.hidden (not model.gameOver) ] [Html.h1 [] [Html.text "GAME OVER"]]
                , Html.div [Attributes.hidden (not model.gameOver)] [Html.br [][], Button.button [ Button.small, Button.warning, Button.onClick ReloadBtnClicked] [ Html.text "Reset"]]
            ]
          ]
        ]
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
