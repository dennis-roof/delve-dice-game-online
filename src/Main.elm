module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import List.Extra


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- ALIAS


type alias Face = Int


type alias Rolls = List Face


type alias Level = Int


type alias Character =
  { name : String
  , defaultHp : Int
  , hp : Int
  }


type alias Amount =
  { min : Int
  , max: Int
  , increments : Int
  }

type alias Enemy =
  { name : String
  , amount : Amount
  , defaultHp : Int
  , hps : List Int
  , rolls: Rolls
  , hitFaces : Rolls
  , hits : Rolls
  }

type alias Attacks =
  { charges : Int
  , sneakAttackIndex : Int
  }


-- MODEL


type alias Model =
  { level: Level
  , enemy: Enemy
  , characters : List Character
  , rolls : Rolls
  , attacks : Attacks
  , debug1: List Int
  , debug2: List Int
  }


initCharacters : List Character
initCharacters =
  [ { name = "Fighter", defaultHp = 6, hp = 6 }
  , { name = "Rogue", defaultHp = 3, hp = 3 }
  , { name = "Cleric", defaultHp = 3, hp = 3 }
  , { name = "Wizard", defaultHp = 1, hp = 1 }
  ]


initAttacks : Attacks
initAttacks = { charges = 2, sneakAttackIndex = -1 }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model
    0
    ( getLevelEnemy 0 )
    initCharacters
    []
    initAttacks
    []
    []
  , Cmd.none
  )


-- HELPER FUNCTIONS


classes : String -> Attribute msg
classes classNames = 
  classList
  ( List.map 
    ( \className -> ( className, True ) ) 
    ( String.split " " classNames )
  )


toInt : String -> Int
toInt string =
  case String.toInt string of
    Nothing -> 0
    Just number -> number


join : List Int -> String
join list = String.join "" ( List.map String.fromInt list )


getUniqueList : List Int -> List Int
getUniqueList numbers =
  List.foldl 
  ( \number result -> if getMaximumNumber result /= number then List.append result [ number ] else result ) 
  [] 
  ( List.sort numbers )


removeNumbers : List Int -> List Int -> List Int
removeNumbers sequence list =
  case ( sequence, list ) of
    ( _, [] ) -> []
    ( [], listNumbers ) -> listNumbers
    ( ( sequenceNumber :: sequenceNumbers ), listNumbers ) ->
      removeNumbers sequenceNumbers (removeNumber sequenceNumber listNumbers)


removeNumber : Int -> List Int -> List Int
removeNumber number list =
  case ( number, list ) of
    ( _, [] ) -> []
    ( matchNumber, ( listNumber :: listNumbers ) ) ->
      if matchNumber == listNumber
      then listNumbers
      else listNumber :: removeNumber number listNumbers


--trimList : List Int -> Int -> List Int
--trimList list limit =
--  case ( list, limit ) of
--    ( [], _ ) -> []
--    ( newList, 0 ) -> []
--    ( ( head :: oldList ), oldLimit ) -> head :: trimList oldList ( oldLimit - 1 )


attackEnemy : ( Int -> Int -> Int ) -> List Int -> List Int
attackEnemy reduceHps hps = 
  List.filter 
  ( \hp -> hp > 0 )
  ( List.indexedMap
    reduceHps 
    hps
  )


-- UPDATE


type Msg
  = InitNextLevel
  | GenerateEnemies Int Int
  | Roll
  | NextFaces (Rolls)
  | Charge Int
  | SneakAttack Int
  | CastRayOfFrost
  | CastChainLightning
  | CastFireball Int
  | CastDemise
  | CastMinorHeal String
  | CastHeal String Rolls
  | CastMiracle


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    InitNextLevel ->
      ( { model | level = model.level + 1, enemy = getLevelEnemy (model.level + 1), attacks = initAttacks }
      , let
          newEnemy = getLevelEnemy (model.level + 1)
        in
          Random.generate 
          ( \numberOfEnemies -> GenerateEnemies numberOfEnemies (model.level + 1) ) 
          ( Random.int newEnemy.amount.min ( if newEnemy.amount.increments > 0 then newEnemy.amount.min else newEnemy.amount.max ) )
      )

    GenerateEnemies numberOfEnemies level ->
      (
        let
          oldEnemy = model.enemy
          newEnemy = { oldEnemy | hps = List.map (\n -> model.enemy.defaultHp) (List.range 1 numberOfEnemies) }
        in
          { model | level = level, enemy = newEnemy }
      , Cmd.none
      )
    
    Roll ->
      ( model
      , Random.generate NextFaces (Random.list 6 (Random.int 1 6))
      )

    NextFaces faces ->
      ( { model | rolls = List.sort faces, attacks = initAttacks }
      , Cmd.none
      )

    Charge enemyIndex ->
      (
        if model.attacks.charges > 0
        then
          let
            oldEnemy = model.enemy
            newEnemy = 
              { oldEnemy 
              | hps = attackEnemy ( \index hp -> if index == enemyIndex then ( hp - 1 ) else hp ) oldEnemy.hps
              }
            oldAttacks = model.attacks
            newAttacks = { oldAttacks | charges = oldAttacks.charges - 1 }
          in
            { model | enemy = newEnemy, rolls = List.Extra.remove 6 model.rolls, attacks = newAttacks }
        else model
      , Cmd.none
      )

    SneakAttack enemyIndex ->
      (
        if model.attacks.sneakAttackIndex == -1 || model.attacks.sneakAttackIndex == enemyIndex
        then
          let
            oldEnemy = model.enemy
            newEnemy = 
              { oldEnemy 
              | hps = attackEnemy ( \index hp -> if index == enemyIndex then ( hp - 1 ) else hp ) oldEnemy.hps
              }
            oldAttacks = model.attacks
            newAttacks = { oldAttacks | sneakAttackIndex = enemyIndex }
          in
            { model | enemy = newEnemy, rolls = List.Extra.remove 1 model.rolls, attacks = newAttacks }
        else model
      , Cmd.none
      )

    CastRayOfFrost ->
      ( model
      , Cmd.none
      )

    CastChainLightning ->
      (
        let
          ( numberOfAKind, _ ) = getLargestNumberOfAKind model.rolls
          oldEnemy = model.enemy
          newEnemy = 
            { oldEnemy 
            | hps = attackEnemy ( \index hp -> ( hp - 1 ) ) oldEnemy.hps
            }
        in
          { model | enemy = newEnemy, rolls = List.filter ( \number -> number /= numberOfAKind ) model.rolls }
      , Cmd.none
      )

    CastFireball enemyIndex ->
      (
        let
          ( numberOfAKind, _ ) = getLargestNumberOfAKind model.rolls
          oldEnemy = model.enemy
          newEnemy = 
            { oldEnemy 
            | hps = attackEnemy ( \index hp -> if index == enemyIndex then ( hp - 6 ) else hp ) oldEnemy.hps
            }
        in
          { model | enemy = newEnemy, rolls = List.filter ( \number -> number /= numberOfAKind ) model.rolls }
      , Cmd.none
      )

    CastDemise ->
      (
        let
          oldEnemy = model.enemy
          newEnemy = { oldEnemy | hps = [] }
        in
          { model | enemy = newEnemy, rolls = [] }
      , Cmd.none
      )

    CastMinorHeal name ->
      ( 
        { model 
        | characters =
          List.map
          ( \character -> 
            if character.name == name && character.hp > 0
            then
              if (character.hp + 2) > character.defaultHp
              then { character | hp = character.defaultHp }
              else { character | hp = character.hp + 2 }
            else character
          )
          model.characters
        , rolls = removeNumbers ( List.take 4 ( getLargestStraight model.rolls ) ) model.rolls
        }
      , Cmd.none
      )

    CastHeal name rolls ->
      ( 
        { model 
        | characters =
          List.map
          ( \character -> 
            if ( character.name == name || getNumberOfAKind rolls 1 > 1 ) && character.hp > 0
            then { character | hp = character.defaultHp }
            else character
          )
          model.characters
        , rolls = removeNumbers ( List.take 5 ( getLargestStraight model.rolls ) ) model.rolls
        }
      , Cmd.none
      )

    CastMiracle ->
      ( { model | characters = initCharacters, rolls = [] }
      , Cmd.none
      )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model = 
  div 
  [ classes "ph5 tc", style "font-family" "'Roboto', sans-serif" ] 
  [ showScreen model 
  , Html.node "link" 
    [ Html.Attributes.rel "stylesheet"
    , Html.Attributes.href "https://fonts.googleapis.com/css2?family=Roboto:wght@100;300;400;500;700;900&display=swap" 
    ] 
    []
  , Html.node "link" 
    [ Html.Attributes.rel "stylesheet"
    , Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/tachyons/4.11.1/tachyons.min.css" 
    ] 
    []
  , Html.node "link" 
    [ Html.Attributes.rel "stylesheet"
    , Html.Attributes.href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" 
    ] 
    []
  ]


showScreen : Model -> Html Msg
showScreen model =
  case model.level of
    0 -> showStartScreen model
    _ -> showBattleScreen model


showStartScreen : Model -> Html Msg
showStartScreen model =
  div []
    [ h1 [ classes "f2 fw3" ] [ text "Welcome to Delve, the adventure dice game" ]
    , button [ onClick InitNextLevel ] [ text "Start adventure" ]
    ]


prettyPrintList : String -> List Int -> Html Msg
prettyPrintList label numbers =
  p []
    [ text 
        ( label ++ 
          ( List.foldl 
          ( \number result -> 
            if result == "" 
            then number 
            else result ++ " - " ++ number
          )
          "" 
          ( List.map String.fromInt numbers )
          )
        )
    ]


showBattleScreen : Model -> Html Msg
showBattleScreen model =
  div []
    (List.foldr (++) [] 
    [ [ h2 [ classes "f2 fw3" ] [ text ( "Chapter " ++ (String.fromInt model.level) ++ ": " ++ model.enemy.name ) ] ]
    , [ displayEnemies model.enemy model.rolls model.attacks ]
    , [ hr [] [] ]
    , ( List.map (\character -> displayCharacter character model.rolls) model.characters )
    , [ div [ classes "cb" ]
        --[ prettyPrintList "Attack rolls: " model.rolls
        --, prettyPrintList "Debug: " ( model.debug1 )
        --, prettyPrintList "Debug 2: " ( model.debug2 )
        [ if model.enemy.hps /= [] && not ( hasAvailableActions model.rolls model.attacks ) 
          then button [ onClick Roll ] [ text "Next Round" ] 
          else span [] []
        ]
    , if model.enemy.hps == [] 
      then div [] 
        [ p [] [ text "Congratulations! You've defeated the enemy." ]
        , button [ onClick InitNextLevel ] [ text "Next Chapter" ]
        ]
      else span [] []
    -- , displayAbilities
    ]])


getLevelEnemy : Level -> Enemy
getLevelEnemy number =
  case number of
    1 ->
      { name = "Raiders"
      , amount = {min = 1, max = 6, increments = 0}
      , defaultHp = 3
      , hps = []
      , rolls = []
      , hitFaces = [5, 6]
      , hits = []
      }
    2 -> 
      { name = "Scorpions"
      , amount = {min = 6, max = 30 , increments = 6}
      , defaultHp = 1
      , hps = []
      , rolls = []
      , hitFaces = [6]
      , hits = []
      }
    3 -> 
      { name = "Sandstorm"
      , amount = {min = 3, max = 3, increments = 0}
      , defaultHp = 0
      , hps = []
      , rolls = []
      , hitFaces = [6]
      , hits = []
      }
    4 -> 
      { name = "Ankheg"
      , amount = {min = 1, max = 1, increments = 0}
      , defaultHp = 9
      , hps = []
      , rolls = []
      , hitFaces = [5, 6]
      , hits = []
      }
    5 -> 
      { name = "Monstrous Centipedes"
      , amount = {min = 2, max = 2, increments = 0}
      , defaultHp = 4
      , hps = []
      , rolls = []
      , hitFaces = [5, 6]
      , hits = []
      }
    6 -> 
      { name = "Sphinxes"
      , amount = {min = 3, max = 3, increments = 0}
      , defaultHp = 3
      , hps = []
      , rolls = []
      , hitFaces = [5, 6]
      , hits = []
      }
    7 -> 
      { name = "Time Trap"
      , amount = {min = 1, max = 1, increments = 0}
      , defaultHp = 12
      , hps = []
      , rolls = []
      , hitFaces = [5, 6]
      , hits = []
      }
    _ -> 
      { name = "Congratulations!"
      , amount = {min = 0, max = 0, increments = 0}
      , defaultHp = 0
      , hps = []
      , rolls = []
      , hitFaces = []
      , hits = []
      }


displayAttackButtons : Rolls -> Attacks -> Int -> Html Msg
displayAttackButtons rolls attacks enemyIndex =
  div []
    [ if canCharge rolls attacks 
      then p [] 
        [ button 
          [ onClick ( Charge enemyIndex ) ] 
          [ text ( "Attack (" ++ String.fromInt ( getNumberOfAKind rolls 6 ) ++ ")" ) ] 
        ] 
      else span [] []
    , if canSneakAttack rolls attacks enemyIndex 
      then p [] 
        [ button 
          [ onClick ( SneakAttack enemyIndex ) ] 
          [ text ( "Strike Target (" ++ String.fromInt ( getNumberOfAKind rolls 1 ) ++ ")" ) ]
        ] 
      else span [] []
    , if canCastRayOfFrost rolls 
      then p [] [ button [ onClick CastRayOfFrost ] [ text "Cast: Sleep" ] ] 
      else span [] []
    , if canCastChainLightning rolls 
      then p [] [ button [ onClick CastChainLightning ] [ text "Cast: Shock All" ] ] 
      else span [] []
    , if canCastFireball rolls 
      then p [] [ button [ onClick ( CastFireball enemyIndex ) ] [ text "Cast: Fireball" ] ] 
      else span [] []
    , if canCastDemise rolls 
      then p [] [ button [ onClick CastDemise ] [ text "Cast: Kill All" ] ] 
      else span [] []
    ]


displayHealButtons : Character -> Rolls -> Html Msg
displayHealButtons character rolls =
  div []
    [ if canCastMinorHeal rolls then p [] [ button [ onClick ( CastMinorHeal character.name ) ] [ text "Cast: Minor Heal" ] ] else span [] []
    , if canCastHeal rolls then p [] [ button [ onClick ( CastHeal character.name rolls ) ] [ text "Cast: Full Heal" ] ] else span [] []
    , if canCastMiracle rolls then p [] [ button [ onClick CastMiracle ] [ text "Cast: Restore Party" ] ] else span [] []
    ]


displayEnemies : Enemy -> Rolls -> Attacks -> Html Msg
displayEnemies enemy rolls attacks =
  div []
    ( List.indexedMap 
      ( \index hp -> 
        div [ classes "dib ma3" ]
          [ div [] ( List.map (\_ -> i [ classes "fa fa-heart red" ] [] ) ( List.repeat hp 1 ) )
          , p [ classes "ma0" ] [ text ( "Health: " ++ String.fromInt hp ) ]
          , displayAttackButtons rolls attacks index
          ]
      )
      (List.filter (\hp -> hp > 0) enemy.hps)
    )


displayCharacter : Character -> Rolls -> Html Msg
displayCharacter character rolls =
  div [ classes "dib ma3" ]
    [ p [] [ text character.name ]
    , div [] ( List.map (\_ -> i [ classes "fa fa-heart red" ] [] ) ( List.repeat character.hp 1 ) )
    , p [ classes "ma0" ] [ text ("Health: " ++ String.fromInt character.hp) ]
    , displayHealButtons character rolls
    ]


displayAbilities : Html Msg
displayAbilities =
  ul [ classes "dib ba b--black-70 ma4 pa3 tl list lh-copy" ]
    [ li [] [ text "Fighter - Charge: any 6's do 1 damage, can only be used twice" ]
    , li [] [ text "Rogue - Sneak Attack: Any 1's do 1 damage, can only be used against one enemy" ]
    , li [] [ text "Wizard - Ray of Frost: 3 of a kind, two less monsters attack this round" ]
    , li [] [ text "Wizard - Chain Lightning: 4 of a kind, one damage to each opponent" ]
    , li [] [ text "Wizard - Fireball: 5 of a kind, 6 damage that can be applied to multiple opponents" ]
    , li [] [ text "Wizard - Demise: 6 of a kind, all opponents are destroyed" ]
    , li [] [ text "Cleric - Minor Heal: straight of 4, heal 2 damage" ]
    , li [] [ text "Cleric - Heal: straight of 5, heal the amount. If roll has an additional 1, heal all living party members fully" ]
    , li [] [ text "Cleric - Miracle: straight of 6, resurrect dead adventurers and restore all party members to full health" ]
    ]


getSequenceOfAKind : Rolls -> Int -> List Int
getSequenceOfAKind numbers number = List.filter ( \n -> n == number ) numbers


getNumberOfAKind : Rolls -> Int -> Int
getNumberOfAKind numbers number = List.length ( getSequenceOfAKind numbers number )


getLargestNumberOfAKind : Rolls -> ( Int, Int )
getLargestNumberOfAKind numbers = 
  case
    List.head 
    ( List.reverse 
      ( List.sort
        [ ( 1, getNumberOfAKind numbers 1 )
        , ( 2, getNumberOfAKind numbers 2 )
        , ( 3, getNumberOfAKind numbers 3 )
        , ( 4, getNumberOfAKind numbers 4 )
        , ( 5, getNumberOfAKind numbers 5 )
        , ( 6, getNumberOfAKind numbers 6 )
        ]
      )
    )
  of
  Nothing -> ( 0, 0 )
  Just ( number, numberOfOccurrences ) -> ( number, numberOfOccurrences )


getMaximumNumber : List Int -> Int
getMaximumNumber numbers = case List.maximum numbers of
  Nothing -> 0
  Just number -> number


getLargestStraight : Rolls -> List Int
getLargestStraight numbers = 
  List.foldl 
  ( \n result -> 
    if List.isEmpty result || getMaximumNumber result == ( n - 1 ) 
    then List.append result [ n ] 
    else ( 
      if List.length result < 4 
      then [ n ] 
      else result 
    ) 
  )
  []
  ( getUniqueList numbers )


hasAvailableActions : Rolls -> Attacks -> Bool
hasAvailableActions rolls attacks = 
  canCharge rolls attacks
  || canSneakAttack rolls attacks -1
  || canCastRayOfFrost rolls
  || canCastChainLightning rolls
  || canCastFireball rolls
  || canCastDemise rolls
  || canCastMinorHeal rolls
  || canCastHeal rolls
  || canCastMiracle rolls


canCharge : Rolls -> Attacks -> Bool
canCharge rolls attacks = attacks.charges > 0 && ( getNumberOfAKind rolls 6 ) > 0


canSneakAttack : Rolls -> Attacks -> Int -> Bool
canSneakAttack rolls attacks enemyIndex = 
  ( enemyIndex == -1 || attacks.sneakAttackIndex == -1 || attacks.sneakAttackIndex == enemyIndex )
  && getNumberOfAKind rolls 1 > 0
  && ( getNumberOfAKind rolls 1 ) > 0


canCastRayOfFrost : Rolls -> Bool
canCastRayOfFrost rolls =
  let
    ( _, numberOfOccurrences ) = getLargestNumberOfAKind rolls
  in
    numberOfOccurrences >= 3


canCastChainLightning : Rolls -> Bool
canCastChainLightning rolls =
  let
    ( _, numberOfOccurrences ) = getLargestNumberOfAKind rolls
  in
    numberOfOccurrences >= 4


canCastFireball : Rolls -> Bool
canCastFireball rolls =
  let
    ( _, numberOfOccurrences ) = getLargestNumberOfAKind rolls
  in
    numberOfOccurrences >= 5


canCastDemise : Rolls -> Bool
canCastDemise rolls =
  let
    ( _, numberOfOccurrences ) = getLargestNumberOfAKind rolls
  in
    numberOfOccurrences >= 6


canCastMinorHeal : Rolls -> Bool
canCastMinorHeal rolls = List.length (getLargestStraight rolls) >= 4


canCastHeal : Rolls -> Bool
canCastHeal rolls = List.length (getLargestStraight rolls) >= 5


canCastMiracle : Rolls -> Bool
canCastMiracle rolls = List.length (getLargestStraight rolls) >= 6

