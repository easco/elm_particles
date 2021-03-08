module Main exposing
    ( Model
    , Msg(..)
    , init
    , initialModel
    , main
    , particleToCircle
    , particlesToCircles
    , spawnNewParticles
    , step
    , subscriptions
    , update
    , view
    )

import Browser exposing (..)
import Browser.Events exposing (onAnimationFrame)
import Html exposing (..)
import Html.Attributes exposing (..)
import Particle
import ParticleSystem exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)


type alias Model =
    ParticleSystem.ParticleSystem


type Msg
    = Tick Posix
    | NewParticles ParticleSystem.ParticleList


init : String -> ( Model, Cmd msg )
init _ =
    ( initialModel
    , randomizeParticles (List.repeat 100 (Particle.new 0 0))
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick posixTime ->
            step model (posixToMillis posixTime)

        NewParticles newParticles ->
            ( addParticles model newParticles, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrame Tick
        , randomizedParticles NewParticles
        ]


view : Model -> Html Msg
view model =
    div [ Html.Attributes.id "root" ]
        [ svg [ Svg.Attributes.width "1024", Svg.Attributes.height "768", viewBox "0 0 1024 768" ]
            [ rect [ fill "black", x "0", y "0", Svg.Attributes.width "1024", Svg.Attributes.height "768" ] []
            , g [ transform "translate(512, 384)", fill "white" ] (particlesToCircles model)
            ]
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


step : Model -> Int -> ( Model, Cmd msg )
step system currentTick =
    let
        elapsedTime =
            currentTick - system.lastTick
    in
    if elapsedTime > 60 then
        ParticleSystem.step system currentTick
            |> spawnNewParticles

    else
        ( system, Cmd.none )


spawnNewParticles : Model -> ( Model, Cmd msg )
spawnNewParticles model =
    case List.partition Particle.isDead model.particles of
        ( [], live ) ->
            ( model, Cmd.none )

        ( dead, live ) ->
            let
                newParticles =
                    List.repeat (List.length dead) (Particle.new 0 0)
            in
            ( { model | particles = live }, randomizeParticles newParticles )


initialModel : Model
initialModel =
    { lastTick = 0
    , particles = []
    }


particlesToCircles : ParticleSystem -> List (Svg msg)
particlesToCircles system =
    system.particles
        |> List.map particleToCircle


particleToCircle : Particle.Particle -> Svg msg
particleToCircle particle =
    circle [ cx (String.fromFloat particle.x), cy (String.fromFloat particle.y), r (String.fromInt (15 - particle.age)), fill (Particle.color particle) ] []
