module Particle exposing (Particle, color, isDead, new, step)


type alias Particle =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , age : Int
    }


new : Float -> Float -> Particle
new x y =
    { x = x, y = y, dx = 0, dy = 0, age = 0 }


isDead : Particle -> Bool
isDead particle =
    (particle.y > 768) || (particle.age > 15)


step : Particle -> Particle
step particle =
    { particle | x = particle.x + particle.dx, y = particle.y + particle.dy, age = particle.age + 1 }


color : Particle -> String
color particle =
    let
        greenBlueValue =
            String.fromInt (round (toFloat particle.age / 15.0 * 255.0))
    in
    "rgb(255," ++ greenBlueValue ++ ",0)"
