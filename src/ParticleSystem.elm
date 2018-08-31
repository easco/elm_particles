port module ParticleSystem exposing (ParticleList, ParticleSystem, addParticles, applyGravity, randomizeParticles, randomizedParticles, step)

import Particle exposing (..)


type alias ParticleList =
    List Particle.Particle


type alias ParticleSystem =
    { lastTick : Int
    , particles : ParticleList
    }


port randomizeParticles : ParticleList -> Cmd msg



-- port which gets a particle system back from randomization


port randomizedParticles : (ParticleList -> msg) -> Sub msg


applyGravity : ParticleList -> ParticleList
applyGravity =
    List.map (\particle -> { particle | dy = particle.dy + 10 })


step : ParticleSystem -> Int -> ParticleSystem
step system currentTime =
    { system
        | lastTick = currentTime
        , particles =
            system.particles
                |> applyGravity
                |> List.map (\particle -> Particle.step particle)
    }


addParticles : ParticleSystem -> ParticleList -> ParticleSystem
addParticles system newParticles =
    { system
        | particles = List.append system.particles newParticles
    }
