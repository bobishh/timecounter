module Tests exposing (..)

import Expect
import Test exposing (..)
import Timer



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Timer.timerTimeLeft"
        [ describe "Timer.timeLeft"
            [ test "One month diff" <|
                \_ ->
                    let
                        timer1 =
                            Timer.TimerTime 2019 11 1 0 0 0

                        timer2 =
                            Timer.TimerTime 2019 10 1 0 0 0

                        timeLeft =
                            Timer.timerTimeLeft timer1 timer2
                    in
                    Expect.equal timeLeft (Timer.TimeLeft 0 1 0 0 0 0 0)
            , test "One day diff" <|
                \_ ->
                    let
                        timer1 =
                            Timer.TimerTime 2019 11 1 0 0 0

                        timer2 =
                            Timer.TimerTime 2019 11 2 0 0 0

                        timeLeft =
                            Timer.timerTimeLeft timer1 timer2
                    in
                    Expect.equal timeLeft (Timer.TimeLeft 0 0 0 1 0 0 0)
            ]
        , test "One week diff" <|
            \_ ->
                let
                    timer1 =
                        Timer.TimerTime 2019 11 1 0 0 0

                    timer2 =
                        Timer.TimerTime 2019 11 8 0 0 0

                    timeLeft =
                        Timer.timerTimeLeft timer1 timer2
                in
                Expect.equal timeLeft (Timer.TimeLeft 0 0 1 1 0 0 0)
        ]
