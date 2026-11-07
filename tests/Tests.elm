module Tests exposing (all)

import Base64
import Expect
import Fuzz
import Test exposing (..)
import Time exposing (Month(..), utc)
import Time.Extra as Extra
import Timer exposing (TimeLeft, Timer, TimerForm)


form : TimerForm
form =
    { title = "A milestone", year = "2024", month = "2", day = "29", hours = "", minutes = "", seconds = "" }


at : Int -> Month -> Int -> Int -> Int -> Int -> Time.Posix
at year month day hour minute second =
    Extra.partsToPosix utc (Extra.Parts year month day hour minute second 0)


timerAt : Time.Posix -> Timer
timerAt posix =
    { posixSeconds = floor (toFloat (Time.posixToMillis posix) / 1000), title = "Event" }


all : Test
all =
    describe "Timer behavior"
        [ describe "Calendar duration used by the screen"
            [ test "A week has no leftover days" <|
                \_ ->
                    Expect.equal (TimeLeft 0 0 1 0 0 0 0)
                        (Timer.timeLeft (timerAt (at 2019 Nov 1 0 0 0)) (at 2019 Nov 8 0 0 0) utc)
            , test "A year and a month leave no negative weeks" <|
                \_ ->
                    Expect.equal (TimeLeft 1 1 0 0 0 0 0)
                        (Timer.timeLeft (timerAt (at 2019 Jan 1 0 0 0)) (at 2020 Feb 1 0 0 0) utc)
            , test "Borrow across midnight and year boundary" <|
                \_ ->
                    Expect.equal (TimeLeft 0 0 0 0 0 0 1)
                        (Timer.timeLeft (timerAt (at 2023 Dec 31 23 59 59)) (at 2024 Jan 1 0 0 0) utc)
            , test "Leap year month end clamps to February 29" <|
                \_ ->
                    Expect.equal (TimeLeft 0 1 0 0 0 0 0)
                        (Timer.timeLeft (timerAt (at 2024 Jan 31 0 0 0)) (at 2024 Feb 29 0 0 0) utc)
            , test "Future events use the same positive duration" <|
                \_ ->
                    Expect.equal (TimeLeft 0 0 1 1 2 3 4)
                        (Timer.timeLeft (timerAt (at 2024 Mar 9 2 3 4)) (at 2024 Mar 1 0 0 0) utc)
            , test "Equal timestamps render zero" <|
                \_ ->
                    Expect.equal (TimeLeft 0 0 0 0 0 0 0)
                        (Timer.timeLeft (timerAt (at 2024 Jan 1 0 0 0)) (at 2024 Jan 1 0 0 0) utc)
            , fuzz (Fuzz.intRange 0 2000000000) "Components stay nonnegative and bounded" <|
                \seconds ->
                    let
                        duration =
                            Timer.timeLeft { posixSeconds = 0, title = "Epoch" } (Time.millisToPosix (seconds * 1000)) utc
                    in
                    Expect.equal True
                        (duration.years >= 0 && duration.months >= 0 && duration.months < 12 && duration.weeks >= 0 && duration.days >= 0 && duration.days < 7 && duration.hours >= 0 && duration.hours < 24 && duration.minutes >= 0 && duration.minutes < 60 && duration.seconds >= 0 && duration.seconds < 60)
            ]
        , describe "Form validation"
            [ test "Leap day accepted, empty clock means midnight" <|
                \_ ->
                    Expect.equal (Ok { posixSeconds = 1709164800, title = "A milestone" }) (Timer.buildTimerFromForm form)
            , test "Impossible day rejected" <|
                \_ ->
                    Expect.err (Timer.buildTimerFromForm { form | year = "2023" })
            , test "Incomplete date rejected" <|
                \_ ->
                    Expect.err (Timer.buildTimerFromForm { form | year = "" })
            , test "Blank title rejected" <|
                \_ ->
                    Expect.err (Timer.buildTimerFromForm { form | title = "   " })
            , test "Out of range clock rejected" <|
                \_ ->
                    Expect.err (Timer.buildTimerFromForm { form | hours = "24" })
            , test "Dates after 2038 avoid signed integer division overflow" <|
                \_ ->
                    Expect.equal (Ok { posixSeconds = 4107542400, title = "A milestone" })
                        (Timer.buildTimerFromForm { form | year = "2100", month = "3", day = "1" })
            ]
        , describe "Share links"
            [ fuzz Fuzz.string "Unicode and delimiter titles round-trip" <|
                \title ->
                    let
                        timer =
                            { posixSeconds = 1709164800, title = title ++ "<|>Привет 🎉 / +" }
                    in
                    Expect.equal (Ok timer) (Timer.fromBase64 (Timer.toBase64 timer))
            , test "New links contain no path separators or padding" <|
                \_ ->
                    let
                        encoded =
                            Timer.toBase64 { posixSeconds = 0, title = "???~~~🎉" }
                    in
                    Expect.equal False (List.any (\char -> String.contains char encoded) [ "/", "+", "=" ])
            , test "Original padded Base64 links still decode" <|
                \_ ->
                    Expect.equal (Ok { posixSeconds = 1572566400, title = "Original" })
                        (Timer.fromBase64 (Base64.encode "1572566400<|>Original"))
            , test "Malformed timestamp rejected instead of becoming epoch" <|
                \_ ->
                    Expect.err (Timer.fromBase64 (Base64.encode "oops<|>Event"))
            , test "Malformed Base64 rejected" <|
                \_ ->
                    Expect.err (Timer.fromBase64 "!broken!")
            ]
        ]
