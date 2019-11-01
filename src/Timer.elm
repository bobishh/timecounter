module Timer exposing (TimeLeft, Timer, TimerForm, TimerTime, buildTimerFromForm, fromBase64, timeLeft, timerTimeLeft)

import Base64
import Maybe
import Time exposing (Month(..), utc)
import Time.Extra as Time exposing (Interval(..), Parts, diff, partsToPosix)


type alias TimerForm =
    { year : String
    , month : String
    , day : String
    , hours : String
    , minutes : String
    , seconds : String
    , title : String
    , filled : Bool
    }


numberFromMonth : Month -> Int
numberFromMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        _ ->
            12



-- 1 get years diff
-- if from.month > to.month then from.month - to.month
-- 2019 10 12 00 01 01 -
-- 2019 11 01 23 59 29


bothDiv : Int -> Int -> ( Int, Int )
bothDiv divisible divider =
    ( divisible // divider, modBy divider divisible )


lesser : TimerTime -> TimerTime -> TimerTime
lesser time1 time2 =
    if time1.year > time2.year then
        time2

    else if time1.month > time2.month then
        time2

    else if time1.day > time2.day then
        time2

    else if time1.hours > time2.hours then
        time2

    else if time1.minutes > time2.minutes then
        time2

    else if time1.seconds > time2.seconds then
        time2

    else
        time1


timerTimeLeft : TimerTime -> TimerTime -> TimeLeft
timerTimeLeft from to =
    let
        allDays =
            abs (from.day - to.day)

        weeks =
            0
    in
    { years = abs (from.year - to.year)
    , months = abs (from.month - to.month)
    , weeks = weeks
    , days = abs (from.day - to.day)
    , hours = abs (from.hours - to.hours)
    , minutes = abs (from.minutes - to.minutes)
    , seconds = abs (from.seconds - to.seconds)
    }


timeToTimerTime : Time.Posix -> TimerTime
timeToTimerTime time =
    let
        parts =
            Time.posixToParts utc time
    in
    { year = parts.year
    , month = numberFromMonth parts.month
    , day = parts.day
    , hours = parts.hour
    , minutes = parts.minute
    , seconds = parts.second
    }


formToTimerTime : TimerForm -> TimerTime
formToTimerTime timerForm =
    { year = Maybe.withDefault 1970 (String.toInt timerForm.year)
    , month = Maybe.withDefault 1 (String.toInt timerForm.month)
    , day = Maybe.withDefault 1 (String.toInt timerForm.day)
    , hours = Maybe.withDefault 0 (String.toInt timerForm.hours)
    , minutes = Maybe.withDefault 0 (String.toInt timerForm.minutes)
    , seconds = Maybe.withDefault 0 (String.toInt timerForm.seconds)
    }


type alias TimerTime =
    { year : Int
    , month : Int
    , day : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


type alias Timer =
    { posixSeconds : Int
    , title : String
    }


type alias TimeLeft =
    { years : Int
    , months : Int
    , weeks : Int
    , days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


fromBase64 : String -> Result String Timer
fromBase64 encoded =
    case Base64.decode encoded of
        Ok value ->
            let
                timer =
                    parseTimerString value
            in
            timer

        _ ->
            Err "Can't decode string"


parseTimerString : String -> Result String Timer
parseTimerString timerString =
    case String.split "<|>" timerString of
        [ secondsString, title ] ->
            let
                seconds =
                    parseSeconds secondsString

                timer =
                    Timer seconds title
            in
            Ok timer

        _ ->
            Err "Can't parse url"


parseSeconds : String -> Int
parseSeconds secondsString =
    String.toInt secondsString |> Maybe.withDefault 0


timeLeft : Timer -> Time.Posix -> Time.Zone -> TimeLeft
timeLeft timer time zone =
    let
        from =
            Time.millisToPosix (timer.posixSeconds * 1000)

        years =
            diff Year zone from time

        months =
            diff Month zone from time

        weeks =
            diff Week zone from time

        days =
            diff Day zone from time

        hours =
            diff Hour zone from time

        minutes =
            diff Minute zone from time

        seconds =
            diff Second zone from time
    in
    { years = abs years
    , months = abs months - abs years * 12
    , weeks = abs weeks - abs months * 4 - abs years * 52
    , days = abs days - abs weeks * 7
    , hours = modBy 24 <| abs hours
    , minutes = modBy 60 <| abs minutes
    , seconds = modBy 60 <| abs seconds
    }


buildTimerFromForm : TimerForm -> Maybe Timer
buildTimerFromForm timerForm =
    if timerForm.filled then
        Just
            { posixSeconds = secondsFromTimerForm timerForm, title = timerForm.title }

    else
        Nothing


secondsFromTimerForm : TimerForm -> Int
secondsFromTimerForm form =
    let
        year =
            Maybe.withDefault 0 (String.toInt form.year)

        month =
            monthFromNumber <| Maybe.withDefault 0 (String.toInt form.month)

        day =
            Maybe.withDefault 0 (String.toInt form.day)

        hours =
            Maybe.withDefault 0 (String.toInt form.hours)

        minutes =
            Maybe.withDefault 0 (String.toInt form.minutes)

        seconds =
            Maybe.withDefault 0 (String.toInt form.seconds)

        posix =
            Time.posixToMillis (Time.partsToPosix utc (Time.Parts year month day hours minutes seconds 0))
    in
    posix // 1000


monthFromNumber : Int -> Month
monthFromNumber number =
    case number of
        1 ->
            Jan

        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        _ ->
            Dec
