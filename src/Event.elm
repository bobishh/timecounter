module Event exposing (Event, fromBase64, secondsToString)

import Base64
import Maybe


type alias Event =
    { posixSeconds : Int, title : String }


type alias TimeLeft =
    { years : Int
    , months : Int
    , weeks : Int
    , days : Int
    , hours : Int
    , minutes : Int
    , seconds : Int
    }


maybeLabel : Int -> String -> String
maybeLabel value string =
    if value > 0 then
        String.fromInt value ++ " " ++ string ++ " "

    else
        ""


secondsToString : Int -> String
secondsToString seconds =
    let
        time =
            formatSeconds (abs seconds)
    in
    maybeLabel time.years "years"
        ++ maybeLabel time.months "months"
        ++ maybeLabel time.weeks "weeks"
        ++ maybeLabel time.days "days"
        ++ maybeLabel time.hours "hours"
        ++ maybeLabel time.minutes "minutes"
        ++ maybeLabel time.seconds "seconds"


bothDiv : Int -> Int -> ( Int, Int )
bothDiv divisible divider =
    ( divisible // divider, modBy divider divisible )


fromBase64 : String -> Result String Event
fromBase64 encoded =
    case Base64.decode encoded of
        Ok value ->
            let
                event =
                    parseEventString value
            in
            event

        _ ->
            Err "Can't decode string"


toBase64 : Event -> String
toBase64 event =
    let
        secondsString =
            String.fromInt event.posixSeconds
    in
    Base64.encode secondsString ++ "<|>" ++ event.title


parseEventString : String -> Result String Event
parseEventString eventString =
    case String.split "<|>" eventString of
        [ secondsString, title ] ->
            let
                seconds =
                    parseSeconds secondsString

                event =
                    Event seconds title
            in
            Ok event

        _ ->
            Err "Can't parse url"


parseSeconds : String -> Int
parseSeconds secondsString =
    String.toInt secondsString |> Maybe.withDefault 0


formatSeconds : Int -> TimeLeft
formatSeconds allSeconds =
    let
        ( years, noYears ) =
            bothDiv allSeconds 31540000

        ( months, noMonths ) =
            bothDiv noYears 2628600

        ( weeks, noWeeks ) =
            bothDiv noMonths 604800

        ( days, noDays ) =
            bothDiv noWeeks 86400

        ( hours, noHours ) =
            bothDiv noDays 3660

        ( minutes, seconds ) =
            bothDiv noHours 60
    in
    { years = years
    , months = months
    , weeks = weeks
    , days = days
    , hours = hours
    , minutes = minutes
    , seconds = seconds
    }
