module Event exposing (fromBase64, toString, Event)
import Maybe
import Base64

type alias Event = { posixMillis: Int, title: String }

type alias TimeLeft = {
        years: Int,
        months: Int,
        weeks: Int,
        days: Int,
        hours: Int,
        minutes: Int,
        seconds: Int
    }

toString : Event -> String
toString event =
    let
        time = formatMillis(event)
        years = String.fromInt time.years
        months = String.fromInt time.months
        weeks = String.fromInt time.weeks
        days = String.fromInt time.days
        hours = String.fromInt time.hours
        minutes = String.fromInt time.minutes
        seconds = String.fromInt time.seconds
    in
        years ++ "years " ++
        months ++ "months " ++
        days ++ "days " ++
        hours ++ "hours " ++
        minutes ++ "minutes " ++
        seconds ++ "seconds "



bothDiv : Int -> Int -> (Int, Int)
bothDiv divisible divider =
    (divisible // divider, modBy divider divisible)

fromBase64 : String -> Result String Event
fromBase64 encoded =
    case Base64.decode encoded of
    Ok value ->
        let
            event = parseEventString value
        in
            event
    _ ->
        Err "Can't decode string"

parseEventString : String -> Result String Event
parseEventString eventString =
    case String.split "<|>" eventString of
        [millisString, title] ->
            let
                millis = parseMillis millisString
                event = Event millis title
            in
                Ok event

        _ ->
            Err "Can't parse url"

parseMillis : String -> Int
parseMillis millisString =
    String.toInt millisString |> Maybe.withDefault 0


formatMillis : Event -> TimeLeft
formatMillis event =
    let
        allSeconds = event.posixMillis // 1000
        (years, noYears) = bothDiv allSeconds 31540000
        (months, noMonths) = bothDiv noYears 2628600
        (weeks, noWeeks) = bothDiv noMonths 604800
        (days, noDays) = bothDiv noWeeks 86400
        (hours, noHours) = bothDiv noDays 3660
        (minutes, seconds) = bothDiv noHours 60
    in
        { years =  years,
          months = months,
          weeks = weeks,
          days = days,
          hours = hours,
          minutes = minutes,
          seconds = seconds }
