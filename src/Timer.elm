module Timer exposing (TimeLeft, Timer, TimerForm, buildTimerFromForm, fromBase64, timeLeft, toBase64)

import Base64
import Time exposing (Month(..), utc)
import Time.Extra as Extra exposing (Interval(..))


type alias TimerForm =
    { year : String
    , month : String
    , day : String
    , hours : String
    , minutes : String
    , seconds : String
    , title : String
    }


type alias Timer =
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


toBase64 : Timer -> String
toBase64 timer =
    Base64.encode (String.fromInt timer.posixSeconds ++ "<|>" ++ timer.title)
        |> String.replace "+" "-"
        |> String.replace "/" "_"
        |> String.replace "=" ""


fromBase64 : String -> Result String Timer
fromBase64 encoded =
    let
        normalized =
            encoded |> String.replace "-" "+" |> String.replace "_" "/"

        padded =
            normalized ++ String.repeat (modBy 4 (4 - modBy 4 (String.length normalized))) "="
    in
    Base64.decode padded
        |> Result.andThen
            (\value ->
                case String.split "<|>" value of
                    secondsString :: firstTitle :: rest ->
                        case String.toInt secondsString of
                            Just seconds ->
                                if seconds >= -62135596800 && seconds <= 253402300799 then
                                    Ok { posixSeconds = seconds, title = String.join "<|>" (firstTitle :: rest) }

                                else
                                    Err "Date is outside the supported range."

                            Nothing ->
                                Err "Invalid timestamp."

                    _ ->
                        Err "Invalid timer link."
            )


{-| Consume calendar years, then months, then split the remaining duration.
Each cursor advances from the previous one; months and years are never
approximated as a fixed number of weeks. Month-end dates clamp to the last day.
-}
timeLeft : Timer -> Time.Posix -> Time.Zone -> TimeLeft
timeLeft timer now zone =
    let
        event =
            Time.millisToPosix (timer.posixSeconds * 1000)

        ( start, finish ) =
            if Time.posixToMillis event <= Time.posixToMillis now then
                ( event, now )

            else
                ( now, event )

        ( years, afterYears ) =
            consume Year zone start finish

        ( months, afterMonths ) =
            consume Month zone afterYears finish

        remaining =
            floor (toFloat (Time.posixToMillis finish - Time.posixToMillis afterMonths) / 1000)

        days =
            remaining // 86400
    in
    { years = years
    , months = months
    , weeks = days // 7
    , days = modBy 7 days
    , hours = modBy 24 (remaining // 3600)
    , minutes = modBy 60 (remaining // 60)
    , seconds = modBy 60 remaining
    }


consume : Interval -> Time.Zone -> Time.Posix -> Time.Posix -> ( Int, Time.Posix )
consume interval zone start finish =
    let
        estimate =
            Extra.diff interval zone start finish

        next =
            Extra.add interval (estimate + 1) zone start

        count =
            if Time.posixToMillis next <= Time.posixToMillis finish then
                estimate + 1

            else if Time.posixToMillis (Extra.add interval estimate zone start) > Time.posixToMillis finish then
                max 0 (estimate - 1)

            else
                estimate
    in
    ( count, Extra.add interval count zone start )


buildTimerFromForm : TimerForm -> Result String Timer
buildTimerFromForm form =
    let
        clock value =
            if String.isEmpty (String.trim value) then
                Just 0

            else
                String.toInt value

        numbers =
            List.map String.toInt [ form.year, form.month, form.day ]
                ++ List.map clock [ form.hours, form.minutes, form.seconds ]
    in
    if String.isEmpty (String.trim form.title) then
        Err "Give your timer a name."

    else
        case numbers of
            [ Just year, Just month, Just day, Just hours, Just minutes, Just seconds ] ->
                if year < 1 || year > 9999 || month < 1 || month > 12 || day < 1 || day > 31 || hours < 0 || hours > 23 || minutes < 0 || minutes > 59 || seconds < 0 || seconds > 59 then
                    Err "Enter a valid date and time (UTC)."

                else
                    let
                        parts =
                            Extra.Parts year (monthFromNumber month) day hours minutes seconds 0

                        posix =
                            Extra.partsToPosix utc parts
                    in
                    if Extra.posixToParts utc posix /= parts then
                        Err "That date does not exist. Check the month and day."

                    else
                        Ok { posixSeconds = floor (toFloat (Time.posixToMillis posix) / 1000), title = String.trim form.title }

            _ ->
                Err "Enter a year, month and day. Time is optional."


monthFromNumber : Int -> Month
monthFromNumber number =
    List.drop (number - 1) [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
        |> List.head
        |> Maybe.withDefault Jan
