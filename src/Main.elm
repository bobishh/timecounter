module Main exposing (main)

import Base64
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Event exposing (Event, fromBase64, secondsToString)
import Html exposing (Html)
import Html.Events exposing (..)
import Maybe
import Task
import Time
import Url
import Url.Parser as Url exposing ((</>), Parser, custom)


yellow =
    rgb255 239 233 174


purple =
    rgb255 239 233 174


black =
    rgb255 0 0 0


white =
    rgb255 255 255 255


type alias Time =
    Float


type alias EncodedEvent =
    String


type alias EventForm =
    { year : String
    , month : String
    , day : String
    , hours : String
    , minutes : String
    , seconds : String
    , title : String
    , filled : Bool
    }


type alias Timer =
    { secondsLeft : Int
    , event : Event
    }


type alias Model =
    { navKey : Nav.Key
    , page : Page
    , timer : Maybe Timer
    }


type Page
    = Index
    | CreateTimer EventForm
    | ShowTimer Event
    | Dunno


base64StringToEvent : String -> Maybe Event
base64StringToEvent string =
    case fromBase64 string of
        Ok event ->
            Just event

        Err _ ->
            Nothing


templateEventForm : EventForm
templateEventForm =
    { year = ""
    , month = ""
    , day = ""
    , hours = ""
    , minutes = ""
    , seconds = ""
    , title = ""
    , filled = False
    }


base64Parser : Parser (Event -> a) a
base64Parser =
    custom "STRING" base64StringToEvent


newEventStringParser : String -> Maybe EventForm
newEventStringParser string =
    if string == "new" then
        Just templateEventForm

    else
        Nothing


newEventParser : Parser (EventForm -> a) a
newEventParser =
    custom "STRING" newEventStringParser


urlToPage : Url.Url -> Page
urlToPage url =
    -- We start with our URL
    url
        -- Send it through our URL parser (located below)
        |> Url.parse urlParser
        -- And if it didn't match any known pages, return Index
        |> Maybe.withDefault Dunno


urlParser : Parser (Page -> a) a
urlParser =
    -- We try to match one of the following URLs
    Url.oneOf
        -- Url.top matches root (i.e. there is nothing after 'https://example.com')
        [ Url.map Index Url.top

        -- Url.s matches URLs ending with some string, in our case '/cats'
        , Url.map CreateTimer newEventParser

        -- Again, Url.s matches a string. </> matches a '/' in the URL, and Url.int matches any integer and "returns" it, so that the user page value gets the user ID
        , Url.map ShowTimer (Url.s "t" </> base64Parser)
        ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { navKey = key
      , page = urlToPage url
      , timer = Nothing
      }
    , maybeSetTimer
    )


maybeSetTimer : Cmd Msg
maybeSetTimer =
    Task.perform MaybeSetTimer Time.now


type Msg
    = LinkClicked UrlRequest
    | UpdateEventForm EventForm
    | BuildEvent EventForm
    | UrlChange Url.Url
    | Tick Time.Posix
    | MaybeSetTimer Time.Posix


updateTimer : Model -> Time.Posix -> Model
updateTimer model time =
    case model.timer of
        Nothing ->
            model

        Just timer ->
            let
                realSecondsLeft =
                    Time.posixToMillis time // 1000 - timer.event.posixSeconds

                newTimer =
                    { timer | secondsLeft = realSecondsLeft }
            in
            { model | timer = Just newTimer }


maybeBuildTimer : Time.Posix -> Event -> Maybe Timer
maybeBuildTimer time event =
    let
        secondsLeft =
            Time.posixToMillis time // 1000 - event.posixSeconds

        timer =
            Timer secondsLeft event
    in
    Just timer


secondsFromEventForm : EventForm -> Int
secondsFromEventForm form =
    let
        year =
            Maybe.withDefault 0 (String.toInt form.year)

        month =
            Maybe.withDefault 0 (String.toInt form.month)

        day =
            Maybe.withDefault 0 (String.toInt form.day)

        hours =
            Maybe.withDefault 0 (String.toInt form.hours)

        minutes =
            Maybe.withDefault 0 (String.toInt form.minutes)

        seconds =
            Maybe.withDefault 0 (String.toInt form.seconds)
    in
    (year - 1970)
        * 31540000
        + ((month - 1) * 2628600)
        + (day * 86400)
        + (hours * 3660)
        + (minutes * 60)
        + seconds


buildEvent : EventForm -> Event
buildEvent eventForm =
    { posixSeconds = secondsFromEventForm eventForm, title = eventForm.title }


buildUrlString : Event -> String
buildUrlString event =
    "/t/" ++ Base64.encode (String.fromInt event.posixSeconds ++ "<|>" ++ event.title)


formFilled : EventForm -> Bool
formFilled form =
    True


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuildEvent eventForm ->
            let
                event =
                    buildEvent eventForm

                newUrl =
                    buildUrlString event
            in
            ( { model | page = ShowTimer event }, Nav.pushUrl model.navKey newUrl )

        MaybeSetTimer newTime ->
            case model.page of
                ShowTimer event ->
                    ( { model | timer = maybeBuildTimer newTime event }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateEventForm eventForm ->
            let
                updatedForm =
                    { eventForm | filled = formFilled eventForm }
            in
            case model.page of
                CreateTimer event ->
                    ( { model | page = CreateTimer eventForm }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick newTime ->
            case model.timer of
                Nothing ->
                    ( model, Cmd.none )

                Just value ->
                    ( updateTimer model newTime, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChange url ->
            ( { model | page = urlToPage url }
            , maybeSetTimer
            )


inputStyles =
    [ Font.color (rgb255 0 0 0), width (fillPortion 1), spacing 36, paddingEach edges ]


edges =
    { top = 15
    , right = 0
    , bottom = 15
    , left = 0
    }


placeholder placeholderText =
    Just (Input.placeholder [] (el [ alignLeft ] (text placeholderText)))


renderEventForm : EventForm -> Element Msg
renderEventForm eventForm =
    el [ padding 100 ]
        (column [ centerX, centerY, width fill, Font.size 50 ]
            [ row [ width fill ]
                [ Input.text inputStyles
                    { label =
                        Input.labelHidden "Title"
                    , placeholder = placeholder "Name it"
                    , text = eventForm.title
                    , onChange = \title -> UpdateEventForm { eventForm | title = title }
                    }
                ]
            , column [ width fill ]
                [ row [ Font.size 14, paddingEach edges ] [ text "add date" ]
                , row [ width fill ]
                    [ Input.text inputStyles
                        { label =
                            Input.labelHidden "Year"
                        , placeholder = placeholder "YYYY"
                        , text = eventForm.year
                        , onChange = \year -> UpdateEventForm { eventForm | year = sanitizeInt year 9999 }
                        }
                    , Input.text inputStyles
                        { label =
                            Input.labelHidden "Month"
                        , placeholder = placeholder "MM"
                        , text = eventForm.month
                        , onChange = \month -> UpdateEventForm { eventForm | month = sanitizeInt month 12 }
                        }
                    , Input.text inputStyles
                        { label =
                            Input.labelHidden "Day"
                        , placeholder = placeholder "DD"
                        , text = eventForm.day
                        , onChange = \day -> UpdateEventForm { eventForm | day = sanitizeInt day 31 }
                        }
                    ]
                ]
            , row [ Font.size 50 ]
                [ Input.text inputStyles
                    { label =
                        Input.labelHidden "Hours"
                    , placeholder = placeholder "HH"
                    , text = eventForm.hours
                    , onChange = \hours -> UpdateEventForm { eventForm | hours = sanitizeInt hours 23 }
                    }
                , Input.text inputStyles
                    { label =
                        Input.labelHidden "Minutes"
                    , placeholder = placeholder "MM"
                    , text = eventForm.minutes
                    , onChange = \minutes -> UpdateEventForm { eventForm | minutes = sanitizeInt minutes 59 }
                    }
                , Input.text inputStyles
                    { label =
                        Input.labelHidden "Seconds"
                    , placeholder = placeholder "SS"
                    , text = eventForm.seconds
                    , onChange = \seconds -> UpdateEventForm { eventForm | seconds = sanitizeInt seconds 59 }
                    }
                ]
            , row [ Font.size 50 ]
                [ Input.button
                    [ Background.color black
                    , Font.color white
                    , paddingXY 32 16
                    , Border.rounded 3
                    , width fill
                    ]
                    { onPress = Just (BuildEvent eventForm)
                    , label = Element.text "apply"
                    }
                ]
            ]
        )


sanitizeInt : String -> Int -> String
sanitizeInt value limit =
    let
        intValue =
            abs (Maybe.withDefault 0 (String.toInt value))
    in
    if intValue < 10 then
        "0" ++ String.fromInt intValue

    else if intValue > limit then
        String.fromInt limit

    else
        String.fromInt intValue


renderPage : Model -> Element Msg
renderPage model =
    case model.page of
        Dunno ->
            column [ centerX, centerY ]
                [ row [ centerX, centerY ]
                    [ Element.el [ centerX, centerY, Font.size 30 ] (text "Error! Dunno what to do with that url :(")
                    ]
                ]

        Index ->
            column [ spacing 100, padding 100, Font.size 45 ]
                [ paragraph [] [ text "This site helps you to track something that is important to you. " ]
                , paragraph [] [ text "Be it last time you had sex, or a day you quit smoking. " ]
                , paragraph []
                    [ text " You can create your own timer by clicking "
                    , link
                        [ Font.heavy, Font.underline ]
                        { url = "/new", label = text "here." }
                    ]
                ]

        CreateTimer eventForm ->
            column [] [ renderEventForm eventForm ]

        ShowTimer encodedEvent ->
            case model.timer of
                Just timer ->
                    let
                        timeLeft =
                            Event.secondsToString timer.secondsLeft
                    in
                    column [ centerX, centerY, width (fillPortion 1), Font.size 45 ]
                        [ row [ centerX, centerY ] [ text timer.event.title ]
                        , el [ centerX, centerY ] (paragraph [] [ text timeLeft ])
                        ]

                Nothing ->
                    row [ Font.size 30 ] [ text "Can't decode event data, check url" ]


view : Model -> Browser.Document Msg
view model =
    { title = "Counting seconds"
    , body =
        [ layout
            [ Background.color yellow
            , Font.color black
            , Font.size 16
            , Font.light
            , Font.family
                [ Font.typeface "Fira Code"
                , Font.sansSerif
                ]
            ]
            (renderPage model)
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        ShowTimer _ ->
            Time.every 1000 Tick

        _ ->
            Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChange
        }
