module Main exposing (main)

-- Time extra

import Base64
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Maybe
import String.Extra
import Task
import Time exposing (Month(..), utc)
import Time.Extra as Time
import Timer exposing (..)
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


type alias Model =
    { navKey : Nav.Key
    , page : Page
    , now : Maybe Time.Posix
    }


type Page
    = Index
    | CreateTimer TimerForm
    | ShowTimer Timer
    | Dunno


base64StringToTimer : String -> Maybe Timer
base64StringToTimer string =
    case fromBase64 string of
        Ok timer ->
            Just timer

        Err _ ->
            Nothing


templateTimerForm : TimerForm
templateTimerForm =
    { year = ""
    , month = ""
    , day = ""
    , hours = ""
    , minutes = ""
    , seconds = ""
    , title = ""
    , filled = False
    }


base64Parser : Parser (Timer -> a) a
base64Parser =
    custom "STRING" base64StringToTimer


newTimerStringParser : String -> Maybe TimerForm
newTimerStringParser string =
    if string == "new" then
        Just templateTimerForm

    else
        Nothing


newTimerParser : Parser (TimerForm -> a) a
newTimerParser =
    custom "STRING" newTimerStringParser


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
        , Url.map CreateTimer newTimerParser

        -- Again, Url.s matches a string. </> matches a '/' in the URL, and Url.int matches any integer and "returns" it, so that the user page value gets the user ID
        , Url.map ShowTimer (Url.s "t" </> base64Parser)
        ]


tickOnce : Cmd Msg
tickOnce =
    Task.perform Tick Time.now


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { navKey = key
      , page = urlToPage url
      , now = Nothing
      }
    , tickOnce
    )


type Msg
    = LinkClicked UrlRequest
    | UpdateTimerForm TimerForm
    | BuildTimer TimerForm
    | UrlChange Url.Url
    | Tick Time.Posix


buildUrlString : Timer -> String
buildUrlString timer =
    "/t/" ++ Base64.encode (String.fromInt timer.posixSeconds ++ "<|>" ++ timer.title)


formFilled : TimerForm -> Bool
formFilled form =
    True


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuildTimer timerForm ->
            case buildTimerFromForm timerForm of
                Just timer ->
                    ( { model | page = ShowTimer timer }, Nav.pushUrl model.navKey (buildUrlString timer) )

                Nothing ->
                    ( model, Cmd.none )

        UpdateTimerForm timerForm ->
            let
                updatedForm =
                    { timerForm | filled = True }
            in
            case model.page of
                CreateTimer _ ->
                    ( { model | page = CreateTimer updatedForm }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick newTime ->
            ( { model | now = Just newTime }, Cmd.none )

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
            , tickOnce
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


renderTimerForm : TimerForm -> Element Msg
renderTimerForm timerForm =
    el [ padding 100 ]
        (column [ centerX, centerY, width fill, Font.size 50 ]
            [ row [ width fill ]
                [ Input.text inputStyles
                    { label =
                        Input.labelHidden "Title"
                    , placeholder = placeholder "Name it"
                    , text = timerForm.title
                    , onChange = \title -> UpdateTimerForm { timerForm | title = title }
                    }
                ]
            , column [ width fill ]
                [ row [ Font.size 14, paddingEach edges ] [ text "add date" ]
                , row [ width fill ]
                    [ Input.text inputStyles
                        { label =
                            Input.labelHidden "Year"
                        , placeholder = placeholder "YYYY"
                        , text = timerForm.year
                        , onChange = \year -> UpdateTimerForm { timerForm | year = sanitizeInt year 9999 }
                        }
                    , Input.text inputStyles
                        { label =
                            Input.labelHidden "Month"
                        , placeholder = placeholder "MM"
                        , text = timerForm.month
                        , onChange = \month -> UpdateTimerForm { timerForm | month = sanitizeInt month 12 }
                        }
                    , Input.text inputStyles
                        { label =
                            Input.labelHidden "Day"
                        , placeholder = placeholder "DD"
                        , text = timerForm.day
                        , onChange = \day -> UpdateTimerForm { timerForm | day = sanitizeInt day 31 }
                        }
                    ]
                ]
            , row [ Font.size 50 ]
                [ Input.text inputStyles
                    { label =
                        Input.labelHidden "Hours"
                    , placeholder = placeholder "HH"
                    , text = timerForm.hours
                    , onChange = \hours -> UpdateTimerForm { timerForm | hours = sanitizeInt hours 23 }
                    }
                , Input.text inputStyles
                    { label =
                        Input.labelHidden "Minutes"
                    , placeholder = placeholder "MM"
                    , text = timerForm.minutes
                    , onChange = \minutes -> UpdateTimerForm { timerForm | minutes = sanitizeInt minutes 59 }
                    }
                , Input.text inputStyles
                    { label =
                        Input.labelHidden "Seconds"
                    , placeholder = placeholder "SS"
                    , text = timerForm.seconds
                    , onChange = \seconds -> UpdateTimerForm { timerForm | seconds = sanitizeInt seconds 59 }
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
                    { onPress = Just (BuildTimer timerForm)
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


maybePluralize : Int -> String -> String
maybePluralize value title =
    String.Extra.pluralize title (title ++ "s") value


maybeInterval : Int -> String -> Element Msg
maybeInterval value title =
    if value > 0 then
        el [] (text (" " ++ maybePluralize value title))

    else
        text ""


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

        CreateTimer timerForm ->
            column [] [ renderTimerForm timerForm ]

        ShowTimer timer ->
            case model.now of
                Nothing ->
                    row [] [ text "Initializing ..." ]

                Just time ->
                    let
                        timerTimeLeft =
                            timeLeft timer time utc
                    in
                    column [ centerX, centerY, width (fillPortion 1), Font.size 45 ]
                        [ row [ centerX, centerY ] [ text timer.title ]
                        , row [ centerX, centerY ]
                            [ maybeInterval timerTimeLeft.years "year"
                            , maybeInterval timerTimeLeft.months "month"
                            , maybeInterval timerTimeLeft.weeks "week"
                            , maybeInterval timerTimeLeft.days "day"
                            , maybeInterval timerTimeLeft.hours "hour"
                            , maybeInterval timerTimeLeft.minutes "minute"
                            , maybeInterval timerTimeLeft.seconds "second"
                            ]
                        ]


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
