module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attributes
import Maybe
import String.Extra
import Task
import Time exposing (utc)
import Timer exposing (..)
import Url
import Url.Parser as Parser


yellow =
    rgb255 239 233 174


black =
    rgb255 0 0 0


white =
    rgb255 255 255 255


type alias Model =
    { navKey : Nav.Key
    , page : Page
    , now : Maybe Time.Posix
    , formError : Maybe String
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
    }


urlToPage : Url.Url -> Page
urlToPage url =
    if String.startsWith "/t/" url.path then
        url.path
            |> String.dropLeft 3
            |> Url.percentDecode
            |> Maybe.andThen base64StringToTimer
            |> Maybe.map ShowTimer
            |> Maybe.withDefault Dunno

    else
        Parser.parse
            (Parser.oneOf
                [ Parser.map Index Parser.top
                , Parser.map (CreateTimer templateTimerForm) (Parser.s "new")
                ]
            )
            url
            |> Maybe.withDefault Dunno


tickOnce : Cmd Msg
tickOnce =
    Task.perform Tick Time.now


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { navKey = key
      , page = urlToPage url
      , now = Nothing
      , formError = Nothing
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
    "/t/" ++ toBase64 timer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuildTimer timerForm ->
            case buildTimerFromForm timerForm of
                Ok timer ->
                    ( { model | page = ShowTimer timer }, Nav.pushUrl model.navKey (buildUrlString timer) )

                Err error ->
                    ( { model | formError = Just error }, Cmd.none )

        UpdateTimerForm timerForm ->
            case model.page of
                CreateTimer _ ->
                    ( { model | page = CreateTimer timerForm, formError = Nothing }, Cmd.none )

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
            ( { model | page = urlToPage url, formError = Nothing }
            , tickOnce
            )


inputStyles label =
    [ Font.color black, width (fillPortion 1), spacing 8, paddingXY 8 12, htmlAttribute (Attributes.attribute "aria-label" label) ]


edges =
    { top = 15
    , right = 0
    , bottom = 15
    , left = 0
    }


placeholder placeholderText =
    Just (Input.placeholder [] (el [ alignLeft ] (text placeholderText)))


renderTimerForm : Maybe String -> TimerForm -> Element Msg
renderTimerForm error timerForm =
    el [ width fill ]
        (column [ centerX, width fill, spacing 16, Font.size 24 ]
            [ el [ Region.heading 1, Font.size 32 ] (text "Create a timer")
            , row [ width fill ]
                [ Input.text (inputStyles "Title")
                    { label =
                        Input.labelAbove [ Font.size 14 ] (text "Title")
                    , placeholder = placeholder "Name it"
                    , text = timerForm.title
                    , onChange = \title -> UpdateTimerForm { timerForm | title = title }
                    }
                ]
            , column [ width fill ]
                [ row [ Font.size 14, paddingEach edges ] [ text "Date and time · UTC (time optional)" ]
                , row [ width fill, spacing 12 ]
                    [ Input.text (inputStyles "Year")
                        { label =
                            Input.labelAbove [ Font.size 14 ] (text "Year")
                        , placeholder = placeholder "YYYY"
                        , text = timerForm.year
                        , onChange = \year -> UpdateTimerForm { timerForm | year = year }
                        }
                    , Input.text (inputStyles "Month")
                        { label =
                            Input.labelAbove [ Font.size 14 ] (text "Month")
                        , placeholder = placeholder "MM"
                        , text = timerForm.month
                        , onChange = \month -> UpdateTimerForm { timerForm | month = month }
                        }
                    , Input.text (inputStyles "Day")
                        { label =
                            Input.labelAbove [ Font.size 14 ] (text "Day")
                        , placeholder = placeholder "DD"
                        , text = timerForm.day
                        , onChange = \day -> UpdateTimerForm { timerForm | day = day }
                        }
                    ]
                ]
            , row [ width fill, spacing 12 ]
                [ Input.text (inputStyles "Hours")
                    { label =
                        Input.labelAbove [ Font.size 14 ] (text "Hours")
                    , placeholder = placeholder "HH"
                    , text = timerForm.hours
                    , onChange = \hours -> UpdateTimerForm { timerForm | hours = hours }
                    }
                , Input.text (inputStyles "Minutes")
                    { label =
                        Input.labelAbove [ Font.size 14 ] (text "Minutes")
                    , placeholder = placeholder "MM"
                    , text = timerForm.minutes
                    , onChange = \minutes -> UpdateTimerForm { timerForm | minutes = minutes }
                    }
                , Input.text (inputStyles "Seconds")
                    { label =
                        Input.labelAbove [ Font.size 14 ] (text "Seconds")
                    , placeholder = placeholder "SS"
                    , text = timerForm.seconds
                    , onChange = \seconds -> UpdateTimerForm { timerForm | seconds = seconds }
                    }
                ]
            , paragraph [ Font.size 16, Font.color (rgb255 140 30 30), htmlAttribute (Attributes.attribute "role" "alert") ]
                [ text (Maybe.withDefault "" error) ]
            , row [ width fill, spacing 12 ]
                [ Input.button
                    [ Background.color black
                    , Font.color white
                    , paddingXY 32 16
                    , Border.rounded 3
                    , width fill
                    ]
                    { onPress = Just (BuildTimer timerForm)
                    , label = Element.text "Create timer"
                    }
                ]
            ]
        )


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
            paragraph [ Font.size 28, htmlAttribute (Attributes.attribute "role" "alert") ] [ text "Invalid timer link." ]

        Index ->
            column [ spacing 28, Font.size 28 ]
                [ paragraph [] [ text "This site helps you to track something that is important to you. " ]
                , paragraph [] [ text "Be it last time you had sex, or a day you quit smoking. " ]
                , paragraph []
                    [ link
                        [ Font.heavy, Font.underline ]
                        { url = "/new", label = text "Create a timer." }
                    ]
                ]

        CreateTimer timerForm ->
            renderTimerForm model.formError timerForm

        ShowTimer timer ->
            case model.now of
                Nothing ->
                    row [] [ text "Initializing ..." ]

                Just time ->
                    let
                        timerTimeLeft =
                            timeLeft timer time utc
                    in
                    column [ centerX, width fill, spacing 28, Font.size 28 ]
                        [ paragraph [ Region.heading 1 ] [ text timer.title ]
                        , paragraph [ Font.size 16 ]
                            [ text
                                (if timer.posixSeconds * 1000 > Time.posixToMillis time then
                                    "Time remaining"

                                 else
                                    "Time elapsed"
                                )
                            ]
                        , wrappedRow [ spacing 8 ]
                            [ if timerTimeLeft == TimeLeft 0 0 0 0 0 0 0 then
                                text "0 seconds"

                              else
                                text ""
                            , maybeInterval timerTimeLeft.years "year"
                            , maybeInterval timerTimeLeft.months "month"
                            , maybeInterval timerTimeLeft.weeks "week"
                            , maybeInterval timerTimeLeft.days "day"
                            , maybeInterval timerTimeLeft.hours "hour"
                            , maybeInterval timerTimeLeft.minutes "minute"
                            , maybeInterval timerTimeLeft.seconds "second"
                            ]
                        , paragraph [ Font.size 14 ] [ text "Share this timer: copy the link from your address bar." ]
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
                [ Font.typeface "monospace"
                , Font.sansSerif
                ]
            ]
            (column [ width (fill |> maximum 800), centerX, paddingXY 24 40, spacing 40, Region.mainContent ]
                [ link [ Font.size 16 ] { url = "/", label = text "Counting seconds" }
                , renderPage model
                , link [ Font.size 16, Font.underline ] { url = "/new", label = text "New timer" }
                ]
            )
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
