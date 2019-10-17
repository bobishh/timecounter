module Main exposing (main)

import Task
import Maybe
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (..)
import Url
import Url.Parser as Url exposing ((</>), Parser)
import Base64
import Time
import Event exposing(Event, fromBase64)

type alias Time = Float
type alias EncodedEvent = String
type alias Timer = {
        millisLeft: Int,
        event: Event
    }

type alias Model =
    { navKey : Nav.Key
    , page : Page
    , timer: Maybe Timer
    }

type Page
    = Index
    | CreateEvent
    | ShowEvent EncodedEvent

urlToPage : Url.Url -> Page
urlToPage url =
    -- We start with our URL
    url
        -- Send it through our URL parser (located below)
        |> Url.parse urlParser
        -- And if it didn't match any known pages, return Index
        |> Maybe.withDefault Index


urlParser : Parser (Page -> a) a
urlParser =
    -- We try to match one of the following URLs
    Url.oneOf
        -- Url.top matches root (i.e. there is nothing after 'https://example.com')
        [ Url.map Index Url.top

        -- Url.s matches URLs ending with some string, in our case '/cats'
        , Url.map CreateEvent (Url.s "new")

        -- Again, Url.s matches a string. </> matches a '/' in the URL, and Url.int matches any integer and "returns" it, so that the user page value gets the user ID
        , Url.map ShowEvent (Url.s "event" </> Url.string)
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
    | UrlChange Url.Url
    | Tick Time.Posix
    | MaybeSetTimer Time.Posix

decTimer : Maybe Timer -> Maybe Timer
decTimer maybe_timer =
    case maybe_timer of
        Nothing -> Nothing
        Just timer ->
            Just { timer | millisLeft = (timer.millisLeft - 1000) }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MaybeSetTimer newTime ->
            case model.page of
                ShowEvent encodedEvent -> (model, Cmd.none)-- { model | timer = maybeBuildTimer newTime encodedEvent }
                _ -> (model, Cmd.none)

        Tick newTime ->
            case model.timer of
                Nothing -> (model, Cmd.none)
                Just value -> ({ model | timer = decTimer model.timer }, Cmd.none )

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
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Timecount"
    , body =
        [ h1 [] [ text "Timecount" ]
        , case model.page of
            Index ->
                text "Hello at timecount"

            CreateEvent ->
                text "Create event:"

            ShowEvent encodedEvent ->
                case fromBase64(encodedEvent) of
                    Ok event -> text <| Event.toString(event)
                    Err _ -> text "Can't decode event data, check url"
        ]
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timer of
        Just timer ->
            Time.every 1000 Tick
        Nothing ->
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
