module Main exposing (..)

import Html exposing (Html, div, text)
import Navigation exposing (Location, program)
import UrlParser exposing (Parser, map, oneOf, parseHash, stringParam, top, (<?>))


type Msg
    = UrlChange Location


type LoadingStatus
    = Idle
    | Loading


type Route
    = Boot
    | NoSiteGiven
    | Site String
    | SiteNotFound


type alias Model =
    { networkStatus : LoadingStatus
    , route : Route
    }


model : Model
model =
    { networkStatus = Idle
    , route = Boot
    }


main : Program Never Model Msg
main =
    program UrlChange
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


init : Location -> ( Model, Cmd Msg )
init location =
    ( { model | route = parseRoute location }, Cmd.none )


parseRoute : Location -> Route
parseRoute =
    parseHash routeParser >> Maybe.withDefault SiteNotFound


routeParser : Parser (Route -> a) a
routeParser =
    let
        siteRoute s =
            s
                |> Maybe.map
                    (\s ->
                        if String.isEmpty s then
                            NoSiteGiven
                        else
                            Site s
                    )
                |> Maybe.withDefault Boot
    in
        oneOf
            [ map siteRoute (top <?> stringParam "site")
            , map Boot top
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange location ->
            ( { model | route = parseRoute location }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.route of
        Boot ->
            div [] []

        NoSiteGiven ->
            div [] [ text "Add a site in the URL query string: e.g. '?site=design.blog'" ]

        Site site ->
            div [] []

        SiteNotFound ->
            div [] []
