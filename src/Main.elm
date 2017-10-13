module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import HttpBuilder exposing (..)
import Http exposing (expectJson)
import Json.Decode as JD
import Navigation exposing (Location, program)
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra exposing (replace)
import UrlParser exposing (Parser, map, oneOf, parseHash, stringParam, top, (<?>))


type Msg
    = ReceivePostIds Int (List Int) (Maybe String)
    | UrlChange Location


type alias Post =
    { id : Int
    , title : String
    , content : String
    , createdAt : String
    , url : String
    }


type alias PostList =
    { site : String
    , totalPosts : Int
    , perPage : Int
    , nextPage : Maybe String
    , posts : Dict Int (WebData Post)
    }


postList : PostList
postList =
    { site = "andrewspics.wordpress.com"
    , totalPosts = 0
    , perPage = 100
    , nextPage = Nothing
    , posts = Dict.empty
    }


type Route
    = NoSiteGiven
    | Site String
    | SiteNotFound


type alias Model =
    { postList : PostList
    , route : Route
    }


model : Model
model =
    { postList = postList
    , route = Site postList.site
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
    let
        route =
            parseRoute location

        nextPostList =
            case route of
                Site site ->
                    { postList | site = site }

                _ ->
                    postList

        startFetching =
            case route of
                Site _ ->
                    fetchPostIds nextPostList

                _ ->
                    Cmd.none
    in
        ( { model
            | postList = nextPostList
            , route = route
          }
        , startFetching
        )


parseRoute : Location -> Route
parseRoute =
    parseHash routeParser >> Maybe.withDefault SiteNotFound


routeParser : Parser (Route -> a) a
routeParser =
    let
        defaultSite =
            Site postList.site

        siteRoute s =
            s
                |> Maybe.map
                    (\s ->
                        if String.isEmpty s then
                            NoSiteGiven
                        else
                            Site s
                    )
                |> Maybe.withDefault defaultSite
    in
        oneOf
            [ map siteRoute (top <?> stringParam "site")
            , map defaultSite top
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "Message" msg) of
        ReceivePostIds total ids nextPage ->
            let
                posts =
                    ids
                        |> List.map (\id -> ( id, Loading ))
                        |> Dict.fromList

                oldPostList =
                    model.postList

                {-
                   the API returns fewer and fewer results as we page
                   so here we just want to keep the first result
                -}
                newTotal =
                    if oldPostList.totalPosts == 0 then
                        total
                    else
                        oldPostList.totalPosts

                postList =
                    { oldPostList
                        | totalPosts = newTotal
                        , nextPage = nextPage
                        , posts = Dict.union posts oldPostList.posts
                    }

                fetchNextPage =
                    nextPage
                        |> Maybe.map (always <| fetchPostIds postList)
                        |> Maybe.withDefault Cmd.none
            in
                ( { model | postList = postList }, fetchNextPage )

        UrlChange location ->
            ( { model | route = parseRoute location }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.route of
        NoSiteGiven ->
            div [] [ text "Add a site in the URL query string: e.g. '?site=design.blog'" ]

        Site site ->
            div [] []

        SiteNotFound ->
            div [] []


fetchPostIds : PostList -> Cmd Msg
fetchPostIds postList =
    let
        url =
            "https://public-api.wordpress.com/rest/v1.2/sites/" ++ postList.site ++ "/posts"

        fixPageHandle =
            replace "=" "%3d" >> replace "&" "%26"

        decoder =
            JD.map3
                ReceivePostIds
                (JD.field "found" JD.int)
                (JD.field "posts" (JD.list (JD.field "ID" JD.int)))
                (JD.at [ "meta", "next_page" ] JD.string |> JD.maybe)

        toMsg result =
            case result of
                Ok msg ->
                    msg

                Err _ ->
                    ReceivePostIds 0 [] Nothing

        pageHandle =
            postList.nextPage
                |> Maybe.map (\handle -> [ ( "page_handle", handle ) ])
                |> Maybe.withDefault []

        queryParams =
            [ ( "number", toString postList.perPage )
            , ( "fields", "ID,date" )
            ]
                |> List.append pageHandle
    in
        get url
            |> withQueryParams queryParams
            |> withExpect (Http.expectJson decoder)
            |> send toMsg
