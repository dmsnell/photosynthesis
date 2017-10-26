port module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, a, div, img, node, text)
import Html.Attributes exposing (class, href, property, src)
import HttpBuilder exposing (..)
import Http exposing (expectJson)
import Json.Decode as JD
import Json.Encode as JE
import Navigation exposing (Location, program)
import Regex as RE
import String.Extra exposing (leftOf)
import UrlParser exposing (Parser, map, oneOf, parseHash, stringParam, top, (<?>))


port requestNextPage : (() -> msg) -> Sub msg


type Msg
    = NetworkError
    | NetworkRequest (RequestBuilder Msg)
    | NetworkSuccess Msg
    | ReceivePosts (Maybe String) (List Post)
    | RequestNextPage
    | UrlChange Location


type alias Post =
    { id : Int
    , title : String
    , content : String
    , createdAt : String
    , url : String
    , imageUrl : Maybe String
    }


type alias PostList =
    { site : String
    , totalPosts : Int
    , perPage : Int
    , nextPage : Maybe String
    , posts : Dict String Post
    }


postList : PostList
postList =
    { site = "andrewspics.wordpress.com"
    , totalPosts = 0
    , perPage = 20
    , nextPage = Nothing
    , posts = Dict.empty
    }


type Route
    = NoSiteGiven
    | Site String
    | SiteNotFound


type alias Model =
    { isNetworkActive : Bool
    , postList : PostList
    , route : Route
    }


model : Model
model =
    { isNetworkActive = False
    , postList = postList
    , route = Site postList.site
    }


main : Program Never Model Msg
main =
    program UrlChange
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    requestNextPage <| always RequestNextPage


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
                    Just <| fetchPosts nextPostList

                _ ->
                    Nothing

        nextModel =
            { model
                | postList = nextPostList
                , route = route
            }
    in
        startFetching
            |> Maybe.map (\m -> update m nextModel)
            |> Maybe.withDefault ( nextModel, Cmd.none )


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
    case msg of
        NetworkError ->
            ( { model | isNetworkActive = False }, Cmd.none )

        NetworkRequest request ->
            let
                toMsg response =
                    response
                        |> Result.map NetworkSuccess
                        |> Result.withDefault NetworkError
            in
                ( { model | isNetworkActive = True }, send toMsg request )

        NetworkSuccess msg ->
            update msg { model | isNetworkActive = False }

        ReceivePosts nextPage posts ->
            let
                oldPostList =
                    model.postList

                validPost { imageUrl } =
                    let
                        url =
                            imageUrl
                                |> Maybe.withDefault ""
                    in
                        [ ".jpg"
                        , ".png"
                        , ".gif"
                        , ".svg"
                        , ".webp"
                        ]
                            |> List.any ((flip String.endsWith) (leftOf "?" url))

                newPosts =
                    posts
                        |> List.filter validPost
                        |> List.map (\post -> ( post.createdAt, post ))
                        |> Dict.fromList
                        |> (flip Dict.union) oldPostList.posts

                newPostList =
                    ({ oldPostList
                        | posts = newPosts
                        , nextPage = nextPage
                     }
                    )
            in
                ( { model | postList = newPostList }, Cmd.none )

        RequestNextPage ->
            if model.isNetworkActive then
                ( model, Cmd.none )
            else
                fetchNextPage model.postList
                    |> Maybe.map (\m -> update m model)
                    |> Maybe.withDefault ( model, Cmd.none )

        UrlChange location ->
            ( { model | route = parseRoute location }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        joinClasses =
            List.filter (Tuple.second >> ((==) True))
                >> List.map Tuple.first
                >> String.join " "

        body =
            case model.route of
                NoSiteGiven ->
                    div [] [ text "Add a site in the URL query string: e.g. '?site=design.blog'" ]

                Site site ->
                    let
                        es =
                            Maybe.withDefault ""

                        posts =
                            model.postList.posts
                                |> Dict.values
                                |> List.reverse

                        post { id, title, content, imageUrl, url } =
                            let
                                excerptContainerClasses =
                                    joinClasses
                                        [ ( "excerpt-container", True )
                                        , ( "empty", String.isEmpty content )
                                        ]

                                excerptClasses =
                                    joinClasses
                                        [ ( "excerpt", True )
                                        , ( "short"
                                          , (False
                                                || String.endsWith "[&hellip;]</p>\n" content
                                                || String.endsWith "&hellip;</p>\n" content
                                            )
                                                |> not
                                          )
                                        , ( "single-line"
                                          , (String.length content > 80 && String.length content < 160 && not (String.contains "<br" content))
                                          )
                                        ]
                            in
                                div [ class "post" ]
                                    [ a [ href url ]
                                        [ img
                                            [ class "primary"
                                            , src <| RE.replace RE.All (RE.regex "quality=\\d+") (always "quality=97") <| es imageUrl
                                            ]
                                            []
                                        ]
                                    , div
                                        [ class excerptContainerClasses ]
                                        [ div
                                            [ class excerptClasses
                                            , rawHtml content
                                            ]
                                            []
                                        ]
                                    ]
                    in
                        case List.isEmpty posts of
                            True ->
                                div [ class "loading-message" ] [ text "Loading…" ]

                            False ->
                                posts
                                    |> List.map post
                                    |> div [ class "post-list" ]

                SiteNotFound ->
                    div [] []
    in
        div []
            [ networkSpinner model.isNetworkActive
            , body
            ]


fetchNextPage : PostList -> Maybe Msg
fetchNextPage postList =
    postList.nextPage
        |> Maybe.map (always <| fetchPosts postList)


fetchPosts : PostList -> Msg
fetchPosts postList =
    let
        url =
            "https://public-api.wordpress.com/rest/v1.2/sites/" ++ postList.site ++ "/posts"

        attributes =
            JD.keyValuePairs (JD.field "URL" JD.string)
                |> JD.map (List.head >> Maybe.map Tuple.second)

        decoder =
            JD.map2
                ReceivePosts
                (JD.maybe <| JD.at [ "meta", "next_page" ] JD.string)
                (JD.field "posts"
                    (JD.list
                        (JD.map6
                            Post
                            (JD.field "ID" JD.int)
                            (JD.field "title" JD.string)
                            (JD.field "excerpt" JD.string)
                            (JD.field "date" JD.string)
                            (JD.field "URL" JD.string)
                            (JD.field "attachments" attributes)
                        )
                    )
                )

        pageHandle =
            postList.nextPage
                |> Maybe.map (\s -> [ ( "page_handle", s ) ])
                |> Maybe.withDefault []

        queryParams =
            [ ( "number", toString postList.perPage )
            , ( "fields", "ID,date,title,excerpt,URL,attachments" )
            ]
                |> List.append pageHandle
    in
        get url
            |> withQueryParams queryParams
            |> withExpect (Http.expectJson decoder)
            |> NetworkRequest


rawHtml : String -> Html.Attribute Msg
rawHtml =
    property "innerHTML" << JE.string


networkSpinner : Bool -> Html msg
networkSpinner isActive =
    if isActive then
        networkSpinnerHtml
    else
        text ""



{-
   thanks http://tobiasahlin.com/spinkit/
-}


networkSpinnerHtml : Html msg
networkSpinnerHtml =
    div [ class "network-spinner" ]
        [ div [ class "network-spinner__1" ] []
        , div [ class "network-spinner__2" ] []
        , div [ class "network-spinner__3" ] []
        , div [ class "network-spinner__4" ] []
        , div [ class "network-spinner__5" ] []
        ]
