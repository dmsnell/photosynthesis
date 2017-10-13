module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, img, node, text)
import Html.Attributes exposing (class, property, src)
import HttpBuilder exposing (..)
import Http exposing (expectJson)
import Json.Decode as JD
import Json.Encode as JE
import Maybe.Extra exposing (isJust)
import Navigation exposing (Location, program)
import RemoteData exposing (RemoteData(..), WebData)
import UrlParser exposing (Parser, map, oneOf, parseHash, stringParam, top, (<?>))


type Msg
    = ReceivePostIds Int (List Int) (Maybe String)
    | ReceivePosts (List Post)
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
    | Site String Int
    | SiteNotFound


type alias Model =
    { perPage : Int
    , postList : PostList
    , route : Route
    }


model : Model
model =
    { perPage = 20
    , postList = postList
    , route = Site postList.site 0
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
                Site site _ ->
                    { postList | site = site }

                _ ->
                    postList

        startFetching =
            case route of
                Site _ _ ->
                    [ fetchPostIds nextPostList
                    , fetchPosts model.perPage nextPostList
                    ]
                        |> Cmd.batch

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
            Site postList.site 0

        siteRoute s =
            s
                |> Maybe.map
                    (\s ->
                        if String.isEmpty s then
                            NoSiteGiven
                        else
                            Site s 0
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
            in
                ( { model | postList = postList }, Cmd.none )

        ReceivePosts posts ->
            let
                oldPostList =
                    model.postList

                newPosts =
                    posts
                        |> List.map (\post -> ( post.id, Success post ))
                        |> Dict.fromList
                        |> (flip Dict.union) oldPostList.posts

                newPostList =
                    ({ oldPostList | posts = newPosts })
            in
                ( { model | postList = newPostList }, Cmd.none )

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

                Site site page ->
                    let
                        es =
                            Maybe.withDefault ""

                        posts =
                            Dict.values model.postList.posts
                                |> List.filter
                                    (\post ->
                                        case post of
                                            Success { imageUrl } ->
                                                isJust imageUrl && not (String.endsWith ".mov" <| es imageUrl)

                                            _ ->
                                                True
                                    )
                                |> List.reverse
                                |> List.take ((page + 1) * model.perPage)

                        post remotePost =
                            case remotePost of
                                Success { id, title, content, imageUrl } ->
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
                                            [ img
                                                [ class "primary"
                                                , src <| es imageUrl
                                                ]
                                                []
                                            , div
                                                [ class excerptContainerClasses ]
                                                [ div
                                                    [ class excerptClasses
                                                    , rawHtml content
                                                    ]
                                                    []
                                                ]
                                            ]

                                _ ->
                                    div [] [ text "Loading…" ]
                    in
                        div []
                            [ text "Posts"
                            , posts
                                |> List.map post
                                |> div [ class "post-list" ]
                            ]

                SiteNotFound ->
                    div [] []
    in
        div []
            [ node "style"
                [ property "textContent" <| JE.string style
                , property "type" <| JE.string "text/css"
                ]
                []
            , body
            ]


style : String
style =
    """
body {
    background-color: black;
}

.post-list img {
    width: 100%;
    height: auto;
    max-height: 120vh;
    object-fit: contain;
}

.post-list .post {

}

.post-list .post .excerpt-container {
    margin-bottom: 128px;
    margin-top: 128px;
    padding-bottom: 24px;
    padding-top: 24px;
    background-color: white;
}

.post-list .post .excerpt-container.empty {
    min-height: 30vh;
    background-color: #111;
}

.post-list .post .excerpt {
    width: 23em;
    margin: auto;
    line-height: 180%;
    font-size: 108%;
}

.post-list .post .excerpt.short {
    width: 40em;
    text-align: center;
}

.post-list .post .excerpt.single-line {
    width: 15em;
}
"""


fetchPostIds : PostList -> Cmd Msg
fetchPostIds postList =
    let
        url =
            "https://public-api.wordpress.com/rest/v1.2/sites/" ++ postList.site ++ "/posts"

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


fetchPosts : Int -> PostList -> Cmd Msg
fetchPosts perPage postList =
    let
        url =
            "https://public-api.wordpress.com/rest/v1.2/sites/" ++ postList.site ++ "/posts"

        attributes =
            JD.keyValuePairs (JD.field "URL" JD.string)
                |> JD.map (List.head >> Maybe.map Tuple.second)

        decoder =
            JD.map
                ReceivePosts
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

        toMsg result =
            case result of
                Ok msg ->
                    msg

                Err _ ->
                    ReceivePosts []

        queryParams =
            [ ( "number", toString perPage )
            , ( "fields", "ID,date,title,excerpt,URL,attachments" )
            ]
    in
        get url
            |> withQueryParams queryParams
            |> withExpect (Http.expectJson decoder)
            |> send toMsg


rawHtml : String -> Html.Attribute Msg
rawHtml =
    property "innerHTML" << JE.string
