module Main exposing (..)

import Html exposing (Html, div)


type Msg
    = NoOp


type alias Model =
    {}


model : Model
model =
    {}


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    div [] []
