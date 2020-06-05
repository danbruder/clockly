module Pages.Top exposing (Flags, Model, Msg, page)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page exposing (Document, Page)


type alias Flags =
    ()


type alias Model =
    { entries : List Entry
    , inputs : Dict String String
    }


type Msg
    = TypedInInput String String
    | SubmittedNewEntryForm


type alias Entry =
    { description : String
    , hours : String
    }


page : Page Flags Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { entries = []
      , inputs = Dict.empty
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TypedInInput key value ->
            if key == "hours" then
                let
                    newValue =
                        filterStringToBeValidFloat value
                in
                ( { model | inputs = Dict.insert key newValue model.inputs }, Cmd.none )

            else
                ( { model | inputs = Dict.insert key value model.inputs }, Cmd.none )

        SubmittedNewEntryForm ->
            let
                description =
                    Dict.get "description" model.inputs |> Maybe.withDefault ""

                hours =
                    Dict.get "hours" model.inputs |> Maybe.withDefault ""
            in
            if description /= "" && hours /= "" then
                ( { model
                    | entries =
                        { description = description
                        , hours = hours
                        }
                            :: model.entries
                    , inputs = Dict.empty
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Document Msg
view model =
    { title = "Top"
    , body =
        [ div [ class "bg-gray-100 flex justify-center h-full" ]
            [ div []
                [ div [ class "max-w-6xl w-full mx-8 mt-8 mb-0 " ]
                    [ h1 [ class "text-3xl text-gray-600 font-bold" ] [ text "Clockly" ]
                    , p [ class "text-gray-800" ] [ text "The best time tracking ever™" ]
                    ]
                , div [ class "bg-white rounded-lg shadow max-w-3xl w-full my-8 p-8" ]
                    [ viewInputBox model
                    , div [ class "mt-5" ] [ viewEntries model ]
                    ]
                ]
            ]
        ]
    }


viewInputBox : Model -> Html Msg
viewInputBox model =
    div [ class "w-full" ]
        [ label [ class "block text-sm font-medium leading-5 text-gray-700" ]
            [ text "New Entry" ]
        , Html.form [ onSubmit SubmittedNewEntryForm, class "mt-1 relative rounded shadow-sm grid grid-cols-12 gap-2" ]
            [ input
                [ class "form-input block sm:text-sm sm:leading-5 rounded col-span-9"
                , id "email"
                , placeholder "Description"
                , onInput (TypedInInput "description")
                , value (Dict.get "description" model.inputs |> Maybe.withDefault "")
                ]
                []
            , input
                [ class "form-input block sm:text-sm sm:leading-5 col-span-2 rounded"
                , id "email"
                , placeholder "0.0"
                , onInput (TypedInInput "hours")
                , value (Dict.get "hours" model.inputs |> Maybe.withDefault "")
                ]
                []
            , input [ class "block text-xs text-green-500 justify-center flex items-center px-3 border rounded bg-green-100 uppercase font-bold border-green-300 border-l col-span-1 hover:text-green-800 hover:bg-green-300 transition ease-in duration-200", type_ "submit", value "Save" ]
                []
            ]
        ]


viewEntries model =
    if List.length model.entries > 0 then
        div [ class "flex flex-col" ]
            [ h1 [ class "text-lg my-2" ] [ text "Entries" ]
            , div [ class "-my-2 py-2 overflow-x-auto sm:-mx-6 sm:px-6 lg:-mx-8 lg:px-8" ]
                [ div [ class "align-middle inline-block min-w-full overflow-hidden sm:rounded-lg border border-gray-200" ]
                    [ table [ class "min-w-full" ]
                        [ thead []
                            [ tr []
                                [ th [ class "px-6 py-3 border-b border-gray-200 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" ]
                                    [ text "Description" ]
                                , th [ class "px-6 py-3 border-b border-gray-200 bg-gray-50" ]
                                    [ text "Hours" ]
                                , th [ class "px-6 py-3 border-b border-gray-200 bg-gray-50" ]
                                    []
                                ]
                            ]
                        , tbody [ class "bg-white" ] <| List.map viewEntry model.entries
                        ]
                    ]
                ]
            ]

    else
        div [ class "text-gray-400 flex justify-center items-center m-20" ]
            [ text "No entries yet"
            ]


viewEntry entry =
    tr []
        [ td [ class "px-6 py-4 whitespace-no-wrap border-b border-gray-200" ]
            [ div [ class "flex items-center" ]
                [ div [ class "flex-shrink-0 h-10 w-10 font-bold text-lg rounded-full text-gray-800 items-center flex justify-center" ]
                    [ text entry.hours
                    ]
                , div [ class "ml-4" ]
                    [ div [ class "text-sm leading-5 font-medium text-gray-900" ]
                        [ text entry.description ]
                    ]
                ]
            ]
        , td [ class "px-6 py-4 whitespace-no-wrap text-right border-b border-gray-200 text-sm leading-5 font-medium" ]
            [ text entry.hours ]
        ]


{-| Float INPUT PROCESSING
-}
filterStringToBeValidFloat : String -> String
filterStringToBeValidFloat val =
    if String.contains "." val then
        let
            filt =
                \char -> Char.isDigit char || char == '.'

            filteredString =
                val |> String.filter filt

            start =
                filteredString |> String.split "." |> List.take 1

            end =
                filteredString |> String.split "." |> List.drop 1 |> String.join ""
        in
        [ start, [ "." ], [ end ] ]
            |> List.concat
            |> String.join ""

    else
        let
            filt =
                \char -> Char.isDigit char || char == '.'
        in
        val |> String.filter filt
