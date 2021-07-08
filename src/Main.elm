module Main exposing (..)

import Browser
import Colors.Opaque exposing (..)
import Csv.Decode as Decode
-- import Data exposing (data)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import GraphicSVG exposing (..)
import GraphicSVG.Widget as Widget
import Html exposing (Html)
import Iso8601
import List.Extra as List
import Material.Icons
import Maybe.Extra as Maybe
import Result.Extra as Result
import Task exposing (Task)
import Time
import Widget exposing (..)
import Widget.Icon as Icon
import Widget.Material as Material
import Zip
import Zip.Entry



---- MODEL ----


type alias Model =
    { csv : Maybe (List TimeSpanByDay)
    }


init : ( Model, Cmd Msg )
init =
    ( { csv = Nothing }, Cmd.none )


type alias TimeSpan a =
    { from : a
    , to : a
    }


type alias TimeSpanByDay =
    { day : Int, from : Int, to : Int }



---- UPDATE ----


type Msg
    = CsvRequested
    | CsvSelected File
    | GotZip (List TimeSpanByDay)


readSleepData : File -> Task Never String
readSleepData =
    File.toBytes
        >> Task.map
            (Zip.fromBytes
                >> Maybe.map (Zip.getEntry "sleep.csv")
                >> Maybe.join
                >> Maybe.map
                    (Zip.Entry.toString
                        >> Result.withDefault ""
                    )
                >> Maybe.withDefault ""
            )


parseDate : String -> Maybe Time.Posix
parseDate =
    Iso8601.toTime >> Result.toMaybe


millisPerDay =
    24 * 60 * 60 * 1000


toDay : Time.Posix -> Int
toDay =
    Time.posixToMillis >> toFloat >> (\a -> a / millisPerDay) >> floor


toTimeOfDay : Time.Posix -> Int
toTimeOfDay a =
    Time.posixToMillis a - (toDay a * millisPerDay)


splitByDay : List (TimeSpan Time.Posix) -> List TimeSpanByDay
splitByDay =
    List.concatMap
        (\{ from, to } ->
            if toDay from == toDay to then
                [ TimeSpanByDay (toDay from) (toTimeOfDay from) (toTimeOfDay to) ]

            else
                [ TimeSpanByDay (toDay from) (toTimeOfDay from) millisPerDay
                , TimeSpanByDay (toDay to) 0 (toTimeOfDay to)
                ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvRequested ->
            ( model
            , Select.file [ "application/zip" ] CsvSelected
            )

        CsvSelected file ->
            ( model
            , readSleepData file
                |> Task.map
                    (Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder
                        >> Result.withDefault []
                        >> List.map (\{ from, to } -> Maybe.map2 TimeSpan (parseDate from) (parseDate to))
                        >> Maybe.values
                        >> splitByDay
                    )
                |> Task.perform GotZip
            )

        GotZip content ->
            ( { model | csv = Just content }
            , Cmd.none
            )


decoder : Decode.Decoder (TimeSpan String)
decoder =
    Decode.into TimeSpan
        |> Decode.pipeline (Decode.field "from" Decode.string)
        |> Decode.pipeline (Decode.field "to" Decode.string)



---- VIEW ----


view : Model -> Html Msg
view model =
    layout []
        (Element.column [ width (fill |> maximum 600), centerX ]
            [ case model.csv of
                Nothing ->
                    el [ centerX ]
                        (textButton (Material.containedButton Material.defaultPalette)
                            { text = "Upload Archive"

                            -- , icon = Material.Icons.favorite |> Icon.elmMaterialIcons Color
                            , onPress = Just CsvRequested
                            }
                        )

                Just content ->
                    paragraph []
                        [ -- Element.text (Debug.toString content),
                          el []
                            (Element.html
                                (let
                                    widgetHeight =
                                        toFloat (List.length (List.uniqueBy .day content) * (barHeight + barDist))
                                    minDay =
                                        List.foldl1 min (List.map .day content) |> Maybe.withDefault 0

                                    toX a =
                                        toFloat a / millisPerDay * width_
                                 in
                                 Widget.icon "Analysis"
                                    width_
                                    widgetHeight
                                    (List.map
                                        (\{ day, from, to } ->
                                            bar ( toX from, toFloat (day - minDay) * -(barHeight + barDist) ) (toX (to - from)) widgetHeight
                                        )
                                        content
                                    )
                                )
                            )
                        ]
            ]
        )


width_ =
    500


barHeight =
    10


barDist =
    5


bar ( x, y ) barWidth widgetHeight =
    rect barWidth barHeight
        |> filled GraphicSVG.blue
        |> move
            ( -width_ / 2 + x + barWidth / 2
            , widgetHeight / 2 + y + barHeight / 2
            )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
