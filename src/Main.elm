module Main exposing (..)

-- import Data exposing (data)

import Browser
import Colors.Opaque as Colors
import Csv.Decode
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import GraphicSVG exposing (..)
import GraphicSVG.Widget as Widget
import Html exposing (Html)
import Html.Events exposing (preventDefaultOn)
import Iso8601
import Json.Decode
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- MODEL ----


type alias Model =
    { csv : Maybe (List TimeSpanByDay)
    , hover : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { csv = Nothing, hover = False }
    , Cmd.none
    )


type alias TimeSpan a =
    { from : a
    , to : a
    }


type alias TimeSpanByDay =
    { day : Int, from : Int, to : Int }



---- UPDATE ----


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotData (List TimeSpanByDay)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick ->
            ( model
            , Select.file [ "application/zip" ] (\a -> GotFiles a [])
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles file files ->
            ( model
            , readSleepData file
                |> Task.map
                    (Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow csvDecoder
                        >> Result.withDefault []
                        >> List.map (\{ from, to } -> Maybe.map2 TimeSpan (parseDate from) (parseDate to))
                        >> Maybe.values
                        >> splitByDay
                    )
                |> Task.perform GotData
            )

        GotData content ->
            ( { model | csv = Just content }
            , Cmd.none
            )


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


csvDecoder : Csv.Decode.Decoder (TimeSpan String)
csvDecoder =
    Csv.Decode.into TimeSpan
        |> Csv.Decode.pipeline (Csv.Decode.field "from" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "to" Csv.Decode.string)


parseDate : String -> Maybe Time.Posix
parseDate =
    Iso8601.toTime >> Result.toMaybe


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


toDay : Time.Posix -> Int
toDay =
    Time.posixToMillis >> toFloat >> (\a -> a / millisPerDay) >> floor


millisPerDay =
    24 * 60 * 60 * 1000


toTimeOfDay : Time.Posix -> Int
toTimeOfDay a =
    Time.posixToMillis a - (toDay a * millisPerDay)



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [ padding 30 ]
        (Element.column [ width (fill |> maximum 600), centerX ]
            [ case model.csv of
                Nothing ->
                    uploadField model

                Just content ->
                    vis content
            ]
        )


uploadField model =
    el
        [ width fill
        , padding 30
        , Border.dashed
        , Border.width 4
        , Border.color
            (if model.hover then
                Colors.black

             else
                Colors.lightgray
            )
        , htmlAttribute (hijackOn "dragenter" (Json.Decode.succeed DragEnter))
        , htmlAttribute (hijackOn "dragover" (Json.Decode.succeed DragEnter))
        , htmlAttribute (hijackOn "dragleave" (Json.Decode.succeed DragLeave))
        , htmlAttribute (hijackOn "drop" dropDecoder)
        ]
        (el [ centerX ]
            (textButton (Material.containedButton Material.defaultPalette)
                { text = "Upload Archive"

                -- , icon = Material.Icons.favorite |> Icon.elmMaterialIcons Color
                , onPress = Just Pick
                }
            )
        )


hijackOn : String -> Json.Decode.Decoder msg -> Html.Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Json.Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


dropDecoder : Json.Decode.Decoder Msg
dropDecoder =
    Json.Decode.at [ "dataTransfer", "files" ] (Json.Decode.oneOrMore GotFiles File.decoder)


vis content =
    el []
        (Element.html
            (let
                widgetHeight =
                    toFloat (List.length (List.uniqueBy .day content) * (barHeight + barDist))

                minDay =
                    List.foldl1 min (List.map .day content) |> Maybe.withDefault 0

                toX a =
                    toFloat a / millisPerDay * widgetWidth
             in
             Widget.icon "Analysis"
                widgetWidth
                widgetHeight
                (List.map
                    (\{ day, from, to } ->
                        bar ( toX from, toFloat (day - minDay) * -(barHeight + barDist) ) (toX (to - from)) widgetHeight
                    )
                    content
                )
            )
        )


widgetWidth =
    500


barHeight =
    10


barDist =
    5


bar ( x, y ) barWidth widgetHeight =
    rect barWidth barHeight
        |> filled GraphicSVG.blue
        |> move
            ( -widgetWidth / 2 + x + barWidth / 2
            , widgetHeight / 2 + y + barHeight / 2
            )
