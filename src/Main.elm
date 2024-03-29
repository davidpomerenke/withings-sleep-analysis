module Main exposing (..)

-- import Data exposing (Stage(..), data)

import Browser
import Color exposing (..)
import Colors.Opaque as Colors
import Csv.Decode
import Dict exposing (Dict)
import Element as El exposing (..)
import Element.Border as Border
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Html.Events exposing (preventDefaultOn)
import Iso8601
import Json.Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Path
import Result.Extra as Result
import Shape
import Task exposing (Task)
import Time
import TypedSvg as Svg exposing (..)
import TypedSvg.Attributes as Svg exposing (..)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types as Svg exposing (..)
import Widget exposing (..)
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
    { csv : Result String (List TimespanByDay)
    , hover : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { csv = Err "Please upload something."
      , hover = False
      }
    , Cmd.none
    )


type alias Timespan a =
    { from : a
    , to : a
    , stage : Stage
    }


type alias TimespanByDay =
    { day : Day
    , from : TimeOfDay
    , to : TimeOfDay
    , stage : Stage
    }


type alias Day =
    Int


type alias TimeOfDay =
    Int


type alias UnparsedRawData =
    { start : String, duration : String, value : String }


type alias ParsedRawData =
    { start : Time.Posix, duration : List Int, value : List Stage }


type Stage
    = Awake
    | Light
    | Deep
    | Rem_



---- UPDATE ----


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotData (Result String (List TimespanByDay))
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick ->
            ( model
            , Select.files [ "application/zip" ] GotFiles
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles file [] ->
            ( model
            , readSleepData file
                |> Task.map
                    (Result.map
                        (Csv.Decode.decodeCsv Csv.Decode.FieldNamesFromFirstRow csvDecoder
                            >> Result.withDefault []
                            >> List.map parseRawData
                            >> Maybe.combine
                            >> Result.fromMaybe "Error parsing CSV file. Sorry."
                            >> Result.map (List.concatMap toTimespan >> splitByDay)
                        )
                        >> Result.join
                    )
                |> Task.perform GotData
            )

        GotFiles file _ ->
            ( { model | csv = Err "Please upload only the Zip file.", hover = False }
            , Cmd.none
            )

        GotData (Ok content) ->
            ( { model | csv = Ok content }
            , Cmd.none
            )

        GotData (Err e) ->
            ( { model | csv = Err e, hover = False }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


readSleepData : File -> Task Never (Result String String)
readSleepData file =
    if File.mime file == "application/zip" then
        File.toBytes file
            |> Task.map
                (Zip.fromBytes
                    >> Maybe.map (Zip.getEntry "raw_bed_sleep-state.csv")
                    >> Maybe.join
                    >> Maybe.map
                        (Zip.Entry.toString
                            >> Result.mapError (\_ -> "Error with extracting the content from the Zip entry. Sorry.")
                        )
                    >> Maybe.withDefault (Err "Error reading the Zip archive. Sorry.")
                )

    else
        Task.succeed (Err "Please upload the Zip file.")


csvDecoder : Csv.Decode.Decoder UnparsedRawData
csvDecoder =
    Csv.Decode.into UnparsedRawData
        |> Csv.Decode.pipeline (Csv.Decode.field "start" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "duration" Csv.Decode.string)
        |> Csv.Decode.pipeline (Csv.Decode.field "value" Csv.Decode.string)


parseRawData : UnparsedRawData -> Maybe ParsedRawData
parseRawData { start, duration, value } =
    Maybe.map3 ParsedRawData
        (parseDate start)
        (Json.Decode.decodeString (Json.Decode.list Json.Decode.int) duration
            |> Result.toMaybe
        )
        (Json.Decode.decodeString (Json.Decode.list Json.Decode.int) value
            |> Result.toMaybe
            |> Maybe.map (List.map toStage >> Maybe.combine)
            |> Maybe.join
        )


parseDate : String -> Maybe Time.Posix
parseDate =
    Iso8601.toTime >> Result.toMaybe


toTimespan : ParsedRawData -> List (Timespan Time.Posix)
toTimespan { start, duration, value } =
    List.foldl
        (\( duration_, value_ ) acc ->
            case acc of
                [] ->
                    [ { from = start
                      , to = add (1000 * duration_) start
                      , stage = value_
                      }
                    ]

                prev :: rest ->
                    if prev.stage == value_ then
                        { prev | to = add (1000 * duration_) prev.to } :: rest

                    else
                        { from = prev.to
                        , to = add (1000 * duration_) prev.to
                        , stage = value_
                        }
                            :: prev
                            :: rest
        )
        []
        (List.zip duration value)


toStage : Int -> Maybe Stage
toStage i =
    case i of
        0 ->
            Just Awake

        1 ->
            Just Light

        2 ->
            Just Deep

        3 ->
            Just Rem_

        _ ->
            Nothing


add : Int -> Time.Posix -> Time.Posix
add millis_ time_ =
    Time.millisToPosix (millis_ + Time.posixToMillis time_)


splitByDay : List (Timespan Time.Posix) -> List TimespanByDay
splitByDay =
    List.concatMap
        (\{ from, to, stage } ->
            if toDay from == toDay to then
                [ TimespanByDay (toDay from) (toTimeOfDay from) (toTimeOfDay to) stage ]

            else
                [ TimespanByDay (toDay from) (toTimeOfDay from) millisPerDay stage
                , TimespanByDay (toDay to) 0 (toTimeOfDay to) stage
                ]
        )


toDay : Time.Posix -> Int
toDay =
    Time.posixToMillis >> toFloat >> (\a -> a / millisPerDay) >> floor


millisPerDay =
    24 * millisPerHour


millisPerHour =
    60 * millisPerMinute


millisPerMinute =
    60 * 1000


toTimeOfDay : Time.Posix -> Int
toTimeOfDay a =
    Time.posixToMillis a - (toDay a * millisPerDay)



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [ paddingXY 0 30 ]
        (El.column [ El.width (El.fill |> maximum widgetWidth), centerX ]
            [ case model.csv of
                Err err ->
                    uploadField err model

                Ok content ->
                    vis content
            ]
        )


uploadField err model =
    El.column [ El.width El.fill, El.spacing 30, padding 30 ]
        [ el
            [ El.width El.fill
            , padding 30
            , Border.dashed
            , Border.width 4
            , Border.color
                (if model.hover then
                    Colors.purple

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
                    { text = "Upload Zip Archive"
                    , onPress = Just Pick
                    }
                )
            )
        , paragraph [] [ El.text err ]
        ]


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
    el [ El.width El.fill ]
        (El.html
            (let
                widgetHeight =
                    toFloat (List.length (days content) * (barHeight + barDist))

                minDay =
                    List.foldl1 Basics.min (List.map .day content) |> Maybe.withDefault 0
             in
             svg
                [ Svg.width (Percent 100)
                , Svg.height (Svg.px widgetHeight)
                , viewBox 0 0 widgetWidth widgetHeight
                ]
                (List.map
                    (\{ day, from, to, stage } ->
                        bar (toColor stage)
                            ( toX 0 from
                            , toFloat (day - minDay) * (barHeight + barDist)
                            )
                            (toX 0 (to - from))
                            widgetHeight
                    )
                    content
                    ++ [ rightCurve (always True) black minDay widgetHeight content
                       , rightCurve ((/=) Awake) black minDay widgetHeight content
                       , rightCurve ((==) Awake) (toColor Awake) minDay widgetHeight content
                       , rightCurve ((==) Light) (toColor Light) minDay widgetHeight content
                       , rightCurve ((==) Deep) (toColor Deep) minDay widgetHeight content
                       , rightCurve ((==) Rem_) (toColor Rem_) minDay widgetHeight content
                       , curve black
                            Shape.monotoneInXCurve
                            (List.map
                                (\( hour, frequency ) ->
                                    Just
                                        ( toX 0 (hour * millisPerHour)
                                        , widgetHeight - frequency / toFloat (List.length (days content)) * widgetHeight
                                        )
                                )
                                (Dict.toList (hourlyHistogram (always True) content))
                            )
                       ]
                )
            )
        )


toX i a =
    let
        shares =
            [ 5, 1 ]
    in
    (List.sum (List.take i shares)
        + (toFloat a
            / millisPerDay
            * (List.getAt i shares |> Maybe.withDefault 1)
          )
    )
        * widgetWidth
        / List.sum shares


widgetWidth =
    800


barHeight =
    20


barDist =
    10


bar : Color.Color -> ( Float, Float ) -> Float -> Float -> Svg Msg
bar color ( x_, y_ ) barWidth widgetHeight =
    rect
        [ x (Svg.px x_)
        , y (Svg.px (widgetHeight - y_))
        , Svg.width (Svg.px barWidth)
        , Svg.height (Svg.px barHeight)
        , Svg.fill (Paint color)
        ]
        []


toColor : Stage -> Color.Color
toColor stage =
    case stage of
        Awake ->
            gray

        Light ->
            lightBlue

        Deep ->
            blue

        Rem_ ->
            purple


rightCurve f color minDay widgetHeight content =
    curve color
        Shape.monotoneInYCurve
        (List.map
            (\( day, value ) ->
                Just
                    ( toX 1 value + 2
                    , widgetHeight
                        - toFloat (day - minDay)
                        * (barHeight + barDist)
                        + 0.5
                        * barHeight
                    )
            )
            (Dict.toList (dailyAggregate f content))
        )


curve color type_ points =
    Path.element
        (Shape.line type_ points)
        [ noFill, strokeWidth (Svg.px 2), stroke (Paint color) ]


dailyAggregate : (Stage -> Bool) -> List TimespanByDay -> Dict Day Int
dailyAggregate f data =
    List.foldl
        (\a d ->
            Dict.update a.day
                (\v ->
                    Just
                        (Maybe.withDefault 0 v
                            + (if f a.stage then
                                a.to - a.from

                               else
                                0
                              )
                        )
                )
                d
        )
        -- populate empty days in between with zeros:
        (days data
            |> List.map (\a -> ( a, 0 ))
            |> Dict.fromList
        )
        data


days : List TimespanByDay -> List Int
days data =
    Maybe.map2 List.range
        (List.foldl1 Basics.min (List.map .day data))
        (List.foldl1 Basics.max (List.map .day data))
        |> Maybe.withDefault []


hourlyHistogram : (Stage -> Bool) -> List TimespanByDay -> Dict Int Float
hourlyHistogram f data =
    List.foldl
        (\a d ->
            let
                updateWholeHours d_ =
                    List.foldl
                        (\hour ->
                            Dict.update hour (Maybe.map ((+) 1))
                        )
                        d_
                        (List.range
                            (ceiling (toFloat a.from / millisPerHour))
                            (floor (toFloat a.to / millisPerHour) - 1)
                        )

                updatePartialHour x =
                    Dict.update
                        (floor (toFloat x / millisPerHour))
                        (Maybe.map
                            ((+)
                                ((toFloat x / millisPerHour)
                                    - toFloat (floor (toFloat x / millisPerHour))
                                )
                            )
                        )
            in
            d
                |> updateWholeHours
                -- |> updatePartialHour a.from
                -- |> updatePartialHour a.to
        )
        (List.range 0 23
            |> List.map (\a -> ( a, 0 ))
            |> Dict.fromList
        )
        data


distances =
    [ 1, 2, 4, 7, 2 * 7, 4 * 7, 7 * 7, 13 * 7, 26 * 7, 52 * 7 ]
