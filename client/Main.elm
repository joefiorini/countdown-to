module Main exposing (main)

import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (..)
import Html.Styled.Events exposing (onSubmit, onInput)
import DateTimePicker
import Date exposing (Date)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = RequestCreateCountdown
    | ChangeStartDate DateTimePicker.State (Maybe Date)
    | ChangeDescription String
    | ChangeShort String
    | ChangeEndDate DateTimePicker.State (Maybe Date)


type alias Countdown =
    { startDate : Maybe Date
    , endDate : Maybe Date
    , description : Maybe String
    , short : Maybe String
    }


type alias Model =
    { template : Countdown
    , startDatePickerState : DateTimePicker.State
    , endDatePickerState : DateTimePicker.State
    , formErrors : Maybe FormErrors
    }


init : ( Model, Cmd Msg )
init =
    { template =
        { startDate = Nothing
        , endDate = Nothing
        , description = Nothing
        , short = Nothing
        }
    , startDatePickerState = DateTimePicker.initialState
    , endDatePickerState = DateTimePicker.initialState
    , formErrors = Nothing
    }
        ! [ DateTimePicker.initialCmd ChangeStartDate DateTimePicker.initialState
          , DateTimePicker.initialCmd ChangeEndDate DateTimePicker.initialState
          ]


setStartDatePickerState : DateTimePicker.State -> Model -> Model
setStartDatePickerState state model =
    { model | startDatePickerState = state }


setEndDatePickerState : DateTimePicker.State -> Model -> Model
setEndDatePickerState state model =
    { model | endDatePickerState = state }


type Validator
    = Validator Countdown FormErrors


type FieldStatus
    = NotChecked
    | Valid
    | Missing
    | BadFormat


type alias FormErrors =
    { startDate : FieldStatus
    , endDate : FieldStatus
    , description : FieldStatus
    }


type FormError
    = StartDateMissing
    | EndDateMissing
    | ShortMissing
    | DescriptionMissing


initValidator : Countdown -> Validator
initValidator countdown =
    Validator countdown { startDate = NotChecked, endDate = NotChecked, description = NotChecked }



-- fromTemplateIn : Model -> Countdown -> Model
-- fromTemplateIn model =
--     model.template


checkStartDate : Validator -> Validator
checkStartDate validator =
    case validator of
        Validator countdown result ->
            case countdown.startDate of
                Nothing ->
                    Validator countdown { result | startDate = Missing }

                _ ->
                    Validator countdown { result | startDate = Valid }


checkEndDate : Validator -> Validator
checkEndDate validator =
    case validator of
        Validator countdown result ->
            case countdown.endDate of
                Nothing ->
                    Validator countdown { result | endDate = Missing }

                _ ->
                    Validator countdown { result | endDate = Valid }


checkDescription : Validator -> Validator
checkDescription validator =
    case validator of
        Validator countdown result ->
            case countdown.description of
                Nothing ->
                    Validator countdown { result | description = Missing }

                _ ->
                    Validator countdown { result | description = Valid }


setStartDate : Maybe Date -> Countdown -> Countdown
setStartDate newDate countdown =
    { countdown | startDate = newDate }


extractErrors : Validator -> FormErrors
extractErrors result =
    case result of
        Validator _ result ->
            result


setEndDate : Maybe Date -> Countdown -> Countdown
setEndDate newDate countdown =
    { countdown | endDate = newDate }


setDescription : Maybe String -> Countdown -> Countdown
setDescription newDescription countdown =
    { countdown | description = newDescription }


setShort : Maybe String -> Countdown -> Countdown
setShort newShort countdown =
    { countdown | short = newShort }


setFieldErrors : FormErrors -> Model -> Model
setFieldErrors result model =
    { model | formErrors = Just result }


setTemplate : Countdown -> Model -> Model
setTemplate countdown model =
    { model | template = countdown }


asFieldErrorsIn : Model -> FormErrors -> Model
asFieldErrorsIn =
    flip setFieldErrors


asTemplateIn : Model -> Countdown -> Model
asTemplateIn =
    flip setTemplate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDescription description ->
            (model.template
                |> setDescription (Just description)
                |> asTemplateIn model
            )
                ! []

        ChangeStartDate datePickerState selectedDate ->
            (model.template
                |> setStartDate selectedDate
                |> asTemplateIn model
                |> setStartDatePickerState datePickerState
            )
                ! []

        ChangeEndDate datePickerState selectedDate ->
            (model.template
                |> setEndDate selectedDate
                |> asTemplateIn model
                |> setEndDatePickerState datePickerState
            )
                ! []

        ChangeShort short ->
            (model.template
                |> setShort (Just short)
                |> asTemplateIn model
            )
                ! []

        RequestCreateCountdown ->
            updateWithValidator
                (initValidator model.template
                    |> checkStartDate
                    |> checkEndDate
                    |> checkDescription
                    |> extractErrors
                    |> asFieldErrorsIn model
                )


allValid : FormErrors -> Bool
allValid { startDate, endDate, description } =
    let
        isValid value =
            case value of
                NotChecked ->
                    False

                Valid ->
                    True

                BadFormat ->
                    False

                Missing ->
                    False
    in
        List.all isValid [ startDate, endDate, description ]


updateWithValidator : Model -> ( Model, Cmd Msg )
updateWithValidator model =
    Maybe.map allValid model.formErrors
        |> Maybe.withDefault False
        |> (\valid ->
                if valid then
                    model ! []
                else
                    model ! []
           )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


label : List (Attribute msg) -> List (Html msg) -> Html msg
label =
    styled Html.label [ display block ]


input : List (Attribute msg) -> List (Html msg) -> Html msg
input =
    styled Html.input [ display block ]


view : Model -> Html Msg
view model =
    main_ []
        [ h1 [] [ text "Countdown!!!" ]
        , Html.form [ onSubmit RequestCreateCountdown ]
            [ label [ for "description" ]
                [ text "Description"
                , input
                    [ type_ "text", name "description", id "description", onInput ChangeDescription, Attr.required True ]
                    []
                ]
            , case model.formErrors of
                Just { description } ->
                    case description of
                        Missing ->
                            span [] [ text "required" ]

                        _ ->
                            text ""

                Nothing ->
                    text ""
            , label [ for "short" ]
                [ text "Custom URL Shortcode"
                , input [ type_ "text", name "short", id "short", onInput ChangeShort ] []
                ]
            , label [ for "starting-on" ]
                [ text "Starting On"
                , DateTimePicker.dateTimePicker
                    ChangeStartDate
                    [ id "starting-on", name "starting-on", Attr.required True ]
                    model.startDatePickerState
                    model.template.startDate
                ]
            , case model.formErrors of
                Just { startDate } ->
                    case startDate of
                        Missing ->
                            span [] [ text "required" ]

                        _ ->
                            text ""

                Nothing ->
                    text ""
            , label [ for "ending-on" ]
                [ text "Ending On"
                , DateTimePicker.dateTimePicker ChangeEndDate [ id "ending-on", name "ending-on" ] model.endDatePickerState model.template.endDate
                ]
            , case model.formErrors of
                Just { endDate } ->
                    case endDate of
                        Missing ->
                            span [] [ text "required" ]

                        _ ->
                            text ""

                Nothing ->
                    text ""
            , button []
                [ text "Start Countdown!" ]
            ]
        ]
