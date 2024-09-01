module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (getAt)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Http


-- MODEL

type alias Question =
    { category : Category
    , prompt : String
    , trueAnswer : String
    }

type Category
    = GreekLetterLowercaseToName

type Msg
    = StartQuiz
    | AnswerSelected String
    | NextQuestion
    | ConfirmCorrectness Bool
    | LoadQuestions (Result Http.Error (List Question))

type alias Model =
    { questions : List Question
    , currentQuestionIndex : Int
    , userAnswers : List String
    , score : Int
    , quizStarted : Bool
    , quizFinished : Bool
    , answerInput : String
    , showCorrectnessCheck : Bool
    , correctAnswer : Maybe String
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( { questions = []
      , currentQuestionIndex = 0
      , userAnswers = []
      , score = 0
      , quizStarted = False
      , quizFinished = False
      , answerInput = ""
      , showCorrectnessCheck = False
      , correctAnswer = Nothing
      }
    , loadQuestions
    )


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StartQuiz ->
            ({ model | quizStarted = True, quizFinished = False, currentQuestionIndex = 0, userAnswers = [], score = 0, answerInput = "", showCorrectnessCheck = False, correctAnswer = Nothing }, Cmd.none)

        AnswerSelected answer ->
            ({ model | answerInput = answer }, Cmd.none)

        NextQuestion ->
            let
                currentQuestion = List.Extra.getAt model.currentQuestionIndex model.questions
            in
            case currentQuestion of
                Just q ->
                    ({ model | showCorrectnessCheck = True, correctAnswer = Just q.trueAnswer }, Cmd.none)
                Nothing ->
                    (model, Cmd.none)

        ConfirmCorrectness correct ->
            let
                updatedScore = if correct then model.score + 1 else model.score
                newModel =
                    if model.currentQuestionIndex < List.length model.questions - 1 then
                        { model | currentQuestionIndex = model.currentQuestionIndex + 1, answerInput = "", showCorrectnessCheck = False, correctAnswer = Nothing, score = updatedScore }
                    else
                        { model | quizStarted = False, quizFinished = True, score = updatedScore }
            in
            (newModel, Cmd.none)

        LoadQuestions (Ok questions) ->
            ({ model | questions = questions }, Cmd.none)

        LoadQuestions (Err _) ->
            -- Handle error, e.g., by showing a message to the user
            (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ if model.quizStarted then
            case List.Extra.getAt model.currentQuestionIndex model.questions of
                Just question ->
                    div []
                        [ div [] [ text question.prompt ]
                        , if model.showCorrectnessCheck then
                            div []
                                [ div [] [ text ("Correct answer: " ++ Maybe.withDefault "" model.correctAnswer) ]
                                , div [] [ text ("Your answer: " ++ model.answerInput) ]
                                , button [ onClick (ConfirmCorrectness True) ] [ text "I got it right" ]
                                , button [ onClick (ConfirmCorrectness False) ] [ text "I got it wrong" ]
                                ]
                          else
                            div []
                                [ input [ type_ "text", placeholder "Your answer", value model.answerInput, onInput AnswerSelected ] []
                                , button [ onClick NextQuestion ] [ text "Submit" ]
                                ]
                        ]
                Nothing ->
                    text "Loading question..."
          else
            div []
                [ if model.quizFinished then
                    div []
                        [ text "Quiz Finished! "
                        , div [] [ text ("Your score: " ++ String.fromInt model.score ++ "/" ++ String.fromInt (List.length model.questions)) ]
                        , button [ onClick StartQuiz ] [ text "Restart Quiz" ]
                        ]
                  else
                    button [ onClick StartQuiz ] [ text "Start Quiz" ]
                ]
        ]


-- LOADING QUESTIONS FROM JSON

loadQuestions : Cmd Msg
loadQuestions =
    Http.get
        { url = "/questions.json"
        , expect = Http.expectJson LoadQuestions questionsDecoder
        }

questionsDecoder : Decode.Decoder (List Question)
questionsDecoder =
    Decode.list questionDecoder

questionDecoder : Decode.Decoder Question
questionDecoder =
    Decode.succeed Question
        |> required "category" (Decode.string |> Decode.andThen categoryDecoder)
        |> required "prompt" Decode.string
        |> required "trueAnswer" Decode.string

categoryDecoder : String -> Decode.Decoder Category
categoryDecoder str =
    case str of
        "GreekLetterLowercaseToName" ->
            Decode.succeed GreekLetterLowercaseToName
        _ ->
            Decode.fail ("Unknown category: " ++ str)


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
