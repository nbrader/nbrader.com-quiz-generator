module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (getAt)

type alias Question =
    { category : Category
    , prompt : String
    , trueAnswer : String
    }

type Category
    = Reverse Category
    | GreekLetterLowercaseToName
    -- And so on for each category

type Msg
    = StartQuiz
    | AnswerSelected String
    | NextQuestion
    -- Other messages for handling user actions

type alias Model =
    { questions : List Question
    , currentQuestionIndex : Int
    , userAnswers : List String
    , score : Int
    , quizStarted : Bool
    , quizFinished : Bool
    }


it comes from
init : (Model, Cmd Msg)
init =
    ( { questions = [ Question GreekLetterLowercaseToName "What is α?" "alpha"
                    , Question GreekLetterLowercaseToName "What is β?" "beta"
                    ] -- Make sure to close the list with ]
      , currentQuestionIndex = 0
      , userAnswers = []
      , score = 0
      , quizStarted = False
      , quizFinished = False
      }
    , Cmd.none
    )



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StartQuiz ->
            ({ model | quizStarted = True, quizFinished = False, currentQuestionIndex = 0, userAnswers = [], score = 0 }, Cmd.none)

        AnswerSelected answer ->
            let
                updatedAnswers = model.userAnswers ++ [answer]
                currentQuestion = List.Extra.getAt model.currentQuestionIndex model.questions
                currentScore = case currentQuestion of
                    Just q -> if q.trueAnswer == answer then model.score + 1 else model.score
                    Nothing -> model.score
            in
            ({ model | userAnswers = updatedAnswers, score = currentScore }, Cmd.none)

        NextQuestion ->
            if model.currentQuestionIndex < List.length model.questions - 1 then
                ({ model | currentQuestionIndex = model.currentQuestionIndex + 1 }, Cmd.none)
            else
                -- Mark quiz as finished
                ({ model | quizStarted = False, quizFinished = True }, Cmd.none)



view : Model -> Html Msg
view model =
    div []
        [ if model.quizStarted then
            case List.Extra.getAt model.currentQuestionIndex model.questions of
                Just question ->
                    div []
                        [ div [] [ text question.prompt ]
                        , input [ type_ "text", placeholder "Your answer", onInput AnswerSelected ] []
                        , button [ onClick NextQuestion ] [ text "Submit" ]
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

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }