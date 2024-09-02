module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, table, tr, td, th)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Http
import Random


-- MODEL

type alias Question =
    { category : Category
    , prompt : String
    , trueAnswer : String
    }

type alias AnsweredQuestion =
    { question : Question
    , userAnswer : String
    , isCorrect : Bool
    }

type Category
    = GreekLetterLowercaseToName

type alias Model =
    { remainingQuestions : List Question
    , currentQuestion : Maybe Question
    , answeredQuestions : List AnsweredQuestion
    , score : Int
    , quizStarted : Bool
    , quizFinished : Bool
    , answerInput : String
    , randomThreshold : Float
    , lastFocusedIndex : Maybe Int
    , randomSeed : Random.Seed
    }

type Msg
    = StartQuiz
    | AnswerSelected String
    | SubmitAnswer
    | ConfirmCorrectness Bool
    | LoadQuestions (Result Http.Error (List Question))
    | PickQuestion
    | EndQuiz
    | RandomGenerated Float


-- INITIALIZATION

init : () -> (Model, Cmd Msg)
init _ =
    let
        seed = Random.initialSeed 42
    in
    ( { remainingQuestions = []
      , currentQuestion = Nothing
      , answeredQuestions = []
      , score = 0
      , quizStarted = False
      , quizFinished = False
      , answerInput = ""
      , randomThreshold = 0.5 -- Define your threshold here
      , lastFocusedIndex = Nothing
      , randomSeed = seed
      }
    , loadQuestions
    )


-- UPDATE

-- Split the list manually without using `List.splitAt`
splitAtIndex : Int -> List a -> (Maybe a, List a)
splitAtIndex index list =
    let
        -- Recursive function to traverse the list
        splitHelper currentIndex remaining acc =
            case remaining of
                [] ->
                    -- If we reach the end, return Nothing and the original list
                    (Nothing, List.reverse acc)

                x :: xs ->
                    if currentIndex == index then
                        -- If the current index matches the desired index, return the element and the rest of the list
                        (Just x, List.reverse acc ++ xs)
                    else
                        -- Otherwise, keep traversing
                        splitHelper (currentIndex + 1) xs (x :: acc)
    in
    splitHelper 0 list []

-- Usage in the `update` function
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StartQuiz ->
            if List.isEmpty model.remainingQuestions then
                -- No questions to load, stop the quiz
                (model, Cmd.none)
            else
                -- Start the quiz by picking the first question
                ({ model | quizStarted = True, quizFinished = False }, pickQuestionCmd model)

        AnswerSelected answer ->
            ({ model | answerInput = answer }, Cmd.none)

        SubmitAnswer ->
            case model.currentQuestion of
                Just q ->
                    let
                        answered = { question = q, userAnswer = model.answerInput, isCorrect = q.trueAnswer == model.answerInput }
                        updatedAnswers = model.answeredQuestions ++ [answered]
                    in
                    ({ model | answeredQuestions = updatedAnswers, answerInput = "", currentQuestion = Nothing }, pickQuestionCmd model)

                Nothing ->
                    (model, Cmd.none)

        ConfirmCorrectness _ ->
            (model, Cmd.none)

        LoadQuestions (Ok questions) ->
            ({ model | remainingQuestions = questions }, Cmd.none)

        LoadQuestions (Err _) ->
            -- Handle error, e.g., by showing a message to the user
            (model, Cmd.none)

        PickQuestion ->
            let
                isEmpty = List.isEmpty model.remainingQuestions
            in
            if isEmpty then
                ({ model | quizFinished = True, quizStarted = False }, Cmd.none)
            else
                (model, Random.generate RandomGenerated (Random.float 0 1))

        RandomGenerated value ->
            let
                lastIndex = List.length model.remainingQuestions - 1
                nextIndex =
                    case model.lastFocusedIndex of
                        Just i ->
                            if i >= lastIndex then
                                0
                            else
                                i + 1

                        Nothing ->
                            0

                (maybeFocusedQuestion, remaining) = splitAtIndex nextIndex model.remainingQuestions
                isSelected = value < model.randomThreshold
            in
            case maybeFocusedQuestion of
                Just focusedQuestion ->
                    if isSelected then
                        ({ model
                           | currentQuestion = Just focusedQuestion
                           , remainingQuestions = remaining
                           , lastFocusedIndex = Just nextIndex
                         }, Cmd.none)
                    else
                        ({ model | lastFocusedIndex = Just nextIndex }, pickQuestionCmd model)

                Nothing ->
                    -- Fallback in case of any unexpected errors
                    (model, Cmd.none)

        EndQuiz ->
            ({ model | quizFinished = True, quizStarted = False }, Cmd.none)





-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ if model.quizStarted then
            case model.currentQuestion of
                Just question ->
                    div []
                        [ div [] [ text question.prompt ]
                        , input [ type_ "text", placeholder "Your answer", value model.answerInput, onInput AnswerSelected ] []
                        , button [ onClick SubmitAnswer ] [ text "Submit" ]
                        , button [ onClick EndQuiz ] [ text "End Quiz" ]
                        ]
                Nothing ->
                    text "Loading question..."
          else if model.quizFinished then
            div []
                [ text "Quiz Finished! "
                , div [] [ text ("Your score: " ++ String.fromInt model.score ++ "/" ++ String.fromInt (List.length model.answeredQuestions)) ]
                , viewResults model.answeredQuestions
                , button [ onClick StartQuiz ] [ text "Restart Quiz" ]
                ]
          else
            button [ onClick StartQuiz ] [ text "Start Quiz" ]
        ]


viewResults : List AnsweredQuestion -> Html msg
viewResults answeredQuestions =
    table []
        (List.map viewResultRow answeredQuestions)


viewResultRow : AnsweredQuestion -> Html msg
viewResultRow answeredQuestion =
    tr []
        [ td [] [ text answeredQuestion.question.prompt ]
        , td [] [ text answeredQuestion.userAnswer ]
        , td [] [ text answeredQuestion.question.trueAnswer ]
        , td [] [ text (if answeredQuestion.isCorrect then "Correct" else "Incorrect") ]
        ]


-- LOADING QUESTIONS FROM JSON

loadQuestions : Cmd Msg
loadQuestions =
    Http.get
        { url = "questions.json"
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


-- HELPERS

pickQuestionCmd : Model -> Cmd Msg
pickQuestionCmd model =
    Cmd.batch
        [ Random.generate RandomGenerated (Random.float 0 1)
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
