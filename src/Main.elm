module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text, table, tr, td, th)
import Html.Attributes exposing (type_, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Http
import Random
import Debug
import Task


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
    , selectionChancePerTry : Float
    , lastFocusedIndex : Maybe Int
    , randomSeed : Random.Seed
    }

type Msg
    = StartQuiz
    | AnswerUpdated String
    | SubmitAnswer
    | ConfirmCorrectness Bool
    | LoadQuestions (Result Http.Error (List Question))
    -- | PickQuestion
    | EndQuiz
    | PickQuestion Float


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
      , selectionChancePerTry = 0.1 -- Define your threshold here
      , lastFocusedIndex = Nothing
      , randomSeed = seed
      }
    , loadQuestions
    )


-- UPDATE

-- Usage in the `update` function
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        StartQuiz ->
            ({ model | quizStarted = True, quizFinished = False }, Cmd.batch [loadQuestions, pickQuestionCmd model])

        AnswerUpdated answer ->
            ({ model | answerInput = answer }, Cmd.none)

        SubmitAnswer ->
            case model.currentQuestion of
                Just q ->
                    let
                        answer = { question = q, userAnswer = model.answerInput, isCorrect = q.trueAnswer == model.answerInput }
                        updatedAnswers = model.answeredQuestions ++ [answer]
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
            ({ model | remainingQuestions = [] }, Cmd.none)

        -- PickQuestion ->
            -- let
                -- isEmpty = List.isEmpty model.remainingQuestions
            -- in
            -- if isEmpty then
                -- -- No more questions left, mark the quiz as finished
                -- ({ model | quizFinished = True, quizStarted = False, currentQuestion = Nothing }, Cmd.none)
            -- else
                -- (model, Random.generate PickQuestion (Random.float 0 1))

        PickQuestion randomValue ->
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

                (maybeFocusedQuestion, remaining) = popIndex lastIndex model.remainingQuestions
                isSelected = randomValue < model.selectionChancePerTry
            in
            case Debug.log ("randomValue : " ++ String.fromFloat randomValue ++ ", isSelected : " ++ (if isSelected then "True" else "False") ++ ", remaining : [" ++ String.join "," (List.map (\q -> q.prompt) remaining) ++ "], " ++ "maybeFocusedQuestion") maybeFocusedQuestion of
                Just focusedQuestion ->
                    if isSelected then
                        ({ model
                           | currentQuestion = Just focusedQuestion
                           , remainingQuestions = remaining
                           , lastFocusedIndex = Just lastIndex
                         }, Cmd.none)
                    else
                        ({ model | lastFocusedIndex = Just nextIndex }, pickQuestionCmd model)

                Nothing ->
                    -- Fallback in case of any unexpected errors
                    Debug.log "case maybeFocusedQuestion of Nothing" (model, endQuizCmd model)

        EndQuiz ->
            ({ model | quizFinished = True, quizStarted = False, currentQuestion = Nothing }, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ if model.quizStarted then
            case model.currentQuestion of
                Just question ->
                    div []
                        [ div [] [ text question.prompt ]
                        , input [ type_ "text", placeholder "Your answer", value model.answerInput, onInput AnswerUpdated ] []
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
    case stringToCategory str of
        Just category ->
            Decode.succeed category
        Nothing ->
            Decode.fail ("Unknown category: " ++ str)

stringToCategory : String -> Maybe Category
stringToCategory str =
    case str of
        "GreekLetterLowercaseToName" -> Just GreekLetterLowercaseToName
        _ -> Nothing

categoryToString : Category -> String
categoryToString category =
    case category of
        GreekLetterLowercaseToName -> "GreekLetterLowercaseToName"


-- HELPERS

-- Split the list manually without using `List.splitAt`
popIndex : Int -> List a -> (Maybe a, List a)
popIndex index list =
    let
        -- Recursive function to traverse the list
        popHelper currentIndex remaining acc =
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
                        popHelper (currentIndex + 1) xs (x :: acc)
    in
    popHelper 0 list []

pickQuestionCmd : Model -> Cmd Msg
pickQuestionCmd model =
    Cmd.batch
        [ Random.generate PickQuestion (Random.float 0 1)
        ]

endQuizCmd : Model -> Cmd Msg
endQuizCmd model = Task.succeed () |> Task.perform (\_ -> EndQuiz)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
