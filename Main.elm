module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (value, style)
import Html.Events exposing (onInput)
import Parser exposing (Parser, Problem(..), (|.), (|=), symbol, spaces)

type alias Cron =
    { minutes : Maybe (Some Minutes)
    , hours : Maybe (Some Hours)
    , days : Maybe (Some Days)
    , months : Maybe (Some Months)
    , daysOfWeek : Maybe (Some DaysOfWeek)
    }

type Some tag = Some Val (List Val)

type Val
    = Range (Int, Int)
    | RangeWithSteps (Int, Int) Int
    | Steps Int
    | Single Int

type Minutes = Minutes
type Hours = Hours
type Days = Days
type Months = Months
type DaysOfWeek = DaysOfWeek

cron : Parser Cron
cron =
    Parser.succeed Cron
        |= ifAny (some (0, 59))
        |. spaces
        |= ifAny (some (0, 23))
        |. spaces
        |= ifAny (some (1, 31))
        |. spaces
        |= ifAny (some (1, 12))
        |. spaces
        |= ifAny (some (0, 7))
        |. Parser.end

ifAny : Parser a -> Parser (Maybe a)
ifAny p =
    Parser.oneOf
        [ Parser.succeed Just |= p
        , Parser.succeed Nothing |. symbol "*"
        ]

some : (Int, Int) -> Parser (Some a)
some bounds =
    Parser.succeed Some
        |= val bounds
        |= Parser.loop [] (many <| val bounds)

many : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
many p vs =
    Parser.oneOf
        [ Parser.succeed (\v -> Parser.Loop (v :: vs))
            |. symbol ","
            |= p
        , Parser.succeed ()
            |> Parser.map (\() -> Parser.Done (List.reverse vs))
        ]

val : (Int, Int) -> Parser Val
val (f, t) =
    Parser.oneOf
        [ steps
        , intBetween f t
            |> Parser.andThen
               (\x -> Parser.oneOf
                    [ range (x, t)
                    -- ^ future left bound should be greater
                    -- than current right bound "x"
                    , Parser.succeed <| Single x
                    ]
               )
        ]

steps : Parser Val
steps =
    Parser.succeed Steps
        |. symbol "*/"
        |= Parser.int

range : (Int, Int) -> Parser Val
range (right, to) =
    Parser.succeed
        (\left mbStep ->
             case mbStep of
                 Just s -> RangeWithSteps (right, left) s
                 Nothing -> Range (right, left)
        )
        |. symbol "-"
        |= intBetween (right + 1) to
        -- try to get -^ a non-empty range
        |= Parser.oneOf
           [ Parser.succeed Just
                |. symbol "/"
                |= Parser.int
           , Parser.succeed Nothing
           ]

intBetween : Int -> Int -> Parser Int
intBetween from to =
    Parser.int
        |> Parser.andThen
           (\x ->
                if x >= from && x <= to
                then Parser.succeed x
                else Parser.problem "Bad range!"
           )

-- GUI

example : String
example = "0,3-10/2,20-45/5,*/4 */3,*/5 * * *"

type alias Model
    = { input : String
      , result : Result (List Parser.DeadEnd) Cron
      }

main : Program () Model String
main =
    Browser.sandbox
        { init = fromInput example
        , view = view
        , update = always << fromInput
        }

fromInput : String -> Model
fromInput s = { input = s, result = Parser.run cron s }

view : Model -> Html String
view m =
    Html.div []
        [ Html.input [ style "width" "99%", onInput identity, value m.input ] []
        , case m.result of
              Err ps -> Html.ul [] <| List.map (viewDeadEnd m.input) ps
              Ok x ->
                  Html.pre []
                      [ Html.code []
                            [ Html.text <| Debug.toString x ]]
        ]

viewDeadEnd : String -> Parser.DeadEnd -> Html a
viewDeadEnd input {col, problem} =
    Html.li []
        [ Html.text <| Debug.toString problem
        , Html.pre []
            [ Html.text <| String.concat
                  [ String.append input "\n"
                  , String.append (String.repeat (col - 1) " ") "^"
                  ]
            ]
        ]
