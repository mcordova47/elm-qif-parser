module Example exposing (main)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Parser
import Quicken exposing (QIFFile)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { text : String
    , result : Result Parser.Error QIFFile
    }


init : Model
init =
    Model exampleFile (Quicken.parse exampleFile)


exampleFile : String
exampleFile =
    """!Type:Bank
D01/17/2018
PBought something
T-40.25
^
D01/16/2018
PBought something else
T-10.00
^
D01/16/2018
PBought some more stuff
T-20.00
^
D01/12/2018
PGot paid!
T500.00
^
"""



-- UPDATE


type Msg
    = Input String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input text ->
            Model text (Quicken.parse text)



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.style
            [ ( "display", "flex" )
            , ( "height", "100%" )
            ]
        ]
        [ Html.textarea
            [ Attributes.value model.text
            , Attributes.style
                [ ( "height", "calc(100% - 50px)" )
                , ( "flex", "1" )
                , ( "margin", "15px" )
                ]
            , Events.onInput Input
            ]
            []
        , Html.pre
            [ Attributes.style
                [ ( "flex", "1" )
                , ( "margin", "15px" )
                , ( "padding", "6px" )
                , ( "overflow", "auto" )
                , ( "height", "calc(100% - 50px)" )
                , ( "background-color", "rgb(254,254,254)" )
                , ( "border-radius", "6px" )
                , ( "border", "1px solid rgb(245,245,245)" )
                ]
            ]
            [ Html.text (printResult model.result) ]
        ]


printResult : Result Parser.Error QIFFile -> String
printResult result =
    case result of
        Err error ->
            toString result

        Ok qif ->
            "Ok\n" ++ printQIFFile 4 qif


printQIFFile : Int -> QIFFile -> String
printQIFFile =
    printRecord
        [ field "type_" (any .type_)
        , field "transactions"
            (\n record ->
                newLine
                    (printList (n + 4) printTransaction)
                    record.transactions
            )
        ]


printTransaction : Int -> Quicken.Transaction -> String
printTransaction =
    printRecord
        [ field "date" (any .date)
        , field "description" (any .description)
        , field "amount" (any .amount)
        ]


type alias Field record =
    { name : String
    , printer : Int -> record -> String
    }


field : String -> (Int -> record -> String) -> Field record
field name printer =
    { name = name
    , printer = printer
    }


any : (record -> a) -> Int -> record -> String
any accessor _ =
    toString << accessor


printRecord : List (Field record) -> Int -> record -> String
printRecord fields spaces record =
    String.repeat spaces " "
        ++ "{ "
        ++ (fields
                |> List.map (printField record spaces)
                |> String.join ("\n" ++ String.repeat spaces " " ++ ", ")
           )
        ++ "\n"
        ++ String.repeat spaces " "
        ++ "}"


newLine : (a -> String) -> a -> String
newLine printer value =
    "\n" ++ printer value


printField : record -> Int -> Field record -> String
printField record spaces { name, printer } =
    name ++ " = " ++ printer spaces record


printList : Int -> (Int -> a -> String) -> List a -> String
printList spaces printer list =
    String.repeat spaces " "
        ++ "[ "
        ++ (list
                |> List.map (String.trim << indent spaces << printer 2)
                |> String.join ("\n" ++ String.repeat spaces " " ++ ", ")
           )
        ++ "\n"
        ++ String.repeat spaces " "
        ++ "]"


indent : Int -> String -> String
indent spaces str =
    str
        |> String.lines
        |> String.join ("\n" ++ String.repeat spaces " ")
