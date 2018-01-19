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
                [ ( "height", "80%" )
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
                , ( "height", "80%" )
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
printQIFFile indentation qif =
    String.repeat indentation " "
        ++ "{ type_ =  \""
        ++ qif.type_
        ++ "\"\n"
        ++ String.repeat indentation " "
        ++ ", transactions =\n"
        ++ printList (indentation + 4) printTransaction qif.transactions
        ++ "\n"
        ++ String.repeat indentation " "
        ++ "}"


printList : Int -> (Int -> a -> String) -> List a -> String
printList indentation printer list =
    String.repeat indentation " "
        ++ "[ "
        ++ (list
                |> List.map (String.trim << indent indentation << printer 2)
                |> String.join ("\n" ++ String.repeat indentation " " ++ ", ")
           )
        ++ "\n"
        ++ String.repeat indentation " "
        ++ "]"


printTransaction : Int -> Quicken.Transaction -> String
printTransaction indentation transaction =
    String.repeat indentation " "
        ++ "{ date = "
        ++ toString transaction.date
        ++ "\n"
        ++ String.repeat indentation " "
        ++ ", description = "
        ++ toString transaction.description
        ++ "\n"
        ++ String.repeat indentation " "
        ++ ", amount = "
        ++ toString transaction.amount
        ++ "\n"
        ++ String.repeat indentation " "
        ++ "}"


indent : Int -> String -> String
indent indentation str =
    str
        |> String.lines
        |> String.join ("\n" ++ String.repeat indentation " ")
