module plt.main

open FParsec
open System

type ASTNode =
    | FieldsNode of string * Position
    | ActionNode of string * Position
    | ErrorNode of string * Position
    | CommandNode of ASTNode list
    | EmptyNode

let stringContent = many1Chars (noneOf ",[]{} \t\r\n")
let stringParser = spaces >>. stringContent .>> spaces
let commaSeparatedStrings = sepBy stringParser (pstring ",")
let commaSeparatedStringsBetweenBrackets = between (spaces .>> pstring "[") (pstring "]" .>> spaces) (commaSeparatedStrings)
let singleStringAsList = stringParser |>> fun s -> [s]
let stringOrCommaSeparatedStringsBetweenBrackets =
    choice [
        singleStringAsList
        commaSeparatedStringsBetweenBrackets
    ]
let fieldsParser =
    spaces >>. pipe2 stringOrCommaSeparatedStringsBetweenBrackets (pstring "," >>. stringParser) (fun ys x -> (ys, x)) .>> spaces

let hexadecimalDigit = satisfy (fun c -> isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
let hexColor =
    pstring "#" >>. manyChars hexadecimalDigit >>= fun hexDigits ->
    match hexDigits.Length with
    | 3 | 6 -> preturn ("#" + hexDigits)
    | _ -> fail "Invalid hex color"
let colorParser =
    choice [
        pstring "red"; pstring "green"; pstring "blue"; pstring "yellow"; pstring "orange"; pstring "black";
        hexColor
    ]

let widthParser = many1Chars digit .>> pstring "px" |>> (fun digits -> digits + "px")

let drawStyleParser =
    choice [
        pstring "solid"; 
        pstring "dashed"; 
        pstring "dotted"
    ]

let styleParser =
    pipe3 (widthParser .>> spaces) (drawStyleParser .>> spaces) (colorParser .>> spaces) (fun width style color -> (width, [(style, color)]))

let styleColorPairParser =
    pipe2 (drawStyleParser .>> spaces) colorParser (fun style color -> (style, color))
let multiStyleParser =
    pipe2 
        (widthParser .>> spaces) 
        (between (pchar '[') (pchar ']') 
            (sepBy styleColorPairParser (pstring "," .>> spaces))
        )
        (fun width styleColorPairs -> (width, styleColorPairs))

let actionParser = pipe2 (many1Chars (noneOf " \t\r\n") .>> spaces) (choice [attempt styleParser; multiStyleParser]) (fun name (width, styleColorPairs) -> (name, width, styleColorPairs))


let openBrace = pstring "{" .>> spaces
let closeBrace = spaces >>. pstring "}"

let actionStringParser =
    pipe2
        (between openBrace closeBrace (manyCharsTill anyChar (lookAhead closeBrace)))
        getPosition
        (fun s pos -> ActionNode (s, pos))

let fieldsStringParser =
    pipe2
        getPosition
        (manyCharsTill anyChar (lookAhead openBrace))
        (fun pos s -> FieldsNode (s, pos))

let errorParser =
    pipe2 getPosition (anyChar) (fun pos chars ->
        if String.IsNullOrWhiteSpace(String.Concat(chars)) then
            EmptyNode
        else
            ErrorNode(String.Concat(chars), pos))
let commandParser =
    pipe2 fieldsStringParser (opt actionStringParser)
        (fun p a -> CommandNode (p :: Option.toList a))

let combinedParser =
    choice [
        attempt commandParser;
        errorParser
    ]

let programStringParser =
    many combinedParser

let validateNodeWithParser parser (node: ASTNode) =
    match node with
    | ActionNode(s, pos)
    | FieldsNode(s, pos) ->
        match run parser s with
        | Success(_, _, _) -> node
        | Failure(msg, _, _) ->
            let errorDetail = msg.Trim().Split('\n') |> Array.last
            let stringDetail = msg.Trim().Split('\n') |> Array.tail |> Array.head

            let nodeType =
                match node with
                | ActionNode _ -> "Action"
                | FieldsNode _ -> "Fields"
                | _ -> "Unknown"

            ErrorNode(sprintf "%s - %s - %s" nodeType stringDetail errorDetail, pos)
    | _ -> node

let actionValidatorParser =
    actionParser

let fieldsValidatorParser =
    fieldsParser

let rec validateASTNode node =
    match node with
    | ActionNode(_, _) -> validateNodeWithParser actionValidatorParser node
    | FieldsNode(_, _) -> validateNodeWithParser fieldsValidatorParser node
    | ErrorNode(_, _) -> node
    | CommandNode(nodes) ->
        let validatedNodes = List.map validateASTNode nodes
        CommandNode(validatedNodes)
    | EmptyNode -> node

let validateAST ast =
    List.map validateASTNode ast

let placeholderPosition = Position("placeholder", 0, 0, 0)

let runAndValidate input =
    match run programStringParser input with
    | Success(result, _, _) ->
        validateAST result
    | Failure(errorMsg, _, _) ->
        [ErrorNode ("Parsing error: " + errorMsg, placeholderPosition)]

let rec printASTNode node =
    match node with
    | FieldsNode(s, _) -> printfn "Fields: %s" s
    | ActionNode(s, _) -> printfn "Action: %s" s
    | ErrorNode(e, p) -> printfn "Error: %s at %A" e p
    | CommandNode(nodes) ->
        printfn "Command:"
        nodes |> List.iter printASTNode
    | EmptyNode -> ignore()

[<EntryPoint>]
let main argv =
    let program : string = argv.[0]
    let validatedAST = runAndValidate program
    validatedAST |> List.iter printASTNode
    0
