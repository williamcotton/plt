﻿module plt.main

open FParsec
open System
open System.Text.RegularExpressions

type ASTNode =
    | FieldsNode of string * Position * Position
    | ActionNode of string * Position * Position
    | ErrorNode of string * Position * Position
    | CommandNode of ASTNode list
    | EmptyNode

let placeholderPosition = Position("placeholder", 0, 0, 0)

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
    choice
        [ attempt (pstring "red" .>> notFollowedBy anyChar)
          attempt (pstring "green" .>> notFollowedBy anyChar)
          attempt (pstring "blue" .>> notFollowedBy anyChar)
          attempt (pstring "yellow" .>> notFollowedBy anyChar)
          attempt (pstring "orange" .>> notFollowedBy anyChar)
          attempt (pstring "black" .>> notFollowedBy anyChar)
          attempt hexColor ]
    <?> "color (e.g., red, green, #123456)"

let widthParser = many1Chars digit .>> pstring "px" |>> (fun digits -> digits + "px")

let drawStyleParser =
    choice [
        pstring "solid"; 
        pstring "dashed"; 
        pstring "dotted"
    ]

let styleParser =
    pipe2 (drawStyleParser .>> spaces) (colorParser .>> spaces) (fun style color -> (style, color))

let styleColorPairParser =
    pipe2 (drawStyleParser .>> spaces) colorParser (fun style color -> (style, color))

let multiStyleParser =
    between (pchar '[') (pchar ']') (sepBy styleColorPairParser (pstring "," .>> spaces))

let actionParser = 
    pipe3 (many1Chars (noneOf " \t\r\n") .>> spaces) (widthParser .>> spaces) (choice [attempt styleParser |>> fun sc -> [sc] |> ignore; multiStyleParser]) (fun name width styleColorPairs -> (name, width, styleColorPairs))
let openBrace = pstring "{" .>> spaces
let closeBrace = spaces >>. pstring "}"

let fieldsStringParser =
    pipe3
        getPosition
        (manyCharsTill anyChar (lookAhead openBrace))
        getPosition
        (fun startPos content endPos -> FieldsNode (content, startPos, endPos))

let actionStringParser =
    pipe3
        getPosition
        (between openBrace closeBrace (manyCharsTill anyChar (lookAhead closeBrace)))
        getPosition
        (fun startPos content endPos -> ActionNode (content, startPos, endPos))

let errorParser =
    pipe3 
        getPosition 
        (manyCharsTill anyChar (lookAhead closeBrace))
        getPosition 
        (fun startPos chars endPos ->
            if String.IsNullOrWhiteSpace(chars) then 
                EmptyNode
            else 
                ErrorNode(chars, startPos, endPos)) // Use startPos to mark the beginning of the error

let commandParser =
    pipe2 fieldsStringParser (opt actionStringParser)
        (fun p a -> CommandNode (p :: Option.toList a))

let combinedParser =
    choice [
        attempt commandParser   
        errorParser
    ] .>> spaces

let programStringParser =
    many combinedParser

let validateNodeWithParser parser (node: ASTNode) =
    let parseNode content (startPos : Position) (endPos : Position) nodeType =
        match run parser content with
        | Success(_, _, _) -> node
        | Failure(msg, e, _) ->
            let adjustedPos =
                let backtrackPattern = @"Error in Ln: (\d+) Col: (\d+)"
                let matches = Regex.Matches(msg, backtrackPattern)
                if matches.Count > 1 then
                    let lastMatch = matches.[matches.Count - 1]
                    let line = Int64.Parse(lastMatch.Groups.[1].Value)
                    let col = Int64.Parse(lastMatch.Groups.[2].Value)
                    Position(startPos.StreamName, startPos.Index + col - 1L, line + startPos.Line - 1L, startPos.Column + col + 1L)
                else
                    Position(startPos.StreamName, startPos.Index + int64 e.Position.Column - 1L, startPos.Line, startPos.Column + int64 e.Position.Column - 1L)

            let errorDetail = msg.Trim().Split('\n') |> Array.last
            ErrorNode(sprintf "%s - %s - %s" nodeType content errorDetail, adjustedPos, endPos)

    match node with
    | ActionNode(content, startPos, endPos) -> parseNode content startPos endPos "Action"
    | FieldsNode(content, startPos, endPos) -> parseNode content startPos endPos "Fields"
    | _ -> node


let actionValidatorParser =
    actionParser

let fieldsValidatorParser =
    fieldsParser

let rec validateASTNode node =
    match node with
    | ActionNode(_, _, _) -> validateNodeWithParser actionValidatorParser node
    | FieldsNode(_, _, _) -> validateNodeWithParser fieldsValidatorParser node
    | ErrorNode(_, _, _) -> node
    | CommandNode(nodes) ->
        let validatedNodes = List.map validateASTNode nodes
        CommandNode(validatedNodes)
    | EmptyNode -> node

let validateAST ast =
    List.map validateASTNode ast

let runAndValidate input =
    match run programStringParser input with
    | Success(result, _, _) ->
        printfn "Program String Parser result: %A" result
        validateAST result
    | Failure(errorMsg, _, _) ->
        [ErrorNode ("Parsing error: " + errorMsg, placeholderPosition, placeholderPosition)]

let rec printASTNode node =
    match node with
    | FieldsNode(s, _, _) -> printfn "Fields: %s" s
    | ActionNode(s, _, _) -> printfn "Action: %s" s
    | ErrorNode(e, p, _) -> printfn "Error: %s at %A" e p
    | CommandNode(nodes) ->
        printfn "Command:"
        nodes |> List.iter printASTNode
    | EmptyNode -> ignore()
    

let rec collectErrorNodes astNode =
    match astNode with
    | ErrorNode(_, _, _) -> [astNode]
    | CommandNode(nodes) -> List.collect collectErrorNodes nodes
    | _ -> []

let getAllErrorNodes ast =
    List.collect collectErrorNodes ast

let createErrorIndicator (errorNodes: ASTNode list) programLength =
    // Initialize a string of whitespace with the length of the program
    let indicatorArray = Array.create programLength ' '

    // Function to update the indicator array based on the position in an ErrorNode
    let updateIndicator (node: ASTNode) =
        match node with
        | ErrorNode(_, pos, _) ->
            let column = int pos.Column
            let index = column - 1 // Convert 1-based column number to 0-based array index

            if index >= 0 && index < Array.length indicatorArray then
                indicatorArray.[index] <- '-'
        | _ -> () // Ignore non-error nodes

    // Update the indicator array for each error node
    errorNodes |> List.iter updateIndicator

    // Convert the indicator array back to a string
    new string (indicatorArray)

[<EntryPoint>]
let main argv =
    let program : string = argv.[0]
    let validatedAST = runAndValidate program
    printfn "Validated AST: %A" validatedAST
    validatedAST |> List.iter printASTNode
    printfn "%s" program
    printfn "%s" (createErrorIndicator (getAllErrorNodes validatedAST) (program.Length + 3))
    0                                               