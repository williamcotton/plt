module plt.parser

open FParsec
open System
open System.Text.RegularExpressions

type IntermediaryASTNode =
    | IntermediaryFieldsNode of string * Position * Position
    | IntermediaryActionNode of string * Position * Position
    | IntermediaryErrorNode of string * Position * Position
    | IntermediaryCommandNode of IntermediaryASTNode list
    | IntermediaryEmptyNode

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

let intermediaryFieldsStringParser =
    pipe3
        (spaces >>. getPosition)
        (manyCharsTill anyChar (lookAhead (spaces .>> openBrace)))
        (getPosition .>> spaces)
        (fun startPos content endPos -> IntermediaryFieldsNode (content, startPos, endPos))

let intermediaryActionStringParser =
    pipe3
        (openBrace >>. spaces >>. getPosition)
        (manyCharsTill anyChar (lookAhead (spaces .>> closeBrace)))
        (getPosition .>> spaces .>> closeBrace)
        (fun startPos content endPos -> IntermediaryActionNode (content, startPos, endPos))

let errorParser =
    pipe3 
        (spaces >>. getPosition)
        (manyCharsTill anyChar (lookAhead closeBrace))
        (getPosition .>> spaces .>> closeBrace)
        (fun startPos chars endPos ->
            if String.IsNullOrWhiteSpace(chars) then 
                IntermediaryEmptyNode
            else 
                IntermediaryErrorNode(chars, startPos, endPos)) // Use startPos to mark the beginning of the error

let intermediaryCommandParser =
    pipe2 intermediaryFieldsStringParser (opt intermediaryActionStringParser)
        (fun p a -> IntermediaryCommandNode (p :: Option.toList a))

let intermediaryCommandErrorParser =
    choice [
        attempt intermediaryCommandParser   
        errorParser
    ] .>> spaces

let intermediaryProgramParser =
    many intermediaryCommandErrorParser

let calculatePosition msg (e : ParserError) (startPos : Position) =
    let backtrackPattern = @"Error in Ln: (\d+) Col: (\d+)"
    let matches = Regex.Matches(msg, backtrackPattern)
    if matches.Count > 1 then
        let lastMatch = matches.[matches.Count - 1]
        let line = Int64.Parse(lastMatch.Groups.[1].Value)
        let col = Int64.Parse(lastMatch.Groups.[2].Value)
        (startPos.Index + col, line + startPos.Line - 1L, startPos.Column + col - 1L, e.Position.Column - 1L)
    else
        (startPos.Index + int64 e.Position.Column - 1L, startPos.Line, startPos.Column + int64 e.Position.Column - 1L, e.Position.Column - 1L)

/// <summary>
/// Validates an intermediary AST node using the specified parser.
/// </summary>
/// <param name="parser">The parser to use for validation.</param>
/// <param name="node">The intermediary AST node to validate.</param>
/// <returns>The validated intermediary AST node.</returns>
let validateIntermediaryNodeWithParser parser node =
    let parseNode content (startPos : Position) (endPos : Position) nodeType =
        match run parser content with
        | Success(_, _, _) -> node
        | Failure(msg, e, _) ->
            let (index, line, column, endColumn) = calculatePosition msg e startPos
            let adjustedStartPos = Position(startPos.StreamName, index, line, column)
            let substring = content.Substring(int(endColumn))
            let endOfStringIndexNumber = 
                match run stringContent substring with
                | Success(restOfString, _, _) ->
                    endColumn + int64(restOfString.Length) - 1L  // adjust index to be relative to the entire content string
                | Failure(_, _, _) ->
                    endPos.Column
            printfn "Substring: %s" substring
            printfn "End of string index number: %d" endOfStringIndexNumber
            let adjustedEndPos = Position(endPos.StreamName, endPos.Index, endPos.Line, endPos.Column - 1L)
            let errorDetail = msg.Trim().Split('\n') |> Array.last
            IntermediaryErrorNode(sprintf "%s - %s - %s" nodeType content errorDetail, adjustedStartPos, adjustedEndPos)

    match node with
    | IntermediaryActionNode(content, startPos, endPos) -> parseNode content startPos endPos "Action"
    | IntermediaryFieldsNode(content, startPos, endPos) -> parseNode content startPos endPos "Fields"
    | _ -> node


let actionValidatorParser =
    actionParser

let fieldsValidatorParser =
    fieldsParser

/// <summary>
/// Validates a single intermediary AST node using the appropriate parser.
/// </summary>
/// <param name="node">The intermediary AST node to validate.</param>
/// <returns>The validated intermediary AST node.</returns>
let rec validateIntermediaryASTNode node =
    match node with
    | IntermediaryActionNode(_, _, _) -> validateIntermediaryNodeWithParser actionValidatorParser node
    | IntermediaryFieldsNode(_, _, _) -> validateIntermediaryNodeWithParser fieldsValidatorParser node
    | IntermediaryErrorNode(_, _, _) -> node
    | IntermediaryCommandNode(nodes) ->
        let validatedNodes = List.map validateIntermediaryASTNode nodes
        IntermediaryCommandNode(validatedNodes)
    | IntermediaryEmptyNode -> node

/// <summary>
/// Validates an intermediary AST by applying specific parsers to each node.
/// </summary>
/// <param name="ast">The intermediary AST to validate.</param>
/// <returns>The validated intermediary AST.</returns>
let validateIntermediaryAST ast =
    List.map validateIntermediaryASTNode ast

/// <summary>
/// Parses a string into an intermediary Abstract Syntax Tree (AST).
/// </summary>
/// <param name="input">The input string to parse.</param>
/// <returns>The parsed intermediary AST.</returns>
let runAndValidate input =
    match run intermediaryProgramParser input with
    | Success(result, _, _) ->
        validateIntermediaryAST result
    | Failure(errorMsg, e, _) ->
        [IntermediaryErrorNode ("Parsing error: " + (errorMsg.Trim().Split('\n') |> Array.last), e.Position, e.Position)]

/// <summary>
/// Prints an intermediary AST node.
/// </summary>
/// <param name="node">The intermediary AST node to print.</param>
let rec printASTNode node =
    match node with
    | IntermediaryFieldsNode(s, _, _) -> printfn "Fields: %s" s
    | IntermediaryActionNode(s, _, _) -> printfn "Action: %s" s
    | IntermediaryErrorNode(e, startPos, endPos) -> printfn "Error: %s at %A %A" e startPos endPos
    | IntermediaryCommandNode(nodes) ->
        printfn "Command:"
        nodes |> List.iter printASTNode
    | IntermediaryEmptyNode -> ignore()
    
/// <summary>
/// Collects all error nodes from an intermediary AST node.
/// </summary>
/// <param name="astNode">The intermediary AST node to collect error nodes from.</param>
/// <returns>A list of error nodes.</returns>
let rec collectErrorNodes astNode =
    match astNode with
    | IntermediaryErrorNode(_, _, _) -> [astNode]
    | IntermediaryCommandNode(nodes) -> List.collect collectErrorNodes nodes
    | _ -> []

/// <summary>
/// Gets all error nodes from an intermediary AST.
/// </summary>
/// <param name="ast">The intermediary AST to collect error nodes from.</param>
/// <returns>A list of error nodes.</returns>
let getAllErrorNodes ast =
    List.collect collectErrorNodes ast
