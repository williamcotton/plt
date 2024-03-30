module plt.SinglePassParser

open FParsec
open plt.common

type IntermediaryAST =
    | FieldNode of string * Position
    | ActionNode of string * Position
    | ErrorNode of string * Position

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

let commandParser =
    (pipe2 
        fieldsParser 
        (between (pchar '{' >>. spaces) (spaces >>. pchar '}') actionParser) 
        (fun fields action -> (fields, action)))

let programParser =
    spaces 
    >>. many (spaces >>. commandParser .>> spaces) .>> eof

[<EntryPoint>]
let main argv =
    let program : string = argv.[0]
    match run programParser program with
    | Success (result, _, _) -> 
        printfn "%A" result
        0
    | Failure (msg, _, _) ->
        printfn "Error: %s" msg
        1

