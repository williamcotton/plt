#!/usr/bin/env dotnet fsi

#r "nuget: FParsec"

// grammar
//
// ```ebnf
// program = command*;
// command = fields "{" action "}";
// fields = FIELDNAME "," FIELDNAME* | "[" FIELDNAME "," FIELDNAME* "]" "," FIELDNAME;
// action = "plot" style | "bar" style | "stackbar" multi_style;
// style = WIDTH DRAW_STYLE COLOR;
// multi_style = WIDTH "[" DRAW_STYLE COLOR "," DRAW_STYLE COLOR "]";
// FIELDNAME = [a-zA-Z0-9_]+;
// WIDTH = [0-9]+ "px";
// DRAW_STYLE = "solid" | "dashed" | "dotted";
// COLOR = "red" | "green" | "blue" | "orange" | "black" | "yellow" | "#" [0-9a-fA-F]{6} | "#" [0-9a-fA-F]{3};
// ```

open FParsec

// test
let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

// common    
let notNewline = satisfy (fun c -> c <> '\n' && c <> '\r')

// fields
let stringContent = many1Chars (noneOf ",[] \t\r\n")
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
    spaces >>. pipe2 stringOrCommaSeparatedStringsBetweenBrackets (pstring "," >>. stringParser) (fun ys x -> (ys, x)) .>> spaces .>> eof

test stringContent "abc" // Success: "abc"
test stringParser "  abc  " // Success: "abc"
test commaSeparatedStrings "   a,   b,     c " // Success: ["a"; "b"; "c"]
test commaSeparatedStringsBetweenBrackets "[a, b, c]" // Success: ["a"; "b"; "c"]
test commaSeparatedStringsBetweenBrackets "[a, b, c" // Failure: Error in Ln: 1 Col: 9
test commaSeparatedStringsBetweenBrackets "a, b, c]" // Failure: Error in Ln: 1 Col: 1
test commaSeparatedStringsBetweenBrackets "[  
     a   , b   ,  
      c  
]" // Success: ["a"; "b"; "c"]
test stringOrCommaSeparatedStringsBetweenBrackets "test" // Success: ["test"]
test stringOrCommaSeparatedStringsBetweenBrackets "[test1, test2]" // Success: ["test1"; "test2"]
test fieldsParser "y, x" // Success: (["y"], "x")
test fieldsParser "[y1, y2], x" // Success: (["y1"; "y2"], "x")
test fieldsParser "[y1,y2],x" // Success: (["y1"; "y2"], "x")
test fieldsParser "   [y1, y2]  , x" // Success: (["y1"; "y2"], "x")
test fieldsParser "   [  
    y1
,    y2   ]  , 
x" // Success: (["y1"; "y2"], "x")
test fieldsParser "y x" // Failure: Error in Ln: 1 Col: 3
test fieldsParser "[y1, y2] x" // Failure: Error in Ln: 1 Col: 10
test fieldsParser "[y1, y2], x, z" // Failure: Error in Ln: 1 Col: 12

// color
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

test colorParser "red" // Success: "red"
test colorParser "green" // Success: "green"
test colorParser "blue" // Success: "blue"
test colorParser "yellow" // Success: "yellow"
test colorParser "orange" // Success: "orange"
test colorParser "black" // Success: "black"
test colorParser "#123de6" // Success: "#123de6"
test colorParser "#1a3" // Success: "#123"
test colorParser "#1z3" // Failure: Error in Ln: 1 Col: 3
test colorParser "#1f34" // Failure: Error in Ln: 1 Col: 6
test colorParser "#12d45" // Failure: Error in Ln: 1 Col: 7
test colorParser "#1234567" // Failure: Error in Ln: 1 Col: 9
test colorParser "#123dez" // Failure: Error in Ln: 1 Col: 

// width
let widthParser = many1Chars digit .>> pstring "px" |>> (fun digits -> digits + "px")

test widthParser "1px" // Success: "1px"
test widthParser "10px" // Success: "10px"
test widthParser "100px" // Success: "100px"
test widthParser "1000px" // Success: "1000px"
test widthParser "sdf" // Failure: Error in Ln: 1 Col: 1

// draw style
let drawStyleParser =
    choice [
        pstring "solid"; 
        pstring "dashed"; 
        pstring "dotted"
    ]

test drawStyleParser "solid" // Success: "solid"
test drawStyleParser "dashed" // Success: "dashed"
test drawStyleParser "dotted" // Success: "dotted"
test drawStyleParser "sdfsdf" // Failure: Error in Ln: 1 Col: 6

let styleParser =
    pipe3 (widthParser .>> spaces) (drawStyleParser .>> spaces) colorParser (fun width style color -> (width, [(style, color)]))

test styleParser "100px solid #123456" // Success: ("100px", "solid", "#123456")
test styleParser "50px dashed red" // Success: ("50px", "dashed", "red")
test styleParser "10px dotted #123" // Success: ("10px", "dotted", "#123")
test styleParser "10px sdfsdf #123456" // Failure: Error in Ln: 1 Col: 6
test styleParser "sdf sdfsdf #123456" // Failure: Error in Ln: 1 Col: 1
test styleParser "10px dotted #1234" // Failure: Error in Ln: 1 Col: 18

// multi_style

let styleColorPairParser =
    pipe2 (drawStyleParser .>> spaces) colorParser (fun style color -> (style, color))
let multiStyleParser =
    pipe2 
        (widthParser .>> spaces) 
        (between (pchar '[') (pchar ']') 
            (sepBy styleColorPairParser (pstring ", " .>> spaces))
        )
        (fun width styleColorPairs -> (width, styleColorPairs))

test multiStyleParser "100px [solid #123456, dotted #ff0000, dashed green]" // Success: ("100px", [("solid", "#123456"); ("dotted", "#ff0000"); ("dashed", "green")])
test multiStyleParser "50px [dashed blue]" // Success: ("50px", [("dashed", "blue")])
test multiStyleParser "20px [solid #123, dashed #abc, dotted yellow]" // Success: ("20px", [("solid", "#123"); ("dashed", "#abc"); ("dotted", "yellow")])
test multiStyleParser "100px [solid #123456]" // Success: ("100px", [("solid", "#123456")])
test multiStyleParser "100px solid #123456, dotted]" // Failure: Error in Ln: 1 Col: 21

// action

let plotParser = 
    pstring "plot" >>. spaces >>. styleParser |>> fun (width, [(style, color)]) -> 
    ("plot", width, [(style, color)])
let barParser = 
    pstring "bar" >>. spaces >>. styleParser |>> fun (width, [(style, color)]) -> 
    ("bar", width, [(style, color)])
let stackbarParser = 
    pstring "stackbar" >>. spaces >>. multiStyleParser |>> fun (width, styleColorPairs) -> 
    ("stackbar", width, styleColorPairs)



let graphicNameParser = manyChars notNewline
let genericGraphicParser = 
    graphicNameParser >>= fun name ->
    spaces >>. choice [styleParser; multiStyleParser] |>> fun (width, styleColorPairs) ->
    (name, width, styleColorPairs)
let actionParser =
    choice [
        plotParser; 
        barParser;
        stackbarParser;
        genericGraphicParser;
    ]

test actionParser "plot 100px solid #123456" // Success: ("plot", "100px", [("solid", "#123456")])
test actionParser "bar 100px solid #123456" // Success: ("bar", "100px", [("solid", "#123456")])
test actionParser "stackbar 100px [solid #123456, dotted #ff0000, dashed green]" // Success: ("stackbar", "100px", [("solid", "#123456"); ("dotted", "#ff0000"); ("dashed", "green")])
test actionParser "plot 100px solid #123456, dotted #ff0000, dashed green" // Success: ("plot", "100px", [("solid", "#123456"); ("dotted", "#ff0000"); ("dashed", "green")])
test actionParser "plugin 100px solid #123456" // Success: ("plugin", "100px", [("solid", "#123456")])