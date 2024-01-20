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

// Define the color parser
let hexadecimalDigit = satisfy (fun c -> isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
let hexColorDigit3 = pipe3 hexadecimalDigit hexadecimalDigit hexadecimalDigit (fun c1 c2 c3 -> sprintf "%c%c%c" c1 c2 c3)
let hexColorDigit6 = pipe2 hexColorDigit3 hexColorDigit3 (fun c1 c2 -> c1 + c2)
let hexColor = pstring "#" >>. (hexColorDigit6 <|> hexColorDigit3) |>> (fun colorDigits -> "#" + colorDigits)

let colorParser =
    choice [
        pstring "red"; pstring "green"; pstring "blue"; pstring "yellow"; pstring "orange"; pstring "black";
        hexColor
    ]

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

// Define yOverX parser to capture both ys and x
let yOverX =
    spaces >>. pipe2 stringOrCommaSeparatedStringsBetweenBrackets (pstring "," >>. stringParser) (fun ys x -> (ys, x)) .>> spaces .>> eof

// Test function
let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

// Run tests
test stringContent "abc" // Success: "abc"
test stringParser "  abc  " // Success: "abc"
test commaSeparatedStrings "   a,   b,     c " // Success: ["a"; "b"; "c"]
test commaSeparatedStringsBetweenBrackets "[a, b, c]" // Success: ["a"; "b"; "c"]
test commaSeparatedStringsBetweenBrackets "[  
     a   , b   ,  
      c  
]" // Success: ["a"; "b"; "c"]
test stringOrCommaSeparatedStringsBetweenBrackets "test" // Success: ["test"]
test stringOrCommaSeparatedStringsBetweenBrackets "[test1, test2]" // Success: ["test1"; "test2"]
test yOverX "y, x" // Success: (["y"], "x")
test yOverX "[y1, y2], x" // Success: (["y1"; "y2"], "x")
test yOverX "[y1,y2],x" // Success: (["y1"; "y2"], "x")
test yOverX "   [y1, y2]  , x" // Success: (["y1"; "y2"], "x")
test yOverX "   [  
    y1
,    y2   ]  , 
x" // Success: (["y1"; "y2"], "x")
test yOverX "y x" // Failure: Error in Ln: 1 Col: 3
test yOverX "[y1, y2] x" // Failure: Error in Ln: 1 Col: 10
test yOverX "[y1, y2], x, z" // Failure: Error in Ln: 1 Col: 13

test hexColorDigit3 "abc" // Success: "abc"
test hexColorDigit3 "123" // Success: "123"
test hexColorDigit3 "a1b" // Success: "a1b"
test hexColorDigit3 "a1" // Failure: Error in Ln: 1 Col: 3

test hexColorDigit6 "abc123" // Success: "abc123"
test hexColorDigit6 "123abc" // Success: "123abc"
test hexColorDigit6 "a7b2c3" // Success: "a1b2c3"

test colorParser "red" // Success: "red"
test colorParser "green" // Success: "green"
test colorParser "blue" // Success: "blue"
test colorParser "yellow" // Success: "yellow"
test colorParser "orange" // Success: "orange"
test colorParser "black" // Success: "black"
test colorParser "#123456" // Success: "#123456"
test colorParser "#123" // Success: "#123"