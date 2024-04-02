module fs.tests

open Expecto
open FParsec
open plt.main

let testParser (p: Parser<_,_>) input expected =
    match run p input with
    | Success(result, _, _) -> result = expected
    | Failure(_, _, _) -> false

[<Tests>]
let tests = 
    testList "Parser Tests" [
        testList "stringContent" [
            testCase "stringContent with 'abc'" <| fun () ->
                Expect.isTrue (testParser stringContent "abc" "abc") "stringContent should parse 'abc'"

            testCase "stringContent with 'abc def'" <| fun () ->
                Expect.isTrue (testParser stringContent "abc def" "abc") "stringContent should parse 'abc'"

            testCase "stringContent with 'abc def ghi'" <| fun () ->
                Expect.isTrue (testParser stringContent "abc def ghi" "abc") "stringContent should parse 'abc'"
        ]
        
        testList "stringParser" [
            testCase "stringParser with 'abc'" <| fun () ->
                Expect.isTrue (testParser stringParser "abc" "abc") "stringParser should parse 'abc'"

            testCase "stringParser with '  abc  '" <| fun () ->
                Expect.isTrue (testParser stringParser "  abc  " "abc") "stringParser should parse 'abc'"

            testCase "stringParser with '  abc  def  '" <| fun () ->
                Expect.isTrue (testParser stringParser "  abc  def  " "abc") "stringParser should parse 'abc'"
        ]

        testList "commaSeparatedStrings" [
            testCase "commaSeparatedStrings with 'a,b, c'" <| fun () ->
                Expect.isTrue (testParser commaSeparatedStrings "a, b, c" ["a"; "b"; "c"]) "commaSeparatedStrings should parse 'a, b, c'"

            testCase "commaSeparatedStrings with '   a,   b,     c '" <| fun () ->
                Expect.isTrue (testParser commaSeparatedStrings "   a,   b,     c " ["a"; "b"; "c"]) "commaSeparatedStrings should parse 'a, b, c'"
        ]

        testList "commaSeparatedStringsBetweenBrackets" [
            testCase "commaSeparatedStringsBetweenBrackets with '[a, b, c]'" <| fun () ->
                Expect.isTrue (testParser commaSeparatedStringsBetweenBrackets "[a, b, c]" ["a"; "b"; "c"]) "commaSeparatedStringsBetweenBrackets should parse '[a, b, c]'"

            testCase "commaSeparatedStringsBetweenBrackets with '[a, b, c'" <| fun () ->
                Expect.isFalse (testParser commaSeparatedStringsBetweenBrackets "[a, b, c" ["a"; "b"; "c"]) "commaSeparatedStringsBetweenBrackets should fail on incomplete bracket"

            testCase "commaSeparatedStringsBetweenBrackets with 'a, b, c]'" <| fun () ->
                Expect.isFalse (testParser commaSeparatedStringsBetweenBrackets "a, b, c]" ["a"; "b"; "c"]) "commaSeparatedStringsBetweenBrackets should fail on incomplete bracket"

            testCase "commaSeparatedStringsBetweenBrackets with '[
                    a   , b   ,
                        c
                ]'" <| fun () ->
                Expect.isTrue (testParser commaSeparatedStringsBetweenBrackets "[
                        a   , b   ,
                            c
                    ]" ["a"; "b"; "c"]) "commaSeparatedStringsBetweenBrackets should parse '[a, b, c]'"
        ]

        testList "stringOrCommaSeparatedStringsBetweenBrackets Tests" [
            testCase "stringOrCommaSeparatedStringsBetweenBrackets with 'test'" <| fun () ->
                Expect.isTrue (testParser stringOrCommaSeparatedStringsBetweenBrackets "test" ["test"]) "stringOrCommaSeparatedStringsBetweenBrackets should parse 'test'"

            testCase "stringOrCommaSeparatedStringsBetweenBrackets with '[test1, test2]'" <| fun () ->
                Expect.isTrue (testParser stringOrCommaSeparatedStringsBetweenBrackets "[test1, test2]" ["test1"; "test2"]) "stringOrCommaSeparatedStringsBetweenBrackets should parse '[test1, test2]'"
        ]

        testList "fieldsParser Tests" [
            testCase "fieldsParser with 'y, x'" <| fun () ->
                Expect.isTrue (testParser fieldsParser "y, x" (["y"], "x")) "fieldsParser should parse 'y, x'"

            testCase "fieldsParser with '[y1, y2], x'" <| fun () ->
                Expect.isTrue (testParser fieldsParser "[y1, y2], x" (["y1"; "y2"], "x")) "fieldsParser should parse '[y1, y2], x'"

            testCase "fieldsParser with '[y1,y2],x'" <| fun () ->
                Expect.isTrue (testParser fieldsParser "[y1,y2],x" (["y1"; "y2"], "x")) "fieldsParser should parse '[y1,y2],x'"

            testCase "fieldsParser with '[y1,y2],x{}'" <| fun () ->
                Expect.isTrue (testParser fieldsParser "[y1,y2],x{}" (["y1"; "y2"], "x")) "fieldsParser should parse '[y1,y2],x{}'"

            testCase "fieldsParser with '   [y1, y2]  , x'" <| fun () ->
                Expect.isTrue (testParser fieldsParser "   [y1, y2]  , x" (["y1"; "y2"], "x")) "fieldsParser should parse '   [y1, y2]  , x'"

            testCase "fieldsParser with multiline input" <| fun () ->
                Expect.isTrue (testParser fieldsParser "   [  
                    y1
                ,    y2   ]  , 
                x" (["y1"; "y2"], "x")) "fieldsParser should parse multiline input"

            testCase "fieldsParser with 'y x' (should fail)" <| fun () ->
                Expect.isFalse (testParser fieldsParser "y x" (["y"], "x")) "fieldsParser should fail on 'y x'"

            testCase "fieldsParser with '[y1, y2] x' (should fail)" <| fun () ->
                Expect.isFalse (testParser fieldsParser "[y1, y2] x" (["y1"; "y2"], "x")) "fieldsParser should fail on '[y1, y2] x'"

            testCase "fieldsParser with '[y1, y2], x, z' (should fail)" <| fun () ->
                Expect.isTrue (testParser fieldsParser "[y1, y2], x, z" (["y1"; "y2"], "x")) "fieldsParser should fail on '[y1, y2], x, z'"
        ]

        testList "colorParser Tests" [
            testCase "colorParser with 'red'" <| fun () ->
                Expect.isTrue (testParser colorParser "red" "red") "colorParser should parse 'red'"

            testCase "colorParser with 'green'" <| fun () ->
                Expect.isTrue (testParser colorParser "green" "green") "colorParser should parse 'green'"

            testCase "colorParser with 'blue'" <| fun () ->
                Expect.isTrue (testParser colorParser "blue" "blue") "colorParser should parse 'blue'"

            testCase "colorParser with 'yellow'" <| fun () ->
                Expect.isTrue (testParser colorParser "yellow" "yellow") "colorParser should parse 'yellow'"

            testCase "colorParser with 'orange'" <| fun () ->
                Expect.isTrue (testParser colorParser "orange" "orange") "colorParser should parse 'orange'"

            testCase "colorParser with 'black'" <| fun () ->
                Expect.isTrue (testParser colorParser "black" "black") "colorParser should parse 'black'"

            testCase "colorParser with '#123de6'" <| fun () ->
                Expect.isTrue (testParser colorParser "#123de6" "#123de6") "colorParser should parse '#123de6'"

            testCase "colorParser with '#1a3'" <| fun () ->
                Expect.isTrue (testParser colorParser "#1a3" "#1a3") "colorParser should parse '#1a3'"

            testCase "colorParser with invalid hex '#1z3'" <| fun () ->
                Expect.isFalse (testParser colorParser "#1z3" "#1z3") "colorParser should fail on '#1z3'"

            testCase "colorParser with invalid hex '#1f34'" <| fun () ->
                Expect.isFalse (testParser colorParser "#1f34" "#1f34") "colorParser should fail on '#1f34'"

            testCase "colorParser with invalid hex '#12d45'" <| fun () ->
                Expect.isFalse (testParser colorParser "#12d45" "#12d45") "colorParser should fail on '#12d45'"

            testCase "colorParser with too long hex '#1234567'" <| fun () ->
                Expect.isFalse (testParser colorParser "#1234567" "#1234567") "colorParser should fail on '#1234567'"

            testCase "colorParser with invalid hex '#123dez'" <| fun () ->
                Expect.isFalse (testParser colorParser "#123dez" "#123dez") "colorParser should fail on '#123dez'"
        ]

        testList "widthParser Tests" [
            testCase "widthParser with '1px'" <| fun () ->
                Expect.isTrue (testParser widthParser "1px" "1px") "widthParser should parse '1px'"

            testCase "widthParser with '10px'" <| fun () ->
                Expect.isTrue (testParser widthParser "10px" "10px") "widthParser should parse '10px'"

            testCase "widthParser with '100px'" <| fun () ->
                Expect.isTrue (testParser widthParser "100px" "100px") "widthParser should parse '100px'"

            testCase "widthParser with '1000px'" <| fun () ->
                Expect.isTrue (testParser widthParser "1000px" "1000px") "widthParser should parse '1000px'"

            testCase "widthParser with invalid input 'sdf'" <| fun () ->
                Expect.isFalse (testParser widthParser "sdf" "sdf") "widthParser should fail on 'sdf'"
        ]

        testList "drawStyleParser Tests" [
            testCase "drawStyleParser with 'solid'" <| fun () ->
                Expect.isTrue (testParser drawStyleParser "solid" "solid") "drawStyleParser should parse 'solid'"

            testCase "drawStyleParser with 'dashed'" <| fun () ->
                Expect.isTrue (testParser drawStyleParser "dashed" "dashed") "drawStyleParser should parse 'dashed'"

            testCase "drawStyleParser with 'dotted'" <| fun () ->
                Expect.isTrue (testParser drawStyleParser "dotted" "dotted") "drawStyleParser should parse 'dotted'"

            testCase "drawStyleParser with invalid input 'sdfsdf'" <| fun () ->
                Expect.isFalse (testParser drawStyleParser "sdfsdf" "sdfsdf") "drawStyleParser should fail on 'sdfsdf'"
        ]

        testList "styleParser Tests" [
            testCase "styleParser with '100px solid #123456'" <| fun () ->
                Expect.isTrue (testParser styleParser "100px solid #123456" ("100px", [("solid", "#123456")])) "styleParser should parse '100px solid #123456'"

            testCase "styleParser with '50px dashed red'" <| fun () ->
                Expect.isTrue (testParser styleParser "50px dashed red" ("50px", [("dashed", "red")])) "styleParser should parse '50px dashed red'"

            testCase "styleParser with '10px dotted #123'" <| fun () ->
                Expect.isTrue (testParser styleParser "10px dotted #123" ("10px", [("dotted", "#123")])) "styleParser should parse '10px dotted #123'"

            testCase "styleParser with trailing characters '10px dotted #123    { '" <| fun () ->
                Expect.isTrue (testParser styleParser "10px dotted #123    {" ("10px", [("dotted", "#123")])) "styleParser should parse '10px dotted #123    {'"

            testCase "styleParser with invalid drawStyle '10px sdfsdf #123456'" <| fun () ->
                Expect.isFalse (testParser styleParser "10px sdfsdf #123456" ("10px", [("sdfsdf", "#123456")])) "styleParser should fail on '10px sdfsdf #123456'"

            testCase "styleParser with invalid width 'sdf sdfsdf #123456'" <| fun () ->
                Expect.isFalse (testParser styleParser "sdf sdfsdf #123456" ("sdf", [("sdfsdf", "#123456")])) "styleParser should fail on 'sdf sdfsdf #123456'"

            testCase "styleParser with invalid color '10px dotted #1234'" <| fun () ->
                Expect.isFalse (testParser styleParser "10px dotted #1234" ("10px", [("dotted", "#1234")])) "styleParser should fail on '10px dotted #1234'"
        ]

        testList "multiStyleParser Tests" [
            testCase "multiStyleParser with '100px [solid #123456, dotted #ff0000, dashed green]'" <| fun () ->
                Expect.isTrue (testParser multiStyleParser "100px [solid #123456, dotted #ff0000, dashed green]" ("100px", [("solid", "#123456"); ("dotted", "#ff0000"); ("dashed", "green")])) "multiStyleParser should parse '100px [solid #123456, dotted #ff0000, dashed green]'"

            testCase "multiStyleParser with '50px [dashed blue]'" <| fun () ->
                Expect.isTrue (testParser multiStyleParser "50px [dashed blue]" ("50px", [("dashed", "blue")])) "multiStyleParser should parse '50px [dashed blue]'"

            testCase "multiStyleParser with '20px [solid #123, dashed #abc, dotted yellow]'" <| fun () ->
                Expect.isTrue (testParser multiStyleParser "20px [solid #123, dashed #abc, dotted yellow]" ("20px", [("solid", "#123"); ("dashed", "#abc"); ("dotted", "yellow")])) "multiStyleParser should parse '20px [solid #123, dashed #abc, dotted yellow]'"

            testCase "multiStyleParser with '100px [solid #123456]'" <| fun () ->
                Expect.isTrue (testParser multiStyleParser "100px [solid #123456]" ("100px", [("solid", "#123456")])) "multiStyleParser should parse '100px [solid #123456]'"

            testCase "multiStyleParser with incomplete input '100px solid #123456, dotted]'" <| fun () ->
                Expect.isFalse (testParser multiStyleParser "100px solid #123456, dotted]" ("100px", [("solid", "#123456"); ("dotted", "")])) "multiStyleParser should fail on '100px solid #123456, dotted]'"
        ]

        testList "actionParser Tests" [
            testCase "actionParser with 'plot 100px solid #123456'" <| fun () ->
                Expect.isTrue (testParser actionParser "plot 100px solid #123456" ("plot", "100px", [("solid", "#123456")])) "actionParser should parse 'plot 100px solid #123456'"

            testCase "actionParser with 'bar 100px solid #123456'" <| fun () ->
                Expect.isTrue (testParser actionParser "bar 100px solid #123456" ("bar", "100px", [("solid", "#123456")])) "actionParser should parse 'bar 100px solid #123456'"

            testCase "actionParser with 'stackbar 100px [solid #123456, dotted #ff0000, dashed green]'" <| fun () ->
                Expect.isTrue (testParser actionParser "stackbar 100px [solid #123456, dotted #ff0000, dashed green]" ("stackbar", "100px", [("solid", "#123456"); ("dotted", "#ff0000"); ("dashed", "green")])) "actionParser should parse 'stackbar 100px [solid #123456, dotted #ff0000, dashed green]'"

            testCase "actionParser with 'plugin 100px solid #123456'" <| fun () ->
                Expect.isTrue (testParser actionParser "plugin 100px solid #123456" ("plugin", "100px", [("solid", "#123456")])) "actionParser should parse 'plugin 100px solid #123456'"

            testCase "actionParser with 'plotplug 100px solid #123456'" <| fun () ->
                Expect.isTrue (testParser actionParser "plotplug 100px solid #123456" ("plotplug", "100px", [("solid", "#123456")])) "actionParser should parse 'plotplug 100px solid #123456'"
        ]

        testList "commandParser Tests" [
            testCase "commandParser with 'y, x { plot 100px solid #123456 }'" <| fun () ->
                Expect.isTrue (testParser commandParser "y, x { plot 100px solid #123456 }" 
                    (CommandNode [
                            FieldsNode ("", Position("", 0 ,0 ,0)); 
                            ActionNode ("", Position("", 0 ,0 ,0))
                        ])) 
                    "commandParser should parse 'y, x { plot 100px solid #123456 }'"

            testCase "commandParser with 'y,x{plot 100px solid #123456}'" <| fun () ->
                Expect.isTrue (testParser commandParser "y,x{plot 100px solid #123456}" (CommandNode [])) "commandParser should parse 'y,x{plot 100px solid #123456}'"

            testCase "commandParser with spaces '   y   ,  x   {    plot    100px    solid    #123456    }    '" <| fun () ->
                Expect.isTrue (testParser commandParser "   y   ,  x   {    plot    100px    solid    #123456    }    " (CommandNode [])) "commandParser should parse '   y   ,  x   {    plot    100px    solid    #123456    }    '"
        ]

        testList "programParser Tests" [
            testCase "programParser with simple commands" <| fun () ->
                let expected = [CommandNode []]
                Expect.isTrue (testParser programStringParser "y, x { plot 100px solid #123456 } x, y { bar 10px solid red }" expected) "programParser should parse simple commands"

            testCase "programParser with multiple field commands" <| fun () ->
                let expected = [ CommandNode [] ]
                Expect.isTrue (testParser programStringParser "y, x { plot 100px solid #123456 } [x1, x2], y { plugin 10px solid red }" expected) "programParser should parse multiple field commands"

            testCase "programParser with complex multi-command input" <| fun () ->
                let expected = [ CommandNode [] ]
                Expect.isTrue (testParser programStringParser "
                    one, date {
                    bug 10px dashed red
                    }

                    three, date {
                    bar 10px dotted #7df
                    }

                    two,date {
                    plot 10px solid #d83
                    }

                    two, date {
                    plot 3px dotted green
                    }

                    [one, two, three], date { bar 10px [solid red, solid green, solid blue] }
                    [one,two,three],date{stackbar 10px [solid orange,dashed #fed,dotted #8d2] }
                    " expected) "programParser should parse complex multi-command input"

            testCase "programParser with extensive commands and new actions" <| fun () ->
                let expected = [ CommandNode [] ]
                Expect.isTrue (testParser programStringParser "
                    one, date {
                    plotplug 10px dashed red
                    }

                    three, date {
                    bar 10px dotted #7df
                    }

                    two,date {
                    plot 10px solid #d83
                    }

                    A, date {highlight 01px solid yellow }

                    two, date {
                    plot 3px dotted green
                    }

                    [one, two, three], date { bar 10px [solid red, solid green, solid blue] }
                    [one,two,three],date{stackbar 10px [solid orange,dashed #fed,dotted #8d2] }

                    three, date { bleep 10px dotted green }
                    " expected) "programParser should parse extensive commands and new actions"
        ]


    ]

[<EntryPoint>]
let main argv =
    runTestsWithCLIArgs [] [||] tests