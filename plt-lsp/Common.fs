module plt.common

open FParsec

let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

// https://stackoverflow.com/questions/9248426/basic-error-recovery-with-fparsec
type UserState = {
    Errors: (string * ParserError) list
} with
    static member Create() = {Errors = []}

type Parser<'t> = Parser<'t, UserState>

let printErrors (errorMsgs: (string * ParserError) list) =        
    for msg, _ in List.rev errorMsgs do
        printfn "%s" msg       

// recover from error by skipping to the char after the next newline or ';'
let recoverWith errorResult (p: Parser<_>) : Parser<_> =    
  fun stream ->
    let stateTag = stream.StateTag
    let mutable reply = p stream
    if reply.Status <> Ok then // the parser failed
        let error = ParserError(stream.Position, stream.UserState, reply.Error)
        let errorMsg = error.ToString(stream)
        stream.SkipCharsOrNewlinesWhile(fun c -> c <> ';' && c <> '\n') |> ignore                        
        stream.ReadCharOrNewline() |> ignore
        // To prevent infinite recovery attempts in certain situations,
        // the following check makes sure that either the parser p 
        // or our stream.Skip... commands consumed some input.
        if stream.StateTag <> stateTag then
            let oldErrors = stream.UserState.Errors
            printErrors oldErrors
            stream.UserState <- {Errors = (errorMsg, error)::oldErrors}     
            reply <- Reply(errorResult)
    reply

// Example values
let exampleStringList = ["example1"; "example2"]
let exampleString = "exampleString"
let exampleTupleList = [("exampleKey1", "exampleValue1"); ("exampleKey2", "exampleValue2")]

// Creating a value of the expected type
let exampleValue = 
    ((exampleStringList, exampleString), (exampleString, exampleString, exampleTupleList))