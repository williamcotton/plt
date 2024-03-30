module plt.common

open FParsec

let (<!>) (p: Parser<_, _>) label : Parser<_, _> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

let printErrors (errorMsgs: (string * ParserError) list) =        
    for msg, _ in List.rev errorMsgs do
        printfn "%s" msg       