module plt.program

open plt.parser

[<EntryPoint>]
let main argv =
    let program : string = argv.[0]
    let validatedAST = runAndValidate program
    validatedAST |> List.iter printASTNode
    0                                               