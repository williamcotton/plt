open System
open System.IO

let tempResponse = """
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "capabilities": {
      "textDocumentSync": 1,
      "completionProvider": {
        "resolveProvider": false,
        "triggerCharacters": [".", ":"]
      },
      "hoverProvider": true
    }
  }
}
"""

let logMessage message =
    let logFile = "/Users/williamcotton/Projects/plt/plt-lsp/log.txt"
    File.AppendAllText(logFile, message + Environment.NewLine)

[<EntryPoint>]
let main argv =
    logMessage "Server starting..."
    argv |> Array.iter (fun arg -> logMessage arg)
    try
        match argv.Length with
        | 0 -> logMessage "No arguments provided"
        | _ ->
            // Simulate processing time
            // System.Threading.Thread.Sleep(1000) // Adjust or remove as needed
            logMessage "Sending response..."
            printfn "%s" tempResponse
    with
    | ex -> logMessage ("Exception occurred: " + ex.Message)
    logMessage "Server shutting down..."
    0
