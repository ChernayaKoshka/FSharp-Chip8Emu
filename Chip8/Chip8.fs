module Chip8
open Decoder

[<EntryPoint>]
let main argv =
    if argv.Length = 1 then
        runFile false argv.[0]
    elif argv.Length = 2 then
        runFile true argv.[0]
    printfn "%A" argv
    0 // return an integer exit code
