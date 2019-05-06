module Program

open Assembler


#if !INTERACTIVE
[<EntryPoint>]
let main argv =
    printfn "%A" argv
    if argv.Length = 1 then
        compile argv.[0] (Path.GetFileNameWithoutExtension(argv.[0]) + ".c8")
        |> printfn "%A"
    else if argv.Length = 2 then
        compile argv.[0] argv.[1]
        |> printfn "%A"
    else
        printf @"Usage: .\program.exe [input path] (output path: optional)"

    0 // return an integer exit code
#endif