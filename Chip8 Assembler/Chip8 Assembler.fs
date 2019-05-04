#if !INTERACTIVE
module Assembler
#endif

#if INTERACTIVE
#r @"C:\Users\Chris\OneDrive\BitBucket\FSharp-Chip8Emu\packages\FParsec\lib\net40-client\FParsecCS.dll"
#r @"C:\Users\Chris\OneDrive\BitBucket\FSharp-Chip8Emu\packages\FParsec\lib\net40-client\FParsec.dll"
#load @"..\Chip8\Extensions.fs"
#load @"..\Chip8\BitOps.fs"
#load @"..\Chip8\General.fs"
#load @"..\Chip8\Instructions.fs"
#endif

open Instructions
open General
open BitOps
open FParsec
open System
open System.IO

type UserState =
    {
        BytesSoFar : uint16
        Labels : Map<string, uint16>
    }
type Parser<'t> = Parser<'t, UserState>

/// Parse an unsigned number between the min and max (inclusive)
/// Allows binary / hex / integer input
let pUnsignedNumBetween min max : Parser<_> =
    fun stream ->
        let reply = numberLiteral NumberLiteralOptions.DefaultUnsignedInteger "Number" stream
        let result =
            if reply.Result.String.StartsWith("0x", StringComparison.InvariantCultureIgnoreCase) ||
               reply.Result.String.StartsWith("0b", StringComparison.InvariantCultureIgnoreCase) then
                reply.Result.String.[2..]
            else
                reply.Result.String
        if reply.Status = Ok then
            let numberBase =
                if reply.Result.IsHexadecimal then
                    16
                else if reply.Result.IsBinary then
                    2
                else
                    10
            try
                let b = Convert.ToUInt32(result, numberBase)
                if b >= min && b <= max then
                    Reply(b)
                else
                    Reply(ReplyStatus.Error, messageError (sprintf "Number was outside of acceptable range: %d-%d!" min max))
            with
            | :? OverflowException ->
                Reply(ReplyStatus.Error, messageError (sprintf "Number was outside of acceptable range: %d-%d!" min max))
            | _ ->
                Reply(ReplyStatus.Error, messageError "Expected number.")
        else
            Reply(reply.Status, reply.Error)

/// Parse a chip8 address (0x0-0x0FFF)
let pAddress : Parser<MemoryAddress> =
    fun stream ->
        let reply = pUnsignedNumBetween 0x0u 0x0FFFu stream
        if reply.Status = Ok then
            Reply(uint16 reply.Result)
        else
            Reply(reply.Status, reply.Error)
/// Parse an unsigned byte (0x0-0xFF)
let pByte = puint8

/// Parse an unsigned nibble (0x0-0xF)
let pNibble : Parser<_> =
    fun stream ->
        let reply = pUnsignedNumBetween 0x0u 0xFu stream
        if reply.Status = Ok then
            Reply(byte reply.Result)
        else
            Reply(reply.Status, reply.Error)

/// Parse a register
let pRegister : Parser<Register> =
    pstring "V" >>. pNibble

let updateByteCount count : Parser<_> =
    updateUserState (fun us -> { us with BytesSoFar = us.BytesSoFar + count })

let assignLabel label : Parser<_> =
    updateUserState (fun us -> { us with Labels = us.Labels.Add(label, us.BytesSoFar) })

let pLabelText = many1Chars asciiLetter

let processLabel : Parser<_> =
    fun stream ->
        let reply = pLabelText stream
        if reply.Status = Ok then
            match stream.UserState.Labels |> Map.tryFind reply.Result with
            | Some pos -> Reply(pos)
            | None -> Reply(FatalError, messageError (sprintf "Label '%s' has not been defined prior to this point." reply.Result))
        else
            Reply(reply.Status, reply.Error)

let pLabel : Parser<_> =
    pchar ':'
    >>. pLabelText
    >>= assignLabel
    >>. skipRestOfLine true


let pInstruction : Parser<_> =
    choice
        [
            pstring "ADDIV"  >>. spaces >>. pRegister    .>> spaces |>> ADDIV
            pstring "ADDVB"  >>. spaces >>. pRegister    .>> spaces .>>. pByte     |>> ADDVB
            pstring "ADDVV"  >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> ADDVV
            pstring "AND"    >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> AND
            pstring "CALL"   >>. spaces >>. processLabel .>> spaces |>> CALL
            pstring "CLS"    >>% spaces >>% CLS
            pstring "DRW"    >>. spaces >>. pipe3 (pRegister .>> spaces) (pRegister .>> spaces) pByte (fun r1 r2 b -> DRW(r1, r2, b))
            pstring "JPA"    >>. spaces >>. processLabel .>> spaces |>> JPA
            pstring "JP0A"   >>. spaces >>. pAddress     .>> spaces |>> JP0A
            pstring "LDIV"   >>. spaces >>. pRegister    .>> spaces |>> LDIV
            pstring "LDBCDV" >>. spaces >>. pRegister    .>> spaces |>> LDBCDV
            pstring "LDDTV"  >>. spaces >>. pRegister    .>> spaces |>> LDDTV
            pstring "LDFV"   >>. spaces >>. pRegister    .>> spaces |>> LDFV
            pstring "LDIA"   >>. spaces >>. processLabel .>> spaces |>> LDIA
            pstring "LDSTV"  >>. spaces >>. pRegister    .>> spaces |>> LDSTV
            pstring "LDVI"   >>. spaces >>. pRegister    .>> spaces |>> LDVI
            pstring "LDVB"   >>. spaces >>. pRegister    .>> spaces .>>. pByte |>> LDVB
            pstring "LDVDT"  >>. spaces >>. pRegister    .>> spaces |>> LDVDT
            pstring "LDVK"   >>. spaces >>. pRegister    .>> spaces |>> LDVK
            pstring "LDVV"   >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> LDVV
            pstring "OR"     >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> OR
            pstring "RET"    >>% spaces >>% RET
            pstring "RND"    >>. spaces >>. pRegister    .>> spaces .>>. pByte     |>> RND
            pstring "SEVB"   >>. spaces >>. pRegister    .>> spaces .>>. pByte     |>> SEVB
            pstring "SEVV"   >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> SEVV
            pstring "SHL"    >>. spaces >>. pRegister    .>> spaces |>> SHL
            pstring "SHR"    >>. spaces >>. pRegister    .>> spaces |>> SHR
            pstring "SKNP"   >>. spaces >>. pRegister    .>> spaces |>> SKNP
            pstring "SKP"    >>. spaces >>. pRegister    .>> spaces |>> SKP
            pstring "SNEVB"  >>. spaces >>. pRegister    .>> spaces .>>. pByte     |>> SNEVB
            pstring "SNEVV"  >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> SNEVV
            pstring "SUB"    >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> SUB
            pstring "SUBN"   >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> SUBN
            pstring "SYS"    >>. spaces >>. pAddress     .>> spaces |>> SYS
            pstring "XOR"    >>. spaces >>. pRegister    .>> spaces .>>. pRegister |>> XOR
        ]
    .>> updateByteCount 2us

let pNext =
    optional pLabel
    >>. pInstruction

let pFile file =
    match runParserOnFile
            (many1 (pNext .>> (spaces <|> skipNewline <|> eof)))
            { BytesSoFar = Chip8.ProgramBase ; Labels = Map.empty }
        file Text.Encoding.UTF8 with
    | Success (res, state, _) -> (res, state)
    | Failure (msg, _, _) -> failwith msg

let compile file (out : string) =
    let instructions, state =  pFile file
    let bytes =
        instructions
        |> Array.ofList
        |> Array.collect (encodeOp >> splitWord)
    File.WriteAllBytes(out, bytes)
    state

#if !INTERACTIVE
[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
#endif