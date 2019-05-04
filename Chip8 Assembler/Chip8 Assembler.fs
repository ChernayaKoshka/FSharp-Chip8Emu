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

type UserState = unit
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
let pAddr : Parser<MemoryAddress> =
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

let pInstruction : Parser<_> =
    choice
        [
            pstring "ADDIV"  >>. spaces >>. pRegister .>> spaces |>> ADDIV
            pstring "ADDVB"  >>. spaces >>. pRegister .>> spaces .>>. pByte     |>> ADDVB
            pstring "ADDVV"  >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> ADDVV
            pstring "AND"    >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> AND
            pstring "CALL"   >>. spaces >>. pAddr     .>> spaces |>> CALL
            pstring "CLS"    >>% spaces >>% CLS
            pstring "DRW"    >>. spaces >>. pipe3 (pRegister .>> spaces) (pRegister .>> spaces) pByte (fun r1 r2 b -> DRW(r1, r2, b))
            pstring "JPA"    >>. spaces >>. pAddr     .>> spaces |>> JPA
            pstring "JP0A"   >>. spaces >>. pAddr     .>> spaces |>> JP0A
            pstring "LDIV"   >>. spaces >>. pRegister .>> spaces |>> LDIV
            pstring "LDBCDV" >>. spaces >>. pRegister .>> spaces |>> LDBCDV
            pstring "LDDTV"  >>. spaces >>. pRegister .>> spaces |>> LDDTV
            pstring "LDFV"   >>. spaces >>. pRegister .>> spaces |>> LDFV
            pstring "LDIA"   >>. spaces >>. pAddr     .>> spaces |>> LDIA
            pstring "LDSTV"  >>. spaces >>. pRegister .>> spaces |>> LDSTV
            pstring "LDVI"   >>. spaces >>. pRegister .>> spaces |>> LDVI
            pstring "LDVB"   >>. spaces >>. pRegister .>> spaces .>>. pByte |>> LDVB
            pstring "LDVDT"  >>. spaces >>. pRegister .>> spaces |>> LDVDT
            pstring "LDVK"   >>. spaces >>. pRegister .>> spaces |>> LDVK
            pstring "LDVV"   >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> LDVV
            pstring "OR"     >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> OR
            pstring "RET"    >>% spaces >>% RET
            pstring "RND"    >>. spaces >>. pRegister .>> spaces .>>. pByte     |>> RND
            pstring "SEVB"   >>. spaces >>. pRegister .>> spaces .>>. pByte     |>> SEVB
            pstring "SEVV"   >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> SEVV
            pstring "SHL"    >>. spaces >>. pRegister .>> spaces |>> SHL
            pstring "SHR"    >>. spaces >>. pRegister .>> spaces |>> SHR
            pstring "SKNP"   >>. spaces >>. pRegister .>> spaces |>> SKNP
            pstring "SKP"    >>. spaces >>. pRegister .>> spaces |>> SKP
            pstring "SNEVB"  >>. spaces >>. pRegister .>> spaces .>>. pByte     |>> SNEVB
            pstring "SNEVV"  >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> SNEVV
            pstring "SUB"    >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> SUB
            pstring "SUBN"   >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> SUBN
            pstring "SYS"    >>. spaces >>. pAddr     .>> spaces |>> SYS
            pstring "XOR"    >>. spaces >>. pRegister .>> spaces .>>. pRegister |>> XOR
        ]

let pProgram file =
    match runParserOnFile (many1 (pInstruction .>> (spaces <|> skipNewline <|> eof))) () file Text.Encoding.UTF8 with
    | Success (res, _, _) -> res
    | Failure (msg, _, _) -> failwith msg

open System.IO
let compile file (out : string) =
    let bytes =
        pProgram file
        |> Array.ofList
        |> Array.collect (encodeOp >> splitWord)
    File.WriteAllBytes(out, bytes)

#if !INTERACTIVE
[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
#endif