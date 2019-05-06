#if !INTERACTIVE
module Assembler
#endif

#if INTERACTIVE
#r @"..\packages\FParsec\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec\lib\net40-client\FParsec.dll"
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
open System.Text

type UserState =
    {
        BytesSoFar : uint16
        Labels : Map<string, uint16>
    }
type Parser<'t> = Parser<'t, UserState>

type Parsed =
    | Opcode of Instruction
    | NeedsLabel of (Instruction * string)
    | Data of byte list

type Processed =
    | Opcode of Instruction
    | Data of byte list

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

/// Adds a label to the map with the position equal to the # of bytes processed so far + the program base (0x200)
let assignLabel label : Parser<_> =
    updateUserState (fun us -> { us with Labels = us.Labels.Add(label, us.BytesSoFar) })

/// Parses 1 or more ascii characters
let pWord = many1Chars asciiLetter

/// Parses a label, used as a jump / call point
let pLabel : Parser<_> =
    pchar ':'
    >>. pWord
    >>= assignLabel
    >>. skipRestOfLine true

/// Parse a single space
let space : Parser<_> = pchar ' '

/// Skip one or more spaces
let skip1Spaces : Parser<_> = many1Chars space

/// Parses any valid Chip8 instruction
let pInstruction : Parser<_> =
    choice
        [
            pstring "ADDIV"  >>. skip1Spaces >>. pRegister    |>> ADDIV
            pstring "ADDVB"  >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pByte     |>> ADDVB
            pstring "ADDVV"  >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pRegister |>> ADDVV
            pstring "AND"    >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pRegister |>> AND
            pstring "CLS"    >>% CLS
            pstring "DRW"    >>. skip1Spaces >>. pipe3 (pRegister .>> skip1Spaces) (pRegister .>> skip1Spaces) pByte (fun r1 r2 b -> DRW(r1, r2, b))
            pstring "JP0A"   >>. skip1Spaces >>. pAddress     |>> JP0A
            pstring "LDIV"   >>. skip1Spaces >>. pRegister    |>> LDIV
            pstring "LDBCDV" >>. skip1Spaces >>. pRegister    |>> LDBCDV
            pstring "LDDTV"  >>. skip1Spaces >>. pRegister    |>> LDDTV
            pstring "LDFV"   >>. skip1Spaces >>. pRegister    |>> LDFV
            pstring "LDSTV"  >>. skip1Spaces >>. pRegister    |>> LDSTV
            pstring "LDVI"   >>. skip1Spaces >>. pRegister    |>> LDVI
            pstring "LDVB"   >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pByte |>> LDVB
            pstring "LDVDT"  >>. skip1Spaces >>. pRegister    |>> LDVDT
            pstring "LDVK"   >>. skip1Spaces >>. pRegister    |>> LDVK
            pstring "LDVV"   >>. skip1Spaces >>. pRegister    .>>. pRegister |>> LDVV
            pstring "OR"     >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pRegister |>> OR
            pstring "RET"    >>% RET
            pstring "RND"    >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pByte     |>> RND
            pstring "SEVB"   >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pByte     |>> SEVB
            pstring "SEVV"   >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pRegister |>> SEVV
            pstring "SHL"    >>. skip1Spaces >>. pRegister    |>> SHL
            pstring "SHR"    >>. skip1Spaces >>. pRegister    |>> SHR
            pstring "SKNP"   >>. skip1Spaces >>. pRegister    |>> SKNP
            pstring "SKP"    >>. skip1Spaces >>. pRegister    |>> SKP
            pstring "SNEVB"  >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pByte     |>> SNEVB
            pstring "SNEVV"  >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pRegister |>> SNEVV
            pstring "SUB"    >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pRegister |>> SUB
            pstring "SUBN"   >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pRegister |>> SUBN
            pstring "SYS"    >>. skip1Spaces >>. pAddress     |>> SYS
            pstring "XOR"    >>. skip1Spaces >>. pRegister    .>> skip1Spaces .>>. pRegister |>> XOR
        ]
    .>> updateByteCount 2us
    |>> Parsed.Opcode

let pInstructionWithLabel : Parser<_> =
    choice
        [
            pstring "CALL"   >>. skip1Spaces >>. pWord |>> (fun label -> CALL 0us, label)
            pstring "JPA"    >>. skip1Spaces >>. pWord |>> (fun label -> JPA 0us, label)
            pstring "LDIA"   >>. skip1Spaces >>. pWord |>> (fun label -> LDIA 0us, label)
        ]
    .>> updateByteCount 2us
    |>> Parsed.NeedsLabel

/// Parse a single-line comment starting with ';'
let pComment : Parser<_> =
    pchar ';'
    >>. restOfLine false

/// An array of whitespace characters NOT included newlines
let whitespaceChars = seq {yield ' '; yield '\t'; yield '\v'; yield '\f'}

/// Parse 1 or more whitespaces characters and returning the concatenated result
let pWhitespace = many1 (anyOf whitespaceChars) |>> (fun chars -> String.Concat(chars))

/// Parse a group of raw data (hex / binary / number) and is inserted directly as-is into the end-result
let pRawData : Parser<_> =
    pstring ".RAW"
    .>> pWhitespace
    >>. sepBy pByte space
    >>= (fun bytes ->
        updateByteCount (uint16 bytes.Length)
        >>. preturn (Parsed.Data bytes))

/// Parses ASCII text into byte-form and is inserted into the end-result
let pAsciiData : Parser<_> =
    pstring ".ASCII"
    .>> space
    >>. restOfLine false
    >>= (fun str ->
        updateByteCount (uint16 <| Encoding.ASCII.GetByteCount(str))
        >>. preturn (
                Encoding.ASCII.GetBytes(str)
                |> List.ofArray
                |> Parsed.Data))

/// Skips newlines / comments / whitespace
let skipWhitespace =
    (newline |>> string) <|> pComment <|> pWhitespace
    |>> ignore

/// Parse 1 or more skipWhitespace or Label
let pNotInstruction =
    many (skipWhitespace <|> pLabel)

/// Parses a chip8 assembly file and returns the instruction list / user state
let pAssembly contents =
    match runParserOnString
            (
                many1 (pNotInstruction >>. (pInstruction <|> pInstructionWithLabel <|> pRawData <|> pAsciiData) .>> pNotInstruction)
                .>> eof
            )
            { BytesSoFar = Chip8.ProgramBase ; Labels = Map.empty }
        String.Empty
        contents with
    | Success (res, state, _) ->
        let processed =
            res
            |> List.map (fun parsed ->
                match parsed with
                | NeedsLabel (op, label) ->
                    match op with
                    | JPA _ -> JPA (state.Labels.[label])
                    | CALL _ -> CALL (state.Labels.[label])
                    | LDIA _ -> LDIA (state.Labels.[label])
                    |> Processed.Opcode
                | Parsed.Opcode op -> Processed.Opcode op
                | Parsed.Data data -> Processed.Data data)
        processed
    | Failure (msg, _, _) -> failwith msg

let assemble instructions =
    instructions
    |> Array.ofList
    |> Array.collect (fun parsed ->
        match parsed with
        | Opcode op -> op |> (encodeOp >> splitWord)
        | Data bytes -> Array.ofList bytes)

/// Parses a chip8 assembly file and writes it out to the specified file
let compile file (out : string) =
    let bytes =
        File.ReadAllText(file)
        |> pAssembly
        |> assemble
    File.WriteAllBytes(out, bytes)
