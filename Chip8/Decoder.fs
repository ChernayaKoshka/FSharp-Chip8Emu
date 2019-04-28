module Decoder

#if INTERACTIVE
#load "Extensions.fs"
#load "BitOps.fs"
#load "General.fs"
#endif

open Extensions
open Extensions.Printf
open BitOps
open General

open System
open System.Collections
open System.IO
open Extensions

type Instruction =
    | ADDIV of VRegister                             // I, Vx
    | ADDVB of VRegister * byte                      // Vx, byte
    | ADDVV of VRegister * VRegister                 // Vx, Vy
    | AND   of VRegister * VRegister                 // Vx, Vy
    | CALL  of MemoryAddress                         // addr
    | CLS                                            //
    | DRW   of VRegister * VRegister * byte          // Vx, Vy, nibble
    | JPA   of MemoryAddress                         // addr
    | JP0A  of MemoryAddress                         // V0, addr
    | LDIV  of VRegister                             // [I], Vx
    | LDBCDV  of VRegister                             // B, Vx
    | LDDTV of VRegister                             // DT, Vx
    | LDFV  of VRegister                             // F, Vx
    | LDIA  of MemoryAddress                         // I, addr
    | LDSTV of VRegister                             // ST, Vx
    | LDVI  of VRegister                             // Vx, [I]
    | LDVB  of VRegister * byte                      // Vx, byte
    | LDVDT of VRegister                             // Vx, DT
    | LDVK  of VRegister                             // Vx, K
    | LDVV  of VRegister * VRegister                 // Vx, Vy
    | OR    of VRegister * VRegister                 // Vx, Vy
    | RET                                            //
    | RND   of VRegister * byte                      // Vx, byte
    | SEVB  of VRegister * byte                      // Vx, byte
    | SEVV  of VRegister * VRegister                 // Vx, Vy
    | SHL   of VRegister                             // Vx {, Vy}
    | SHR   of VRegister                             // Vx {, Vy}
    | SKNP  of VRegister                             // Vx
    | SKP   of VRegister                             // Vx
    | SNEVB of VRegister * byte                      // Vx, byte
    | SNEVV of VRegister * VRegister                 // Vx, Vy
    | SUB   of VRegister * VRegister                 // Vx, Vy
    | SUBN  of VRegister * VRegister                 // Vx, Vy
    | SYS   of MemoryAddress                         // addr
    | XOR   of VRegister * VRegister                 // Vx, Vy
    | BADOP of MemoryAddress

let decodeOp (s : uint16) : Instruction =
    let instruction,d1,d2,d3 = splitNibbles s
    //printfn "Processing: %X" s
    match instruction with
    | 0x0uy ->
        if s = 0x00E0us then
            CLS
        elif s = 0x00EEus then
            RET
        else
            let addr = clearUpperNibble s
            SYS addr
    | 0x1uy ->
        let addr = clearUpperNibble s
        JPA addr
    | 0x2uy ->
        let addr = clearUpperNibble s
        CALL addr
    | 0x3uy ->
        let vx = d1
        let kk = combineNibble d2 d3
        SEVB(vx, kk)
    | 0x4uy ->
        let vx = d1
        let kk = combineNibble d2 d3
        SNEVB(vx, kk)
    | 0x5uy ->
        let vx = d1
        let vy = d2
        SEVV(vx, vy)
    | 0x6uy ->
        let vx = d1
        let kk = combineNibble d2 d3
        LDVB(vx, kk)
    | 0x7uy ->
        let vx = d1
        let kk = combineNibble d2 d3
        ADDVB(vx, kk)
    | 0x8uy ->
        let vx = d1
        let vy = d2
        let  n = d3
        match n with
        | 0x00uy ->
            LDVV(vx, vy)
        | 0x01uy ->
            OR(vx, vy)
        | 0x02uy ->
            AND(vx, vy)
        | 0x03uy ->
            XOR(vx, vy)
        | 0x04uy ->
            ADDVV(vx, vy)
        | 0x05uy ->
            SUB(vx, vy)
        | 0x06uy ->
            SHR(vx)
        | 0x07uy ->
            SUBN(vx, vy)
        | 0x0Euy ->
            SHL(vx)
        | _ -> BADOP(s)
    | 0x9uy ->
        if d3 = 0x00uy then
            let vx = d1
            let vy = d2
            SNEVV(vx, vy)
        else
            BADOP(s)
    | 0xAuy ->
        let addr = clearUpperNibble s
        LDIA(addr)
    | 0xBuy ->
        let addr = clearUpperNibble s
        JP0A(addr)
    | 0xCuy ->
        let vx = d1
        let kk = combineNibble d2 d3
        RND(vx, kk)
    | 0xDuy ->
        let vx = d1
        let vy = d2
        let  n = d3
        DRW(vx, vy, n)
    | 0xEuy ->
        let vx = d1
        match combineNibble d2 d3 with
        | 0x9Euy ->
            SKP(vx)
        | 0xA1uy ->
            SKNP(vx)
        | _ -> BADOP(s)
    | 0xFuy ->
        let vx = d1
        match combineNibble d2 d3 with
        | 0x07uy ->
            LDVDT(vx)
        | 0x0Auy ->
            LDVK(vx)
        | 0x15uy ->
            LDDTV(vx)
        | 0x18uy ->
            LDSTV(vx)
        | 0x1Euy ->
            ADDIV(vx)
        | 0x29uy ->
            LDFV(vx)
        | 0x33uy ->
            LDBCDV(vx)
        | 0x55uy ->
            LDIV(vx)
        | 0x65uy ->
            LDVI(vx)
        | _ -> BADOP(s)
    | _ -> BADOP(s)

let rand = Random()
let executeOp (chip:Chip8) (op : Instruction) =
    match op with
    | ADDIV(Vx)->
        { chip with I = chip.I + uint16 chip.V.[int Vx] }
    | ADDVB(Vx, kk)->
        { chip with V = Array.copySet chip.V (int Vx) ( kk + chip.V.[int Vx]) }
    | ADDVV(Vx, Vy)->
        let addResult = uint16 chip.V.[int Vx] + uint16 chip.V.[int Vy]
        let newRegisters =
            if addResult > 0xFFus then
                Array.copySet chip.V (0x0F) 1uy
            else
                Array.copySet chip.V (0x0F) 0uy
        let newRegisters = Array.copySet newRegisters (int Vx) (byte addResult)
        { chip with V = newRegisters }
    | AND(Vx, Vy)->
        { chip with V = Array.copySet chip.V (int Vx) (chip.V.[int Vx] &&& chip.V.[int Vy]) }
    | CALL(addr)->
        let newSP = chip.SP + 1uy
        { chip with PC = addr; SP = newSP; Ram = chip.WriteRam (int newSP * 2 + int Chip8.StackBase) (splitWord chip.PC) }
    | CLS->
        { chip with Screen = BitArray(64*32) }
    | DRW(Vx, Vy, n)->
        let xPos = uint16 chip.V.[int Vx]
        let yPos = uint16 chip.V.[int Vy]

        let spriteData = bytesToBits (chip.ReadRam (int chip.I) (int n))

        let newScreen,collision = drawSprite chip.Screen spriteData xPos yPos

        if collision then
            { chip with Screen = newScreen; V = Array.copySet chip.V 0xF 0x1uy }
        else
            { chip with Screen = newScreen; V = Array.copySet chip.V 0xF 0x0uy }
    | JPA(addr)->
        { chip with PC = addr }
    | JP0A(addr)->
        { chip with PC = addr + (uint16 chip.V.[0]) }
    | LDIV(Vx)->
        let written = chip.WriteRam (int chip.I) chip.V.[0..int Vx]
        { chip with Ram = written }
    | LDBCDV(Vx)->
        let str = chip.V.[int Vx].ToString() //hack, but I don't feel like dealing with it
        let bcd =
            if str.Length = 3 then
                let hundreds = Byte.Parse(string str.[0])
                let tens = Byte.Parse(string str.[1])
                let ones = Byte.Parse(string str.[2])
                [|hundreds; tens; ones|]
            elif str.Length = 2 then
                let tens = Byte.Parse(string str.[0])
                let ones = Byte.Parse(string str.[1])
                [|0uy; tens; ones|]
            elif str.Length = 1 then
                let ones = Byte.Parse(string str.[0])
                [|0uy; 0uy; ones|]
            else failwith "Not possible"
        { chip with Ram = chip.WriteRam (int chip.I) bcd }
    | LDDTV(Vx)->
        { chip with DT = chip.V.[int Vx] }
    | LDFV(Vx)->
        { chip with I = uint16 chip.V.[int Vx] * 5us }
    | LDIA(addr)->
         { chip with I = addr }
    | LDSTV(Vx)->
         { chip with ST = chip.V.[int Vx] }
    | LDVI(Vx)->
        let numRead = int Vx + 1
        let read = chip.ReadRam (int chip.I) numRead
        let newRegisters = Array.copyBlit read 0 chip.V 0 numRead
        { chip with V = newRegisters }
    | LDVB(Vx, kk)->
        { chip with V = Array.copySet chip.V (int Vx) kk }
    | LDVDT(Vx)->
        { chip with V = Array.copySet chip.V (int Vx) chip.DT }
    | LDVK(Vx) ->
        let key = Chip8.GetAnyKeyPress()
        { chip with V = Array.copySet chip.V (int Vx) key }
    | LDVV(Vx, Vy)->
        { chip with V = Array.copySet chip.V (int Vx) (chip.V.[int Vy]) }
    | OR(Vx, Vy)->
        let vXVal = chip.V.[int Vx]
        let vYVal = chip.V.[int Vy]
        let res = vXVal ||| vYVal
        { chip with V = Array.copySet chip.V (int Vx) res }
    | RET->
        let stackAddr = int chip.SP * 2 + int Chip8.StackBase
        let priorAddr = combineByteArr <| chip.ReadRam stackAddr 2
        { chip with PC = priorAddr; SP = chip.SP - 1uy }
    | RND(Vx, kk)->
        let rndArr = [|0uy|]
        rand.NextBytes(rndArr)
        let rnd = rndArr.[0] &&& kk
        { chip with V = Array.copySet chip.V (int Vx) rnd }
    | SEVB(Vx, kk)->
        if chip.V.[int Vx] = kk then
            { chip with PC = chip.PC + 2us }
        else
            chip
    | SEVV(Vx, Vy)->
        if chip.V.[int Vx] = chip.V.[int Vy] then
            { chip with PC = chip.PC + 2us }
        else
            chip
    | SHL(Vx)->
        let carry = if (bytesToBits [|chip.V.[int Vx]|]).[0] then 1uy else 0uy
        let v' = Array.copySet chip.V (int Vx) (chip.V.[int Vx] <<< 1)
        let v'' = Array.copySet v' 15 carry
        { chip with V = v'' }
    | SHR(Vx)->
        let carry = if (bytesToBits [|chip.V.[int Vx]|]).[7] then 1uy else 0uy
        let v' = Array.copySet chip.V (int Vx) (chip.V.[int Vx] >>> 1)
        let v'' = Array.copySet v' 15 carry
        { chip with V = v'' }
    | SKNP(Vx)->
        if not <| Chip8.IsKeyPressed chip.V.[int Vx] then
            { chip with PC = chip.PC + 2us }
        else
            chip
    | SKP(Vx)->
        if Chip8.IsKeyPressed chip.V.[int Vx] then
            { chip with PC = chip.PC + 2us }
        else
            chip
    | SNEVB(Vx, kk)->
        if chip.V.[int Vx] <> kk then
            { chip with PC = chip.PC + 2us }
        else
            chip
    | SNEVV(Vx, Vy)->
        if chip.V.[int Vx] <> chip.V.[int Vy] then
            { chip with PC = chip.PC + 2us }
        else
            chip
    | SUB(Vx, Vy)->
        let vXVal = chip.V.[int Vx]
        let vYVal = chip.V.[int Vy]
        let carry = if vXVal > vYVal then 1uy else 0uy
        let res = vXVal - vYVal
        let v' =  Array.copySet chip.V (int Vx) res
        let v'' =  Array.copySet v' 15 carry
        { chip with V = v'' }
    | SUBN(Vx, Vy)->
        let vXVal = chip.V.[int Vx]
        let vYVal = chip.V.[int Vy]
        let carry = if vYVal > vXVal then 1uy else 0uy
        let res = vYVal - vXVal
        let v' =  Array.copySet chip.V (int Vx) res
        let v'' =  Array.copySet v' 15 carry
        { chip with V = v'' }
    | SYS(addr)->
        chip
    | XOR(Vx, Vy)->
        let vXVal = chip.V.[int Vx]
        let vYVal = chip.V.[int Vy]
        let res = vXVal ^^^ vYVal
        { chip with V = Array.copySet chip.V (int Vx) res }
    | BADOP(addr)->
        failwithf "%A" op

let readFile file = File.ReadAllBytes(file)

let decodeMult (bytes : byte[]) =
    bytes
    |> Array.chunkBySize 2
    |> Array.filter (fun arr -> arr.Length = 2)
    |> Array.map (fun bytes ->
        combineByte bytes.[0] bytes.[1]
        |> decodeOp)

let decodeFile = readFile >> decodeMult

let dumpFile file =
    let decoded = decodeFile file
    use writer = new StreamWriter(File.OpenWrite("dump.txt"))
    Array.iteri (fun i op -> writer.WriteLine(sprintf "% 6d\t%A" (i*2) op)) decoded

let mutable breakpointSet = false
let mutable breakpoint = 0x200us
let runFile debug file =
    let bytes = readFile file
    let chip = Chip8.Create().LoadProgram bytes
    let timer = External.Time.HighResTimer()
    let rec next (chip : Chip8) timeAccumulated =
        let accumulated = timeAccumulated + timer.DeltaTime
        if accumulated >= Chip8.Frequency then
            let readNext  = chip.ReadRam (int chip.PC) 2
            let nextInstr =
                readNext
                |> combineByteArr
                |> decodeOp
            let nextState =
                executeOp
                    {
                        chip with
                            PC = chip.PC + 2us;
                            DT = if chip.DT <> 0uy then chip.DT - 1uy else 0uy;
                            ST = if chip.ST <> 0uy then chip.ST - 1uy else 0uy;
                    } nextInstr

            if debug then
                if not breakpointSet || (breakpointSet && chip.PC = breakpoint) then
                    breakpointSet <- false
                    printfn "Executed %A with a result of:" nextInstr
                    printf "V: "

                    Array.iteri2 (fun i v v' ->
                        cprintfDiff v v' "%d : %d | " i v') chip.V nextState.V
                    printfn ""

                    printf "PC: %d, " nextState.PC
                    cprintfDiff chip.SP nextState.SP "SP: %d, " nextState.SP
                    cprintfDiff chip.I nextState.I "I: %d" nextState.I
                    cprintfDiff chip.DT nextState.DT "DT: %d" nextState.DT
                    cprintfDiff chip.ST nextState.ST "ST: %d" nextState.ST
                    printfn ""

                    printfn "Next3: %A" (decodeMult (nextState.ReadRam (int nextState.PC) 6))
                    printfn "Ram [%d-%d]: %A" (int nextState.I) ((int nextState.I) + 9) (nextState.ReadRam (int nextState.I) 10)
                    printfn "Execute next until?"
                    let input = Console.ReadLine()
                    match UInt16.TryParse(input) with
                    | (true, num) ->
                        breakpointSet <- true
                        breakpoint <- num
                    | _ -> ()

            if nextState.Screen <> chip.Screen then
                updateScreen chip.Screen nextState.Screen debug
            next nextState (accumulated - Chip8.Frequency)
        else
            next chip accumulated
    next chip 0.0
    ()