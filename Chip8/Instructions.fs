module Instructions

open Extensions
open Extensions.Printf
open BitOps
open General

open System
open System.Collections
open System.IO

type Instruction =
    | ADDIV  of Register                   // I, Vx
    | ADDVB  of Register * byte            // Vx, byte
    | ADDVV  of Register * Register        // Vx, Vy
    | AND    of Register * Register        // Vx, Vy
    | CALL   of MemoryAddress              // addr
    | CLS                                  //
    | DRW    of Register * Register * byte // Vx, Vy, nibble
    | JPA    of MemoryAddress              // addr
    | JP0A   of MemoryAddress              // V0, addr
    | LDIV   of Register                   // [I], Vx
    | LDBCDV of Register                   // B, Vx
    | LDDTV  of Register                   // DT, Vx
    | LDFV   of Register                   // F, Vx
    | LDIA   of MemoryAddress              // I, addr
    | LDSTV  of Register                   // ST, Vx
    | LDVI   of Register                   // Vx, [I]
    | LDVB   of Register * byte            // Vx, byte
    | LDVDT  of Register                   // Vx, DT
    | LDVK   of Register                   // Vx, K
    | LDVV   of Register * Register        // Vx, Vy
    | OR     of Register * Register        // Vx, Vy
    | RET                                  //
    | RND    of Register * byte            // Vx, byte
    | SEVB   of Register * byte            // Vx, byte
    | SEVV   of Register * Register        // Vx, Vy
    | SHL    of Register                   // Vx {, Vy}
    | SHR    of Register                   // Vx {, Vy}
    | SKNP   of Register                   // Vx
    | SKP    of Register                   // Vx
    | SNEVB  of Register * byte            // Vx, byte
    | SNEVV  of Register * Register        // Vx, Vy
    | SUB    of Register * Register        // Vx, Vy
    | SUBN   of Register * Register        // Vx, Vy
    | SYS    of MemoryAddress              // addr
    | XOR    of Register * Register        // Vx, Vy
    | BADOP  of MemoryAddress              //

let decodeOp (s : uint16) : Instruction =
    let instruction, d1, d2, d3 = splitNibbles s
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

let encodeOp (instruction : Instruction) :  uint16 =
    match instruction with
    | CLS -> 0x00E0us
    | RET -> 0x00EEus
    | SYS nnn ->
        0x0000us ||| nnn
    | JPA addr ->
        0x1000us ||| addr
    | CALL addr ->
        0x2000us ||| addr
    | SEVB(vx, kk) ->
        0x3000us ||| combineByte vx kk
    | SNEVB(vx, kk) ->
        0x4000us ||| combineByte vx kk
    | SEVV(vx, vy) ->
        0x5000us ||| (uint16 (combineNibble vx vy) <<< 4)
    | LDVB(vx, kk) ->
        0x6000us ||| combineByte vx kk
    | ADDVB(vx, kk) ->
        0x7000us ||| combineByte vx kk
    | LDVV(vx, vy) ->
        0x8000us ||| (uint16 (combineNibble vx vy) <<< 4)
    | OR(vx, vy) ->
        0x8001us ||| (uint16 (combineNibble vx vy) <<< 4)
    | AND(vx, vy) ->
        0x8002us ||| (uint16 (combineNibble vx vy) <<< 4)
    | XOR(vx, vy) ->
        0x8003us ||| (uint16 (combineNibble vx vy) <<< 4)
    | ADDVV(vx, vy) ->
        0x8004us ||| (uint16 (combineNibble vx vy) <<< 4)
    | SUB(vx, vy) ->
        0x8005us ||| (uint16 (combineNibble vx vy) <<< 4)
    | SHR vx ->
        0x8006us ||| (uint16 (combineNibble vx 0uy) <<< 4)
    | SUBN(vx, vy) ->
        0x8007us ||| (uint16 (combineNibble vx vy) <<< 4)
    | SHL vx ->
        0x800Eus ||| (uint16 (combineNibble vx 0uy) <<< 4)
    | SNEVV(vx, vy) ->
        0x9000us ||| (uint16 (combineNibble vx vy) <<< 4)
    | LDIA addr ->
        0xA000us ||| addr
    | JP0A addr ->
        0xB000us ||| addr
    | RND(vx, kk) ->
        0xC000us ||| combineByte vx kk
    | DRW(vx, vy, n) ->
        0xD000us ||| (uint16 (combineNibble vx vy) <<< 4) ||| uint16 n
    | SKP vx ->
        0xE09Eus ||| ((uint16 vx) <<< 8)
    | SKNP vx ->
        0xE0A1us ||| ((uint16 vx) <<< 8)
    | LDVDT vx ->
        0xF007us ||| ((uint16 vx) <<< 8)
    | LDVK vx ->
        0xF00Aus ||| ((uint16 vx) <<< 8)
    | LDDTV vx ->
        0xF015us ||| ((uint16 vx) <<< 8)
    | LDSTV vx ->
        0xF018us ||| ((uint16 vx) <<< 8)
    | ADDIV vx ->
        0xF01Eus ||| ((uint16 vx) <<< 8)
    | LDFV vx ->
        0xF029us ||| ((uint16 vx) <<< 8)
    | LDBCDV vx ->
        0xF033us ||| ((uint16 vx) <<< 8)
    | LDIV vx ->
        0xF055us ||| ((uint16 vx) <<< 8)
    | LDVI vx ->
        0xF065us ||| ((uint16 vx) <<< 8)
    | BADOP op -> op

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
        let result = vXVal - vYVal
        let v' =  Array.copySet chip.V (int Vx) result
        let v'' =  Array.copySet v' 15 carry
        { chip with V = v'' }
    | SUBN(Vx, Vy)->
        let vXVal = chip.V.[int Vx]
        let vYVal = chip.V.[int Vy]
        let carry = if vYVal > vXVal then 1uy else 0uy
        let result = vYVal - vXVal
        let v' =  Array.copySet chip.V (int Vx) result
        let v'' =  Array.copySet v' 15 carry
        { chip with V = v'' }
    | SYS(addr)->
        chip
    | XOR(Vx, Vy)->
        let vXVal = chip.V.[int Vx]
        let vYVal = chip.V.[int Vy]
        let result = vXVal ^^^ vYVal
        { chip with V = Array.copySet chip.V (int Vx) result }
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

let mutable breakpointSet = false
let mutable breakpoint = 0x200us
let runFile debug file =
    let bytes = readFile file
    let chip = Chip8.Create().LoadProgram bytes
    let timer = External.Time.HighResTimer()

    printFirstScreen()

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

            /// Selectively update the screen to prevent ugly console flicker
            if nextState.Screen <> chip.Screen then
                updateScreen chip.Screen nextState.Screen debug
            next nextState (accumulated - Chip8.Frequency)
        else
            next chip accumulated
    next chip 0.0
    ()