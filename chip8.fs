open System
open System.IO

module Byte =
    let getBit (b : byte) (pos : int) =
        (b &&& (1uy <<< pos-1)) <> 0uy
    let lower (b : byte) =
        b &&& 0xF0uy
    let upper (b : byte) =
        b &&& 0x0Fuy

module Nibble =
    type Nibble = Nibble of (bool * bool * bool * bool)
    let create (b : byte) =
        let getBit = Byte.getBit b
        Nibble(getBit 4, getBit 5, getBit 6, getBit 7)
    let fromLowerByte b =
        let getLowerBit =
            b
            |> Byte.lower
            |> Byte.getBit
        Nibble(getLowerBit 4, getLowerBit 5, getLowerBit 6, getLowerBit 7)
    let fromUpperByte b =
        let getUpperBit =
            b
            |> Byte.upper
            |> Byte.getBit
        Nibble(getUpperBit 0, getUpperBit 1, getUpperBit 2, getUpperBit 3)

module Memory =
    let ram = Array.create 4096 0uy
    let stack = Array.create 16 0s

    let visualData() =
        ram.[0x00..0x1FF]

module Registers =
    let V (*General purpose*) = Array.create 16 0uy
    let mutable I (*16 bit general purpose, usually for memory addresses*) = 0s
    let mutable DT (*delay timer*) = 0uy
    let mutable ST (*Sound timer*) = 0uy
    let mutable PC (*Program counter*) = 0s
    let mutable SP (*Stack pointer*) = 0uy


module Decode =
    open System
    type MemoryAddress = uint16
    type VRegister = byte
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
        | LDBV  of VRegister                             // B, Vx
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

    let inline clearUpper s = s &&& 0x0FFFus

    let inline to8 (s : uint16) =
        BitConverter.GetBytes(s).[0]

    let inline to16 (s : byte) =
        let arr = if BitConverter.IsLittleEndian then [|s; 0x00uy|] else  [|0x00uy; s|]
        BitConverter.ToUInt16(arr, 0)

    let inline combineByte (n1 : byte) n2 =
        let arr = if BitConverter.IsLittleEndian then [|n2; n1|] else [|n1; n2|]
        BitConverter.ToUInt16(arr, 0)

    let inline combineNibble (n1 : byte) n2 =
        n1 <<< 4 ||| n2

    let splitNibbles (s : uint16) =
        let n1 = to8 (s &&& 0xF000us >>> 12)
        let n2 = to8 (s &&& 0x0F00us >>> 8 )
        let n3 = to8 (s &&& 0x00F0us >>> 4 )
        let n4 = to8 (s &&& 0x000Fus       )
        (n1, n2, n3, n4)

    let fromNibbles n1 n2 n3 n4 =
        let mutable result = 0x0000us
        result <- (result ||| (to16 n1)) <<< 4
        result <- (result ||| (to16 n2)) <<< 4
        result <- (result ||| (to16 n3)) <<< 4
        result <- (result ||| (to16 n4)) <<< 0
        result

    let inline failInvalidInstruction (s : uint16) = failwithf "Invalid instruction: 0x%X" s

    let decode (s : uint16) : Instruction =
        let instruction,d1,d2,d3 = splitNibbles s
        printfn "Processing: %X" s
        match instruction with
        | 0x00uy ->
            if s = 0x00E0us then
                CLS
            elif s = 0x00EEus then
                RET
            else
                let addr = clearUpper s
                SYS addr
        | 0x01uy ->
            let addr = clearUpper s
            JPA addr
        | 0x02uy ->
            let addr = clearUpper s
            CALL addr
        | 0x03uy ->
            let vx = d1
            let kk = combineNibble d2 d3
            SEVB(vx, kk)
        | 0x04uy ->
            let vx = d1
            let kk = combineNibble d2 d3
            SNEVB(vx, kk)
        | 0x05uy ->
            let vx = d1
            let vy = d2
            SEVV(vx, vy)
        | 0x06uy ->
            let vx = d1
            let kk = combineNibble d2 d3
            LDVB(vx, kk)
        | 0x07uy ->
            let vx = d1
            let kk = combineNibble d2 d3
            ADDVB(vx, kk)
        | 0x08uy ->
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
            | _ -> failInvalidInstruction s
        | 0x09uy ->
            if d3 = 0x00uy then
                let vx = d1
                let vy = d2
                SNEVV(vx, vy)
            else
                failInvalidInstruction s
        | 0x0Auy ->
            let addr = clearUpper s
            LDIA(addr)
        | 0x0Buy ->
            let addr = clearUpper s
            JP0A(addr)
        | 0x0Cuy ->
            let vx = d1
            let kk = combineNibble d2 d3
            RND(vx, kk)
        | 0x0Duy ->
            let vx = d1
            let vy = d2
            let  n = d3
            DRW(vx, vy, n)
        | 0x0Euy ->
            let vx = d1
            match combineNibble d2 d3 with
            | 0x9Euy ->
                SKP(vx)
            | 0xA1uy ->
                SKNP(vx)
            | _ -> failInvalidInstruction s
        | 0x0Fuy ->
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
                LDBV(vx)
            | 0x55uy ->
                LDIV(vx)
            | 0x65uy ->
                LDVI(vx)
            | _ -> failInvalidInstruction s
        | _ -> failInvalidInstruction s
