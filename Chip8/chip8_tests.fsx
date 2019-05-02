open System.Collections
#load "Extensions.fs"
#load "BitOps.fs"
#load "General.fs"
#load "Decoder.fs"

open BitOps
open Decoder
open General

let inline assertTupleEqual(a, b) =
    if a <> b then
        failwithf "Expected: %A, got %A" a b
    printfn "A = B"

let inline assertEqual a b =
    if a <> b then
        failwithf "Expected: %A, got %A" a b
    printfn "%A = %A" a b

let inline assertEqualBitArray (a:BitArray) (b:BitArray) =
    if a.Length <> b.Length then
        failwithf "Lengths do not match"
    for i in 0..a.Length-1 do
        if a.[i] <> b.[i] then
            failwithf "Expected: %A, got %A @ index %d" a.[i] b.[i] i
    printfn "%A = %A" a b

module OpDecoderTests =
    //GOOD
    [
        (0x03FFus, SYS(0x03FFus))
        (0x00E0us, CLS)
        (0x00EEus, RET)
        (0x13FFus, JPA(0x03FFus))
        (0x23FFus, CALL(0x03FFus))
        (0x31FFus, SEVB(0x01uy, 0xFFuy))
        (0x41FFus, SNEVB(0x01uy, 0xFFuy))
        (0x51F0us, SEVV(0x01uy, 0x0Fuy))
        (0x61FFus, LDVB(0x01uy, 0xFFuy))
        (0x71FFus, ADDVB(0x01uy, 0xFFuy))
        (0x81F0us, LDVV(0x01uy, 0x0Fuy))
        (0x81F1us, OR(0x01uy, 0x0Fuy))
        (0x81F2us, AND(0x01uy, 0x0Fuy))
        (0x81F3us, XOR(0x01uy, 0x0Fuy))
        (0x81F4us, ADDVV(0x01uy, 0x0Fuy))
        (0x81F5us, SUB(0x01uy, 0x0Fuy))
        (0x81F6us, SHR(0x01uy))
        (0x81F7us, SUBN(0x01uy, 0x0Fuy))
        (0x81FEus, SHL(0x01uy))
        (0x91F0us, SNEVV(0x01uy, 0x0Fuy))
        (0xA3FFus, LDIA(0x03FFus))
        (0xB3FFus, JP0A(0x03FFus))
        (0xC1FFus, RND(0x01uy, 0xFFuy))
        (0xD1FFus, DRW(0x01uy, 0x0Fuy, 0x0Fuy))
        (0xE19Eus, SKP(0x01uy))
        (0xE1A1us, SKNP(0x01uy))
        (0xF107us, LDVDT(0x01uy))
        (0xF10Aus, LDVK(0x01uy))
        (0xF115us, LDDTV(0x01uy))
        (0xF118us, LDSTV(0x01uy))
        (0xF11Eus, ADDIV(0x01uy))
        (0xF129us, LDFV(0x01uy))
        (0xF133us, LDBCDV(0x01uy))
        (0xF155us, LDIV(0x01uy))
        (0xF165us, LDVI(0x01uy))
    ]
    |> List.iter (fun (toDecode, expected) ->
        assertEqual (decodeOp toDecode) expected)

    //BAD
    [
        ((decodeOp 0xFFFFus),(BADOP(0xFFFFus)))
    ]
    |> List.iter assertTupleEqual

module OpEncoderTests =
    //GOOD
    [
        (0x03FFus, SYS(0x03FFus))
        (0x00E0us, CLS)
        (0x00EEus, RET)
        (0x13FFus, JPA(0x03FFus))
        (0x23FFus, CALL(0x03FFus))
        (0x31FFus, SEVB(0x01uy, 0xFFuy))
        (0x41FFus, SNEVB(0x01uy, 0xFFuy))
        (0x51F0us, SEVV(0x01uy, 0x0Fuy))
        (0x61FFus, LDVB(0x01uy, 0xFFuy))
        (0x71FFus, ADDVB(0x01uy, 0xFFuy))
        (0x81F0us, LDVV(0x01uy, 0x0Fuy))
        (0x81F1us, OR(0x01uy, 0x0Fuy))
        (0x81F2us, AND(0x01uy, 0x0Fuy))
        (0x81F3us, XOR(0x01uy, 0x0Fuy))
        (0x81F4us, ADDVV(0x01uy, 0x0Fuy))
        (0x81F5us, SUB(0x01uy, 0x0Fuy))
        (0x8106us, SHR(0x01uy))
        (0x81F7us, SUBN(0x01uy, 0x0Fuy))
        (0x810Eus, SHL(0x01uy))
        (0x91F0us, SNEVV(0x01uy, 0x0Fuy))
        (0xA3FFus, LDIA(0x03FFus))
        (0xB3FFus, JP0A(0x03FFus))
        (0xC1FFus, RND(0x01uy, 0xFFuy))
        (0xD1FFus, DRW(0x01uy, 0x0Fuy, 0x0Fuy))
        (0xE19Eus, SKP(0x01uy))
        (0xE1A1us, SKNP(0x01uy))
        (0xF107us, LDVDT(0x01uy))
        (0xF10Aus, LDVK(0x01uy))
        (0xF115us, LDDTV(0x01uy))
        (0xF118us, LDSTV(0x01uy))
        (0xF11Eus, ADDIV(0x01uy))
        (0xF129us, LDFV(0x01uy))
        (0xF133us, LDBCDV(0x01uy))
        (0xF155us, LDIV(0x01uy))
        (0xF165us, LDVI(0x01uy))
    ]
    |> List.iter (fun (expected, toEncode) ->
        assertEqual (encodeOp toEncode) expected)

module chipTests =
    let ramReadWrite_test =
        let chip = { Chip8.Create() with I = 0x20us }
        let toWrite = [|0x1uy; 0x2uy; 0x3uy|]
        let result = { chip with Ram = chip.WriteRam (int chip.I) toWrite }

        let expected = toWrite
        let actual = result.ReadRam (int chip.I) 3
        assertEqual expected actual

module OpExecuteTests =
    let addiv_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        let op = ADDIV(0uy)
        let expected = 0x20us + 0x20us
        let actual = (executeOp chip op).I
        assertEqual expected actual
    let addvb_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        let op = ADDVB(0uy, 0x20uy)
        let expected = 0x20uy + 0x20uy
        let actual = (executeOp chip op).V.[0]
        assertEqual expected actual
    let addvv_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        chip.V.[1] <- 0xFFuy
        let op = ADDVV(0uy, 1uy)
        let expected = 0x20uy + 0xFFuy
        let actual = (executeOp chip op).V.[0]
        assertEqual expected actual
    let and_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        chip.V.[1] <- 0x20uy
        let op = AND(0uy, 1uy)
        let expected = 0x20uy
        let actual = (executeOp chip op).V.[0]
        assertEqual expected actual
    let call_test =
        let chip = { Chip8.Create() with I = 0x20us }

        let op = CALL(0us)
        let result = (executeOp chip op)

        let expectedPC = 0us
        let actualPC = result.PC
        assertEqual expectedPC actualPC

        let expectedSP = 1uy
        let actualSP = result.SP
        assertEqual expectedSP actualSP

        let expectedRam = [| 0x02uy; 0x00uy |]
        let actualRam = result.ReadRam (int Chip8.StackBase + 2) 2
        assertEqual expectedRam actualRam
    let cls_test =
        let screen = BitArray(64*32)
        screen.SetAll(true)
        let chip = { Chip8.Create() with Screen = screen }
        let op = CLS
        let result = (executeOp chip op)
        let expected = BitArray(64*32)
        let actual = result.Screen
        for i in 0..64*32-1 do
            if expected.[i] <> actual.[i] then
                failwithf "%d" i
        assertEqualBitArray expected actual
    let drw_test = ()

    let jpa_test =
        let chip = { Chip8.Create() with I = 0x20us }
        let op = JPA(0x20us)
        let result = (executeOp chip op)
        assertEqual 0x20us result.PC
    let jp0a_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 1uy
        let op = JP0A(0x20us)
        let result = (executeOp chip op)
        assertEqual 0x21us result.PC
    let ldiv_test =
        let expected = [|for i in 0uy..15uy do yield i|]
        let chip = { Chip8.Create() with I = 0x200us }
        let chip = { chip with V = expected }
        let op = LDIV(15uy)
        let result = (executeOp chip op).ReadRam 0x200 16
        assertEqual expected result
    let ldbcdv_test =
        //TODO: NYI: NYI
        ()
    let lddtv_test =
        let chip = { Chip8.Create() with I = 0x200us }
        chip.V.[0] <- 0x20uy
        let op = LDDTV(0uy)
        let result = (executeOp chip op)
        assertEqual result.DT 0x20uy
    let ldfv_test =
        //TODO: NYI: NYI
        ()
    let ldia_test =
        let chip = { Chip8.Create() with I = 0x200us }
        let op = LDIA(0x20us)
        let result = (executeOp chip op)
        assertEqual result.I 0x20us
    let ldstv_test =
        let chip = { Chip8.Create() with I = 0x200us }
        chip.V.[0] <- 0x20uy
        let op = LDSTV(0uy)
        let result = (executeOp chip op)
        assertEqual result.ST 0x20uy
    let ldvi_test =
        let expected = [|for i in 0uy..15uy do yield 0xFFuy-i |]
        let chip = { Chip8.Create() with I = 0x200us; V = [|for i in 0uy..15uy do yield i|] }
        let chip = { Chip8.Create() with Ram = chip.WriteRam 0x200 expected }
        let op = LDVI(15uy)
        let result = (executeOp chip op).ReadRam 0x200 16
        assertEqual expected result
    let ldvb_test =
        let chip = { Chip8.Create() with I = 0x200us }
        let op = LDVB(0uy, 0x20uy)
        let result = (executeOp chip op)
        assertEqual result.V.[0] 0x20uy
    let ldvdt_test =
        let chip = { Chip8.Create() with I = 0x200us; DT = 0x20uy }
        let op = LDVDT(0uy)
        let result = (executeOp chip op)
        assertEqual result.V.[0] 0x20uy
    let ldvk_test =
        //TODO: NYI: NYI
        ()
    let ldvv_test =
        let chip = { Chip8.Create() with I = 0x200us }
        chip.V.[1] <- 0x20uy
        let op = LDVV(0uy, 1uy)
        let result = (executeOp chip op)
        assertEqual result.V.[0] 0x20uy
    let or_test =
        ()
        //TODO: NYI: NYI
        (*
            let chip = { Chip8.Create() with I = 0x200us }
            chip.V.[0] <- 0x10uy
            chip.V.[1] <- 0x20uy
            let op = OR(0uy, 1uy)
            let result = (executeOp chip op)
            assertEqual result.V.[0] 0x30uy
        *)
    let ret_test =
        let chip = { Chip8.Create() with I = 0x20us }
        let op = CALL(0us)
        let result = (executeOp chip op)
        let op = RET
        let result = (executeOp result op)

        let expectedPC = 0x200us
        let actualPC = result.PC
        assertEqual expectedPC actualPC

        let expectedSP = 0uy
        let actualSP = result.SP
        assertEqual expectedSP actualSP
    let rnd_test =
        ()
        //TODO: NYI: Test NYI
    let sevb_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        let op = SEVB(0uy, 0x20uy)
        let result = (executeOp chip op)
        assertEqual (chip.PC + 2us) result.PC

        let op = SEVB(0uy, 0x21uy)
        let result = (executeOp chip op)
        assertEqual chip.PC result.PC
    let sevv_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        chip.V.[1] <- 0x20uy
        let op = SEVV(0uy, 1uy)
        let result = (executeOp chip op)
        assertEqual (chip.PC + 2us) result.PC

        chip.V.[1] <- 0x21uy
        let op = SEVB(0uy, 1uy)
        let result = (executeOp chip op)
        assertEqual chip.PC result.PC
    let shl_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        let op = SHL(0uy)
        let result = (executeOp chip op)
        assertEqual 0x40uy result.V.[0]
        assertEqual 0uy result.V.[15]

        chip.V.[0] <- 0xFFuy
        let op = SHL(0uy)
        let result = (executeOp chip op)
        assertEqual 0xFEuy result.V.[0]
        assertEqual 1uy result.V.[15]
    let shr_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        let op = SHR(0uy)
        let result = (executeOp chip op)
        assertEqual 0x10uy result.V.[0]
        assertEqual 0uy result.V.[15]

        chip.V.[0] <- 0xFFuy
        let op = SHR(0uy)
        let result = (executeOp chip op)
        assertEqual 0x7Fuy result.V.[0]
        assertEqual 1uy result.V.[15]
    let sknp_test =
        //TODO: NYI: Test NYI
        ()
    let skp_test =
        //TODO: NYI: Test NYI
        ()
    let snevb_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        let op = SNEVB(0uy, 0x21uy)
        let result = (executeOp chip op)
        assertEqual (chip.PC + 2us) result.PC

        let op = SNEVB(0uy, 0x20uy)
        let result = (executeOp chip op)
        assertEqual chip.PC result.PC
    let snevv_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        chip.V.[1] <- 0x21uy
        let op = SNEVV(0uy, 1uy)
        let result = (executeOp chip op)
        assertEqual (chip.PC + 2us) result.PC

        chip.V.[1] <- 0x20uy
        let op = SNEVV(0uy, 1uy)
        let result = (executeOp chip op)
        assertEqual chip.PC result.PC
    let sub_test =
        let chip = { Chip8.Create() with I = 0x20us }
        chip.V.[0] <- 0x20uy
        chip.V.[1] <- 0x19uy
        let op = SUB(0uy, 1uy)
        let result = (executeOp chip op)
        assertEqual (0x20uy-0x19uy) result.V.[0]
        assertEqual 1uy result.V.[15]

        chip.V.[0] <- 0x19uy
        chip.V.[1] <- 0x20uy
        let op = SUB(0uy, 1uy)
        let result = (executeOp chip op)
        assertEqual (0x19uy-0x20uy) result.V.[0]
        assertEqual 0uy result.V.[15]
    let subn_test =
        //TODO: NYI: NYI
        ()
    let sys_test =
        //TODO: NYI: NYI
        ()
    let xor_test =
        let chip = { Chip8.Create() with I = 0x200us }
        chip.V.[0] <- 0x10uy
        chip.V.[1] <- 0x20uy
        let op = XOR(0uy, 1uy)
        let result = (executeOp chip op)
        assertEqual (0x10uy ^^^ 0x20uy) result.V.[0]
    let badop_test =
        //TODO: NYI: NYI
        ()










