#load "Decoder.fs"

open Decoder
let inline assertEqual(a, b) =
    if a <> b then
        failwithf "Expected: %A, got %A" a b
//GOOD
[
    ((decodeOp 0x03FFus), (SYS(0x03FFus)))
    ((decodeOp 0x00E0us), (CLS))
    ((decodeOp 0x00EEus), (RET))
    ((decodeOp 0x13FFus), (JPA(0x03FFus)))
    ((decodeOp 0x23FFus), (CALL(0x03FFus)))
    ((decodeOp 0x31FFus), (SEVB(0x01uy, 0xFFuy)))
    ((decodeOp 0x41FFus), (SNEVB(0x01uy, 0xFFuy)))
    ((decodeOp 0x51F0us), (SEVV(0x01uy, 0x0Fuy)))
    ((decodeOp 0x61FFus), (LDVB(0x01uy, 0xFFuy)))
    ((decodeOp 0x71FFus), (ADDVB(0x01uy, 0xFFuy)))
    ((decodeOp 0x81F0us), (LDVV(0x01uy, 0x0Fuy)))
    ((decodeOp 0x81F1us), (OR(0x01uy, 0x0Fuy)))
    ((decodeOp 0x81F2us), (AND(0x01uy, 0x0Fuy)))
    ((decodeOp 0x81F3us), (XOR(0x01uy, 0x0Fuy)))
    ((decodeOp 0x81F4us), (ADDVV(0x01uy, 0x0Fuy)))
    ((decodeOp 0x81F5us), (SUB(0x01uy, 0x0Fuy)))
    ((decodeOp 0x81F6us), (SHR(0x01uy)))
    ((decodeOp 0x81F7us), (SUBN(0x01uy, 0x0Fuy)))
    ((decodeOp 0x81FEus), (SHL(0x01uy)))
    ((decodeOp 0x91F0us), (SNEVV(0x01uy, 0x0Fuy)))
    ((decodeOp 0xA3FFus), (LDIA(0x03FFus)))
    ((decodeOp 0xB3FFus), (JP0A(0x03FFus)))
    ((decodeOp 0xC1FFus), (RND(0x01uy, 0xFFuy)))
    ((decodeOp 0xD1FFus), (DRW(0x01uy, 0x0Fuy, 0x0Fuy)))
    ((decodeOp 0xE19Eus), (SKP(0x01uy)))
    ((decodeOp 0xE1A1us), (SKNP(0x01uy)))
    ((decodeOp 0xF107us), (LDVDT(0x01uy)))
    ((decodeOp 0xF10Aus), (LDVK(0x01uy)))
    ((decodeOp 0xF115us), (LDDTV(0x01uy)))
    ((decodeOp 0xF118us), (LDSTV(0x01uy)))
    ((decodeOp 0xF11Eus), (ADDIV(0x01uy)))
    ((decodeOp 0xF129us), (LDFV(0x01uy)))
    ((decodeOp 0xF133us), (LDBV(0x01uy)))
    ((decodeOp 0xF155us), (LDIV(0x01uy)))
    ((decodeOp 0xF165us), (LDVI(0x01uy)))
]
|> List.iter assertEqual

//BAD
[
    ((decodeOp 0xFFFFus),(BADOP(0xFFFFus)))
]
|> List.iter assertEqual