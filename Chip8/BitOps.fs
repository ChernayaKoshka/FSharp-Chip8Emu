module BitOps

open Extensions
open System
open System.Collections

let inline clearUpperNibble s = s &&& 0x0FFFus

let inline combineByte (n1 : byte) n2 =
    let arr = if BitConverter.IsLittleEndian then [|n2; n1|] else [|n1; n2|]
    BitConverter.ToUInt16(arr, 0)

let inline combineByteArr (arr:Byte[]) =
    let arr = if BitConverter.IsLittleEndian then [|arr.[1]; arr.[0]|] else [|arr.[0]; arr.[1]|]
    BitConverter.ToUInt16(arr, 0)

let inline splitWord ( word : uint16 ) =
    let converted = BitConverter.GetBytes(word)
    if BitConverter.IsLittleEndian then
        Array.rev converted
    else
        converted

let inline combineNibble (n1 : byte) n2 =
    n1 <<< 4 ||| n2

let splitNibbles (s : uint16) =
    let n1 = byte (s &&& 0xF000us >>> 12)
    let n2 = byte (s &&& 0x0F00us >>> 8 )
    let n3 = byte (s &&& 0x00F0us >>> 4 )
    let n4 = byte (s &&& 0x000Fus       )
    (n1, n2, n3, n4)

let bytesToBits (bytes:byte[]) =
    if BitConverter.IsLittleEndian then
        BitArray(bytes).Rev()
    else
        BitArray(bytes)
