module BitOps

open System
open System.Collections
open Extensions

let inline clearUpperNibble s = s &&& 0x0FFFus

let inline to8 (s : uint16) =
    BitConverter.GetBytes(s).[0]

let inline to16 (s : byte) =
    let arr = if BitConverter.IsLittleEndian then [|s; 0x00uy|] else  [|0x00uy; s|]
    BitConverter.ToUInt16(arr, 0)

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

let bytesToBits (bytes:byte[]) =
    if BitConverter.IsLittleEndian then
        BitArray(bytes).ToBigEndian()
    else
        BitArray(bytes)
