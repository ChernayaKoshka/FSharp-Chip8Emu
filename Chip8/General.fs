module General

open System
open Extensions
open BitOps
open System.Collections
open System.Windows.Forms


type MemoryAddress = uint16
type VRegister = byte

type Chip8 =
     {
        Screen : BitArray

        Ram : byte[]
        //registers
        //General purpose, V16 is used for flags
        V : VRegister[]
        //General purpose, used for addresses
        I : MemoryAddress
        //General purpose timer
        DT : VRegister
        //Sound timer, plays continuous tone and ticks downwards every 60 cycles
        ST : VRegister
        //Program counter, points to current/next instruction
        PC : MemoryAddress
        //Stack pointer, points to the current stack
        SP : VRegister
     }
     with
        static member FontSprites =
            [|
                0xF0uy;0x90uy;0x90uy;0x90uy;0xF0uy; // 0
                0x70uy;0x20uy;0x20uy;0x60uy;0x20uy; // 1
                0xF0uy;0x80uy;0xF0uy;0x10uy;0xF0uy; // 2
                0xF0uy;0x10uy;0xF0uy;0x10uy;0xF0uy; // 3
                0x10uy;0x10uy;0xF0uy;0x90uy;0x90uy; // 4
                0xF0uy;0x10uy;0xF0uy;0x80uy;0xF0uy; // 5
                0xF0uy;0x90uy;0xF0uy;0x80uy;0xF0uy; // 6
                0x40uy;0x40uy;0x20uy;0x10uy;0xF0uy; // 7
                0xF0uy;0x90uy;0xF0uy;0x90uy;0xF0uy; // 8
                0xF0uy;0x10uy;0xF0uy;0x90uy;0xF0uy; // 9
                0x90uy;0x90uy;0xF0uy;0x90uy;0xF0uy; // A
                0xE0uy;0x90uy;0xE0uy;0x90uy;0xE0uy; // B
                0xF0uy;0x80uy;0x80uy;0x80uy;0xF0uy; // C
                0xE0uy;0x90uy;0x90uy;0x90uy;0xE0uy; // D
                0xF0uy;0x80uy;0xF0uy;0x80uy;0xF0uy; // E
                0x80uy;0x80uy;0xF0uy;0x80uy;0xF0uy; // F
            |]
        static member StackBase = 0xEA0us
        static member ProgramBase = 0x200us
        static member ByteToKey b =
            let key =
                match b with
                | 0x00uy -> Keys.NumPad0
                | 0x07uy -> Keys.NumPad1
                | 0x08uy -> Keys.NumPad2
                | 0x09uy -> Keys.NumPad3
                | 0x04uy -> Keys.NumPad4
                | 0x05uy -> Keys.NumPad5
                | 0x06uy -> Keys.NumPad6
                | 0x01uy -> Keys.NumPad7
                | 0x02uy -> Keys.NumPad8
                | 0x03uy -> Keys.NumPad9
                | 0x0Auy -> Keys.A
                | 0x0Buy -> Keys.B
                | 0x0Cuy -> Keys.C
                | 0x0Duy -> Keys.D
                | 0x0Euy -> Keys.E
                | 0x0Fuy -> Keys.F
            key
        static member Frequency = 1.00 / 120.00 //60hz, 60 cycles per second
        static member IsKeyPressed = Chip8.ByteToKey >> External.IsKeyPressed
        /// Creates a new model of Chip8
        static member Create() =
            let ram = Array.append Chip8.FontSprites (Array.create 4016 0uy)
            {
                Screen = BitArray(64*32)
                Ram    = ram;
                 V     = Array.create 16 0uy;
                 I     = 0x0us
                DT     = 0x0uy
                ST     = 0x0uy
                PC     = Chip8.ProgramBase
                SP     = 0x0uy
            }
        //copyBlit source sourceIndex target targetIndex count
        member this.LoadProgram (bytes:Byte[]) =
            { this with Ram = Array.copyBlit bytes 0 this.Ram (int Chip8.ProgramBase) bytes.Length }
        member this.ReadRam startPos count =
            //printfn "Reading from %d to %d" startPos (startPos+count-1)
            let read = Array.rev <| this.Ram.[startPos..startPos+count-1]
            //printfn "Read: %A" read
            read
        member this.WriteRam addr bytes =
            //printfn "Writing %A to %X" bytes addr
            Array.copyBlit ((*Array.rev*) bytes) 0 this.Ram addr bytes.Length

let printFirstScreen() =
    Console.Clear()
    Console.CursorVisible <- false
    for y in 0..31 do
        for x in 0..63 do
            printf " "
        printfn ""

let updateScreen (oldScreen : BitArray) (newScreen : BitArray) =
    let diff = newScreen.DiffArray oldScreen
    for y in 0..31 do
        for x in 0..63 do
            match diff.[y * 64 + x] with
            | Some true ->
                Console.SetCursorPosition(x, y)
                Console.Write("█")
            | Some false ->
                Console.SetCursorPosition(x, y)
                Console.Write(" ")
            | _ -> ()

let drawSprite (screen:BitArray) (spriteData:BitArray) xPos yPos =
    let newScreen = BitArray(screen)
    let height = Convert.ToUInt16(spriteData.Length / 8)
    let mutable collision = false
    for y in 0us.. height-1us do
        for x in 0us..7us do
            let newXPos =
                if xPos + x > 63us then (xPos + x) % 64us
                else xPos + x
            let newYPos =
                if yPos + y > 31us then (yPos + y) % 32us
                else yPos + y

            let arrayPos = int (newYPos * 64us + newXPos)
            let spritePos = int(y * 8us + x)

            if screen.[arrayPos] && spriteData.[spritePos] then
                collision <- true

            if screen.[arrayPos] = spriteData.[spritePos] then
                newScreen.Set(arrayPos, false)
            else
                newScreen.Set(arrayPos, true)
    (newScreen, collision)