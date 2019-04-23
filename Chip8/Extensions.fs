module Extensions
open System
open System.Collections
open System.Runtime.InteropServices
open System.Windows.Forms

type System.IO.BinaryReader with
    member this.ReadBytesBigEndian bytes =
        Array.rev <| this.ReadBytes(bytes)

    member this.ReadUInt16BigEndian() =
        BitConverter.ToUInt16(this.ReadBytes(sizeof<uint16>) |> Array.rev, 0)

    member this.ReadUInt16BigEndianAsBytes() =
        Array.rev <| this.ReadBytes(sizeof<uint16>)

type System.Collections.BitArray with
    member this.ToBigEndian()  =
        if BitConverter.IsLittleEndian then
            let bitArrayArray = Array.create this.Length false
            this.CopyTo(bitArrayArray, 0)
            BitArray(Array.rev bitArrayArray)
        else
            this

    member this.DiffArray (oldArr : BitArray) =
        let newArr' = Array.create this.Length false
        let oldArr' = Array.create oldArr.Length false
        this.CopyTo(newArr', 0)
        oldArr.CopyTo(oldArr', 0)
        let diffed =
            Array.map2 (fun n o ->
                if n && not o then
                    Some true
                else if not n && o then
                    Some false
                else
                    None) newArr' oldArr'
        diffed

[<RequireQualifiedAccess>]
module External =
    [<DllImport("user32.dll")>]
    extern int16 GetAsyncKeyState(Keys vKey)
    let IsKeyPressed (vKey:Keys) =
         0s <> (GetAsyncKeyState(vKey) &&& 0x8000s)

    module Time =
        [<DllImport("kernel32.dll", SetLastError=true)>]
        extern bool QueryPerformanceCounter(int64& lpPerformanceCount);

        [<DllImport("kernel32.dll", SetLastError=true)>]
        extern bool QueryPerformanceFrequency(int64& lpPerformanceCount);

        type HighResTimer() =
            let mutable secondsPerTick = 0.0
            let mutable timeCount = 0L
            let mutable timePassed = 0.0
            let mutable priorTime = 0.0
            do
                let mutable frequency = 0L
                if not <| QueryPerformanceFrequency(&frequency) then
                    failwith "High res timers not supported?"
                secondsPerTick <- 1.0 / Convert.ToDouble(frequency)

                if not <| QueryPerformanceCounter(&timeCount) then
                    failwith "High res timers not supported?"

            member __.SecondsPerTick with get() = secondsPerTick
            member __.TimeCount with get() = timeCount
            member this.TimePassed
                with get() =
                    let mutable counter = 0L
                    QueryPerformanceCounter(&counter) |> ignore
                    let interval = counter - timeCount
                    timeCount <- counter

                    let secondsPassed = Convert.ToDouble(interval) * this.SecondsPerTick
                    timePassed <- timePassed + secondsPassed
                    timePassed
            member this.DeltaTime
                with get () =
                    let curTime = this.TimePassed
                    let accumulated = curTime - priorTime
                    priorTime <- curTime
                    accumulated

[<RequireQualifiedAccess>]
module Array =
    let copySet array index element =
        let newArr = Array.copy array
        newArr.[index] <- element
        newArr

    let copyBlit source sourceIndex target targetIndex count =
        let newArr = Array.copy target
        Array.blit source sourceIndex newArr targetIndex count
        newArr