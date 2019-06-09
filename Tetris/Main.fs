open System

[<EntryPoint>]
let main argv =
    Console.Clear()
    Console.CursorVisible <- false;
    EngineFunctions.run StateFunctions.createState
    Console.CursorVisible <- true;
    Console.Clear()
    0