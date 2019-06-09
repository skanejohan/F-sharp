module EngineFunctions

    open System
    open DomainTypes
    open StateFunctions
    open DrawFunctions

    let private msBetweenDrops (State (_, _, _, _, score)) = 50 * (11 - getLevel score)
        
    let rec private step pieceTransformer state n : unit = 
        let (changed, _, newstate) = pieceTransformer state
        if (changed) then
            drawState newstate
            step id newstate n
        else
            let key = if Console.KeyAvailable then Some (Console.ReadKey(true).Key) else None
            if key = Some ConsoleKey.Escape then
                ()
            elif key = Some ConsoleKey.UpArrow then
                step rotate state n 
            elif key = Some ConsoleKey.RightArrow then
                step moveRight state n 
            elif key = Some ConsoleKey.DownArrow then
                step drop state n 
            elif key = Some ConsoleKey.LeftArrow then
                step moveLeft state n 
            elif key = Some ConsoleKey.R then
                step initial createState 0 
            elif (n > msBetweenDrops state) then
                step moveDown state 0 
            else
                System.Threading.Thread.Sleep(10)
                step id state (n + 10) 

    let run state = step initial state 0
