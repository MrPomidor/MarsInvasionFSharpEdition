namespace MarsInvasion.Domain

module Domain =
    type Direction = 
        | North     (* Север *)
        | South     (* Юг *)
        | West      (* Восток *)
        | East      (* Запад *)

    type RoverCommand =
        | Move
        | TurnLeft
        | TurnRight

    type Point = { x:int; y:int }

    type Plateu private (LeftCorner: Point, RightCorner: Point) =
        private new(rightCorner) = Plateu({ x = 0; y = 0; }, rightCorner);
        
        member this.LeftCorner = LeftCorner;
        member this.RightCorner = RightCorner;

        static member createPlateu(rightCorner: Point): Option<Plateu> =
            let leftCorner = { x = 0; y = 0; }
            if (rightCorner.x <= leftCorner.x) then
                None
            elif (rightCorner.y <= leftCorner.y) then
                None
            else 
                let plateu = Plateu(leftCorner, rightCorner)
                Some plateu

    type SerialNumber = SerialNumber of int

    type Rover = { serialNumber: SerialNumber; point: Point; direction: Direction }

    type PointOutOfPlateuError = 
        | XAxis
        | YAxis

    type PointOccupiedError = 
        | OccupiedByRover of point:Point * roverSerialNumber:SerialNumber

    type RoverDeployError = 
        | PointOccupiedError of PointOccupiedError
        | PointOutOfPlateuError of PointOutOfPlateuError

    let handleMove point direction =
        match direction with
        | North ->
            { point with y = point.y + 1 }
        | South ->
            { point with y = point.y - 1 }
        | West ->
            { point with x = point.x - 1 }
        | East ->
            { point with x = point.x + 1 }

    let turnLeft direction =
        match direction with
        | North -> West
        | South -> East
        | West -> South
        | East -> North

    let turnRight direction =
        match direction with
        | North -> East
        | South -> West
        | West -> North
        | East -> West

    let handleCommand startingPoint startingDirection command =
        match command with
        | Move ->
            let newPoint = handleMove startingPoint startingDirection
            (newPoint, startingDirection)
        | TurnLeft ->
            let newDirection = turnLeft startingDirection
            (startingPoint, newDirection)
        | TurnRight ->
            let newDirection = turnRight startingDirection
            (startingPoint, newDirection)

    let isPointInPlateu (plateu:Plateu, point:Point) =
        if (point.x < plateu.LeftCorner.x || point.x > plateu.RightCorner.x) then
            Error PointOutOfPlateuError.XAxis
        elif (point.y < plateu.LeftCorner.y || point.y > plateu.RightCorner.y) then
            Error PointOutOfPlateuError.YAxis
        else
            Ok ()

    let isPointOccupied (rovers:list<Rover>, point:Point) =
        let pointMatch rover = rover.point = point
        match List.tryFind pointMatch rovers with
        | Some rover -> PointOccupiedError.OccupiedByRover (point, rover.serialNumber) |> Error
        | None -> Ok ()

    let validatePoint plateu rovers point : Result<unit, RoverDeployError> =
        match isPointInPlateu (plateu, point) with 
        | Error error -> RoverDeployError.PointOutOfPlateuError error |> Error
        | Ok _ ->
        
        match isPointOccupied (rovers, point) with
        | Error error -> RoverDeployError.PointOccupiedError error |> Error
        | Ok _ -> 
        
        Ok ()

    let handleRoverPath plateu rovers point direction commands =
        match validatePoint plateu rovers point with
        | Error error -> error |> Error
        | Ok _ -> 

        let commandExecutionSequence = seq {
            let mutable currentPoint = point
            let mutable currentDirection = direction

            for command in commands do
                let (newPoint, newDirection) = handleCommand currentPoint currentDirection command
                match validatePoint plateu rovers newPoint with
                | Error error -> yield Error error
                | Ok _ -> ()

                currentPoint <- newPoint
                currentDirection <- newDirection
                
            yield Ok (currentPoint, currentDirection)
        }

        let sequenceFirst = Seq.tryExactlyOne commandExecutionSequence

        match sequenceFirst with
        | Some sequenceItem -> sequenceItem
        | None -> Ok (point, direction)

        