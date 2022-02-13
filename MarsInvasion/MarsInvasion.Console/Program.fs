open System
open MarsInvasion.Domain.Domain
open MarsInvasion.Domain.Parsers

[<EntryPoint>]
let main args =
    
    Console.Write "Enter Graph Upper Right Coordinate: "
    let input = Console.ReadLine()

    match parsePlateu input with 
    | Error errorStr -> Console.WriteLine errorStr; -1
    | Ok plateu ->

    let mutable rovers = List<Rover>.Empty
    let roverNumber = SerialNumber 1

    sprintf "Rover %A Starting Position: " roverNumber |> Console.Write 
    let input = Console.ReadLine()

    match parseRoverPosition input with 
    | Error errorStr -> Console.WriteLine errorStr; -1
    | Ok (startingPoint, direction) ->

    sprintf "Rover %A Movement Plan: " roverNumber |> Console.Write
    let input = Console.ReadLine()

    match parseRoverMovementPlan input with
    | Error errorStr -> Console.WriteLine errorStr; -1
    | Ok commands ->

    match handleRoverPath plateu rovers startingPoint direction commands with
    | Error roverDelpoyError ->
        match roverDelpoyError with
        | PointOutOfPlateuError pointOutOfPlateu -> Console.Write "Warning ! Coordinates out of available bounds."; -1
        | PointOccupiedError pointOccupied -> 
            match pointOccupied with
            | OccupiedByRover (occupedPoint, poverSerialNumber) -> sprintf "Warning ! Coordinates in use by rover %A" poverSerialNumber |> Console.WriteLine; -1
    | Ok (point, direction) ->

    let rover = { serialNumber = roverNumber; point = point; direction = direction; }
    sprintf "Rover %A Output: %A %A %A" rover.serialNumber rover.point.x rover.point.y rover.direction |> Console.WriteLine

    rovers <- rover::rovers  

    0