namespace MarsInvasion.Domain

module Parsers =
    open Domain
    open System.Text.RegularExpressions


    (* TODO probably reorganize*)

    let plateuInvalidFormatMessage = "Invalid format ! Please, input graph upper right coordinates in expected format: ONE integer, SPACE, ONE integer"
    let plateuInputRegex = new Regex(@"^(?<x>\d+) (?<y>\d+)$", RegexOptions.Compiled)

    let parsePlateu input =
        match plateuInputRegex.Match input with
        | m when not m.Success -> Error plateuInvalidFormatMessage
        | m ->

        let xCoordinates = System.Int32.Parse m.Groups["x"].Value;
        let yCoordinates = System.Int32.Parse m.Groups["y"].Value;

        match Plateu.createPlateu({ x = xCoordinates; y = yCoordinates }) with
        | None -> Error plateuInvalidFormatMessage
        | Some plateu -> Ok plateu
    
    let roverInvalidFormatMessage = "Invalid format ! Please, input initial coordinates in expected format: ONE integer, SPACE, ONE integer, SPACE, ONE letter among N S E W"
    let roverPositionInputRegex = new Regex(@"^(?<x>\d+) (?<y>\d+) (?<direction>[NSEW])$", RegexOptions.Compiled)

    let parseRoverDirection input =
        match input with
        | "N" -> Ok North
        | "S" -> Ok South
        | "E" -> Ok East
        | "W" -> Ok West
        | _ -> Error roverInvalidFormatMessage 

    let parseRoverPosition input =
        match roverPositionInputRegex.Match input with
        | m when not m.Success -> Error roverInvalidFormatMessage
        | m ->

        let xCoordinates = System.Int32.Parse m.Groups["x"].Value;
        let yCoordinates = System.Int32.Parse m.Groups["y"].Value;
        let directionStr = m.Groups["direction"].Value;

        match parseRoverDirection directionStr with
        | Error error -> Error error
        | Ok direction ->

        Ok ({ x = xCoordinates; y = yCoordinates; }, direction)

    let invalidMovementPlanMessage = "Invalid format ! Please, input movement plan in expected format: ONE OR MORE letters among M L R"
    let roverMovementPlanInputRegex = new Regex(@"^[MLR]+$", RegexOptions.Compiled)

    let parseRoverMovement input =
        match input with
        | 'M' -> Ok Move
        | 'L' -> Ok TurnLeft
        | 'R' -> Ok TurnRight
        | _ -> Error invalidMovementPlanMessage

    let parseRoverMovementPlan input =
        match roverMovementPlanInputRegex.Match input with
        | m when not m.Success -> Error invalidMovementPlanMessage
        | m ->

        let movementPlanParsed = input.ToCharArray() |> Seq.ofArray |> Seq.map parseRoverMovement

        let planItemInvalid item = 
            match item with
            | Ok _ -> false
            | _ -> true

        match Seq.exists planItemInvalid movementPlanParsed with
        | true -> Error invalidMovementPlanMessage
        | false -> 
            let resultSequence = seq{
                for movementPlanItem in movementPlanParsed do
                    match movementPlanItem with
                    | Ok command -> yield command
                    | _ -> ()
            }
            Ok resultSequence
