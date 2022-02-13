module MarsInvasion.Domain.ParsersTests

open NUnit.Framework
open Domain
open Parsers
open System.Linq

[<Test>]
let parsePlateu_correctInput_shouldPass () =
    let input = "1 1"
    let expectedX = 1
    let expectedY = 1

    let result = parsePlateu input

    match result with
    | Error errorStr -> sprintf "Unexpected fail with %A" errorStr |> Assert.Fail
    | Ok plateu ->
        match plateu with
        | plateu when plateu.RightCorner.x = expectedX && plateu.RightCorner.y = expectedY -> Assert.Pass()
        | _ -> sprintf "Unexpected plateu with x:%A and y:%A" plateu.RightCorner.x plateu.RightCorner.y |> Assert.Fail

[<Test>]
let parsePlateu_incorrectAlphanumericInput_shouldFail () =
    let input = "A 1"

    let result = parsePlateu input

    match result with 
    | Error errorStr when errorStr = "Invalid format ! Please, input graph upper right coordinates in expected format: ONE integer, SPACE, ONE integer" -> Assert.Pass()
    | Error errorStr -> sprintf "Unexpected error message: %A" errorStr |> Assert.Fail
    | Ok _ -> Assert.Fail "Expected not to create plateu, but was created"

[<Test>]
let parsePlateu_incorrectNumericInput_shouldFail () =
    let input = "-1 -1"

    let result = parsePlateu input

    match result with 
    | Error errorStr when errorStr = "Invalid format ! Please, input graph upper right coordinates in expected format: ONE integer, SPACE, ONE integer" -> Assert.Pass()
    | Error errorStr -> sprintf "Unexpected error message: %A" errorStr |> Assert.Fail
    | Ok _ -> Assert.Fail "Expected not to create plateu, but was created"

[<Test>]
let parseRoverMovementPlan_correctInput_shouldPass () =
    let input = "LMLMLMLMM"
    let expectedMovementPlan = List.toSeq [ TurnLeft; Move; TurnLeft; Move; TurnLeft; Move; TurnLeft; Move; Move; ]

    let result = parseRoverMovementPlan input

    match result with 
    | Error errorStr -> sprintf "Expected to pass but failed with message: %A" errorStr |> Assert.Fail
    | Ok movementPlan when Enumerable.SequenceEqual (movementPlan, expectedMovementPlan) -> Assert.Pass()
    | Ok movementPlan -> sprintf "Unexpected movement plan: %A" movementPlan |> Assert.Fail 
