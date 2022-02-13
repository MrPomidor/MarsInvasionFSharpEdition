module MarsInvasion.Domain.DomainTests

open NUnit.Framework
open Domain

[<Test>]
let handleRoverPath_ShouldPassTestScenario () =
    // arrange
    let plateuOption = Plateu.createPlateu({ x = 5; y = 5; })

    match plateuOption with
    | None -> Assert.Fail()
    | Some plateu -> 

    let initialPoint = { x=1; y=2 }
    let initialDirection = North
    let commands = List.toSeq [ TurnLeft; Move; TurnLeft; Move; TurnLeft; Move; TurnLeft; Move; Move; ] // LMLMLMLMM
    let expectedPosition = { x=1; y=3; }
    let expectedDirection = North

    // act
    let result = handleRoverPath plateu list.Empty initialPoint initialDirection commands

    // assert
    match result with 
    | Error error -> sprintf "Unexpected failed result: %A" error |> Assert.Fail
    | Ok (point, direction) ->

    match (point, direction) with
    | (point, direction) when (point = expectedPosition && direction = expectedDirection) -> Assert.Pass()
    | _ -> sprintf "Unexpected point (%A) and direction (%A) results" point direction |> Assert.Fail
