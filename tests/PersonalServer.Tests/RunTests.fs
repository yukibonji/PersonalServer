namespace PersonalServer.Tests

open Expecto

module Tests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args DomainTypes.testTag |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testNonEmptyString |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString2 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString3 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString4 |> ignore
//        Tests.runTestsWithArgs defaultConfig args DomainTypes.testNameOfPerson |> ignore

        0
