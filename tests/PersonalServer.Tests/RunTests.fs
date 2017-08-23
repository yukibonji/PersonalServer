namespace PersonalServer.Tests

open Expecto

module Tests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString4 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testFullName |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testPersonName |> ignore

        0
