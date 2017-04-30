namespace PersonalServer.Tests

open Expecto

module Tests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args DomainTypes.testTag |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testTrimNonEmptyString |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigits |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigits2 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigits3 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigits4 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testFullName |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testPersonName |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testNameAndAffixes |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testZipCode5 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testZipCode5Plus4 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testNonUsPostalCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testZipCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testPostalCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testPhysicalAddress |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testEmailAddress |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testUsPhone |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.tesuOtherPhone |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testPhone |> ignore

        0
