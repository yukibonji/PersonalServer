namespace PersonalServer.Tests

open Expecto

module Tests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args DomainTypes.testTag |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testTrimNonEmptyString |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString2 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString3 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testDigitString4 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testFullName |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testPersonName |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testNameAndAffixes |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testZipCode5 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.ZipCode5Plus4 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testNonUsPostalCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.ZipCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.PostalCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.PhysicalAddress |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testEmailAddress |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.testUsPhone |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.tesuOtherPhone |> ignore

        0
