namespace PersonalServer.Tests

open Expecto

module Tests =

    [<EntryPoint>]
    let main args =

        Tests.runTestsWithArgs defaultConfig args DomainTypes.tag |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.trimNonEmptyString |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.digits |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.digits2 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.digits3 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.digits4 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.fullName |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.simpleName |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.nameAndAffixes |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.zipCode5 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.zipCode5Plus4 |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.nonUsPostalCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.zipCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.postalCode |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.physicalAddress |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.emailAddress |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.usPhone |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.otherPhone |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.phone |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.phoneNumber |> ignore
        Tests.runTestsWithArgs defaultConfig args DomainTypes.uriTagged |> ignore

        Tests.runTestsWithArgs defaultConfig args ContactImport.simpleEntityBuilder |> ignore
        Tests.runTestsWithArgs defaultConfig args ContactImport.addressElimination |> ignore
        Tests.runTestsWithArgs defaultConfig args ContactImport.addressEliminationTagMerge |> ignore
        Tests.runTestsWithArgs defaultConfig args ContactImport.contactElimination |> ignore
        Tests.runTestsWithArgs defaultConfig args ContactImport.addressEliminationTagMerge |> ignore

        0
