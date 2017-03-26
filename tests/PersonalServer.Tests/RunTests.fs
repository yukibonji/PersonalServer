namespace PersonalServer.Tests

open Expecto

module Tests =

    [<EntryPoint>]
    let main _ =
      
      Tests.runTests defaultConfig DomainTypes.testTag |> ignore
      Tests.runTests defaultConfig DomainTypes.testNonEmptyString |> ignore
      Tests.runTests defaultConfig DomainTypes.testNonEmptyStringOption |> ignore
      Tests.runTests defaultConfig DomainTypes.testDigitString |> ignore
      Tests.runTests defaultConfig DomainTypes.testDigitString2 |> ignore
      Tests.runTests defaultConfig DomainTypes.testDigitString3 |> ignore
      Tests.runTests defaultConfig DomainTypes.testDigitString4 |> ignore

      0
