namespace PersonalServer.Tests

//open PersonalServer
open Expecto

module Tests =
    let tests =
      test "A simple test" {
        let subject = "Hello world"
        Expect.equal subject "Hello World" "The strings should equal"
      }

    [<EntryPoint>]
    let main args =
      runTestsWithArgs defaultConfig args tests |> ignore

      Tests.runTests defaultConfig DomainTypes.testTag
