module PersonalServer.Tests

open PersonalServer
open NUnit.Framework
open FsUnit

[<Test>]
let ``hello returns 42`` () =
  let result = 42
  printfn "%i" result
  Assert.AreEqual(42,result)

  result |> should equal 42
