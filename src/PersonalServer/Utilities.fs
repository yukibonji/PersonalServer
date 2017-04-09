namespace Jackfoxy.PersonalServer

open System.Text.RegularExpressions

module internal Utilities =
        
    let (|Success|Failure|) = 
        function
        | Choice1Of2 a -> Success a
        | Choice2Of2 e -> Failure e

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x
               
    let combineNumber foo =
        foo
        |> List.concat
        |> String.concat ""

    let numbersFromString foo =
        Regex.Split(foo, @"\D+")
        |> String.concat ""

    let dashAndNumberOnly (foo : string) =
        let x = foo.Split '-'
        let regex = new Regex("^[0-9]+$")
        let ret =
            (true, x)
            ||> Array.fold (fun s t ->
                if regex.IsMatch t then s
                else false)
        ret, (x.Length - 1)
            



