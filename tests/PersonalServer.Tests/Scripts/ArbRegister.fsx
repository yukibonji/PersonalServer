#load "load-project-debug.fsx"

open Jackfoxy.PersonalServer
open PersonalServer.Tests
open FsCheck

Arb.register<DomainGenerators>() |> ignore

let x = 
    Prop.forAll (DomainGenerators.FullName()) 
        (fun (fullName : FullName)  -> 
                let first = fullName.First |> Option.map (fun x -> x.Value)
                let middle = fullName.Middle |> List.map (fun x -> x.Value)
                let family = fullName.Family |> Option.map (fun x -> x.Value)

                let t = 
                    FullName.TryParse (first, middle, family, fullName.NameOrder, Set.empty<Tag>)

                t.Value = fullName |@ sprintf "first: %A middle: %A family: %A"  fullName.First middle fullName.Family
            )

let testLabel =
    Prop.forAll (DomainGenerators.FullName()) 
        (fun (fullName : FullName) -> 
                                
                let first = fullName.First |> Option.map (fun x -> x.Value)
                let middle = fullName.Middle |> List.map (fun x -> x.Value)
                let family = fullName.Family |> Option.map (fun x -> x.Value)

                1 = 2|@ sprintf "first: %A middle: %A family: %A"  fullName.First middle fullName.Family
            )

let fullNameTest = 
   fun (fullName : FullName)  -> 
            let first = fullName.First |> Option.map (fun x -> x.Value)
            let middle = fullName.Middle |> List.map (fun x -> x.Value)
            let family = fullName.Family |> Option.map (fun x -> x.Value)

            let t = 
                FullName.TryParse (first, middle, family, fullName.NameOrder, Set.empty<Tag>)

            t.Value = fullName |@ sprintf "first: %A middle: %A family: %A"  fullName.First middle fullName.Family

Check.Quick fullNameTest

Check.Quick x

Check.Quick testLabel

Check.One({ Config.Quick with Replay = Some <| Random.StdGen (420278051,296290671) }, x)


Check.One({ Config.Quick with Replay = Some <| Random.StdGen (310936656,296287911) }, fullNameTest)  //works, but only after registering!
