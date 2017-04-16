namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open Expecto
open FsCheck

module DomainTypes =

    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with replay = Some <| (1919174669,296290663) }  //see Tips & Tricks for FsCheck
 
    Arb.register<DomainGenerators>() |> ignore

    [<Tests>]
    let testFullName =
        testList "DomainTypes.FullName" [

            testPropertyWithConfig config10k "test label" <|
                fun  () ->
                    Prop.forAll (DomainGenerators.FullName()) 
                        (fun (fullName : FullName) -> 
                                
                                let first = fullName.First |> Option.map (fun x -> x.Value)
                                let middle = fullName.Middle |> List.map (fun x -> x.Value)
                                let family = fullName.Family |> Option.map (fun x -> x.Value)
                                //note: custom label does not print on failure
                                1 = 2|@ sprintf "first: %A middle: %A family: %A"  fullName.First middle fullName.Family
                            )

            testPropertyWithConfig config10k "this will fail with config10k, then succeed with configReplay" <|
//            testPropertyWithConfig configReplay "this will fail with config10k, then succeed with configReplay" <|
                fun  () ->
                    Prop.forAll (DomainGenerators.FullName()) 
                        (fun (fullName : FullName) -> 
                                
                                let first = fullName.First |> Option.map (fun x -> x.Value)
                                let middle = fullName.Middle |> List.map (fun x -> x.Value)
                                let family = fullName.Family |> Option.map (fun x -> x.Value)

                                let t =  
                                    FullName.TryParse (first, middle, family, fullName.NameOrder, Set.empty<Tag>)
                                    
                                t.Value = fullName
                            )

            testPropertyWithConfig config10k "This should have registered FullName above" <|
                fun  (fullName : FullName) ->

                    let t = 
                        let first = fullName.First |> Option.map (fun x -> x.Value)
                        let middle = fullName.Middle |> List.map (fun x -> x.ToString())
                        let family = fullName.Family |> Option.map (fun x -> x.Value)
                        FullName.TryParse (first, middle, family,  NameOrder.Western, Set.empty<Tag>)
 
                    t.Value = fullName
            ]

    