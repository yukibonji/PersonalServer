namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open Expecto
open FsCheck
open System

module DomainTypes =

    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { Config.Quick with Replay = Some <| Random.StdGen(123, 456) }  //see Tips & Tricks for FsCheck

    let (.=.) left right = left = right |@ sprintf "%A = %A" left right

    [<Tests>]
    let testTag =
        testList "FsCheck samples" [

            testCase "tag throws on empty string" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  Tag System.String.Empty |> ignore)
                                            "Expected argument exception.")

            testCase "tag throws on null" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  Tag null |> ignore)
                                            "Expected argument exception.")

            testPropertyWithConfig config10k "tag creation" <|
                fun  (tag : NonEmptyString) ->
                    let t = Tag (tag.ToString())
                    let t' = t.ToString()
                    (tag.ToString()) = t'

            testPropertyWithConfig config10k "tag set creation" <|
                fun  (tagList : list<NonEmptyString>) ->
                    let distinctList = 
                        tagList
                        |> List.map (fun x -> x.ToString())
                        |> List.distinct
                        |> List.sort

                    let tagSet =
                        distinctList
                        |> List.map (fun x -> Tag <| x.ToString())
                        |> Set.ofList

                    let returnedList = 
                        tagSet
                        |> Set.toList
                        |> List.map (fun x -> x.ToString())
                        |> List.sort
                   
                    distinctList = returnedList

            testPropertyWithConfig config10k "tag set is unique" <|
                fun  (tagList : list<NonEmptyString>) ->
                    let distinctList = 
                        tagList
                        |> List.map (fun x -> x.ToString())
                        |> List.distinct
                        |> List.sort

                    let tagSet =
                        distinctList
                        |> List.map (fun x -> Tag <| x.ToString())
                        |> Set.ofList

                    let tagSetAltered =
                        (tagSet, distinctList)
                        ||> List.fold (fun s t -> s.Add <| Tag t) 

                    let returnedList = 
                        tagSetAltered
                        |> Set.toList
                        |> List.map (fun x -> x.ToString())
                        |> List.sort
                   
                    distinctList = returnedList
            ]

