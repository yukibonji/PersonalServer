namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
//open FsCheck
//open System

module Contacts =
    let tagSet1 = [Tag.TryParse "foo1";Tag.TryParse "foo2";Tag.TryParse "foo3";Tag.TryParse "foo4"] |> Set.ofList
    let tagSet1a = [Tag.TryParse "foo1";Tag.TryParse "foo3";] |> Set.ofList
    let tagSet1b = [Tag.TryParse "foo2";Tag.TryParse "foo4";] |> Set.ofList

    let tagSet2 = [Tag.TryParse "bar1";Tag.TryParse "bar2";Tag.TryParse "bar3";Tag.TryParse "bar4"] |> Set.ofList
    let tagSet2a = [Tag.TryParse "bar1";Tag.TryParse "bar3";] |> Set.ofList
    let tagSet2b = [Tag.TryParse "bar2";Tag.TryParse "bar4";] |> Set.ofList

    let tagSet3 = [Tag.TryParse "zip1";Tag.TryParse "zip2";Tag.TryParse "zip3";Tag.TryParse "zip4"] |> Set.ofList
    let tagSet3a = [Tag.TryParse "zip1";Tag.TryParse "zip3";] |> Set.ofList
    let tagSet3b = [Tag.TryParse "zip2";Tag.TryParse "zip4";] |> Set.ofList


    //|| (fullName.First.IsSome && fullName.First.Value = simpleName.Value)
    //|| (fullName.Family.IsSome && fullName.First.Value = simpleName.Value) 
    //|| (fullName.First.IsSome 
    //    && fullName.Family.IsSome 
    //    && sprintf "%s %s " fullName.First.Value.Value fullName.Family.Value.Value |> TrimNonEmptyString = simpleName.Value) 

    let contactNames =
        [
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Herr";"Dr"; "Dr"], "Anthony Foo", ["esq"; "IV"], Set.empty)).Value
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Dr"; "Dr"], "Anthony Foo", [],                   Set.empty)).Value
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Dr"], "Anthony Foo", ["IV"],                     Set.empty)).Value 
        ContactName.SimpleName     <| (SimpleName.TryParse ("Anthony Foo",                                         Set.empty)).Value  //will non deterministically eliminat into one of the others
        ContactName.FullName       <| (FullName.TryParse (Some "Anthony", [], Some "Foo", NameOrder.Western,       Set.empty)).Value
        ]

module List = 
    // http://fssnip.net/4u/title/Very-Fast-Permutations
    // From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    // Much faster than anything else I've tested

    let rec private insertions x = function
    //let rec private insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))