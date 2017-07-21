namespace PersonalServer.Tests

open Jackfoxy.PersonalServer

module Tags =
    let tagSet1 = [Tag.TryParse "foo1";Tag.TryParse "foo2";Tag.TryParse "foo3";Tag.TryParse "foo4"] |> List.choose id |> Set.ofList
    let tagSet1a = [Tag.TryParse "foo1";Tag.TryParse "foo3";] |> List.choose id |> Set.ofList
    let tagSet1b = [Tag.TryParse "foo1";Tag.TryParse "foo2";Tag.TryParse "foo4";] |> List.choose id |> Set.ofList

    let tagSet2 = [Tag.TryParse "bar1";Tag.TryParse "bar2";Tag.TryParse "bar3";Tag.TryParse "bar4"] |> List.choose id |> Set.ofList
    let tagSet2a = [Tag.TryParse "bar1";Tag.TryParse "bar3";] |> List.choose id |> Set.ofList
    let tagSet2b = [Tag.TryParse "bar2";Tag.TryParse "bar4";] |> List.choose id |> Set.ofList

    let tagSet3 = [Tag.TryParse "zip1";Tag.TryParse "zip2";Tag.TryParse "zip3";Tag.TryParse "zip4"] |> List.choose id |> Set.ofList
    let tagSet3a = [Tag.TryParse "zip1";Tag.TryParse "zip3";] |> List.choose id |> Set.ofList
    let tagSet3b = [Tag.TryParse "zip2";Tag.TryParse "zip4";] |> List.choose id |> Set.ofList

module Contacts =
    let nameAndAffixes1 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Herr";"Dr"; "Dr"], "Anthony Foo", ["esq"; "IV"], Tags.tagSet3a, Set.empty)).Value
    let nameAndAffixes2 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Dr"; "Dr"], "Anthony Foo", [],                       Set.empty, Set.empty)).Value
    let nameAndAffixes3 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Dr"], "Anthony Foo", ["IV"],                         Set.empty, Set.empty)).Value

    let nameAndAffixesFoo1 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Foo"], "Foo1 Foo", ["Foo"],                          Set.empty, Set.empty)).Value
    let nameAndAffixesFoo2 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Foo"], "Foo2 Foo", ["Foo"],                          Set.empty, Set.empty)).Value 

    let simpleName1 =
        ContactName.SimpleName     <| (SimpleName.TryParse ("Anthony Foo",                                         Tags.tagSet1a, Set.empty)).Value
    let simpleName2 =
        ContactName.SimpleName     <| (SimpleName.TryParse ("Bar Bar",                                                 Set.empty, Set.empty)).Value

    let simpleNameFoo1 =
        ContactName.SimpleName     <| (SimpleName.TryParse ("Foo1 Foo",                                                Set.empty, Set.empty)).Value
    let simpleNameFoo3 =
        ContactName.SimpleName     <| (SimpleName.TryParse ("Foo3 Foo",                                                Set.empty, Set.empty)).Value


    let fullName1 =
        ContactName.FullName       <| (FullName.TryParse (Some "Anthony", [], Some "Foo", NameOrder.Western,       Tags.tagSet2a, Set.empty)).Value

    let fullNameFoo1 =
        ContactName.FullName       <| (FullName.TryParse (Some "Foo", [], Some "Foo1", NameOrder.Western,              Set.empty, Set.empty)).Value

    let fullNameFoo2 =
        ContactName.FullName       <| (FullName.TryParse (Some "Foo", [], Some "Foo2", NameOrder.Western,              Set.empty, Set.empty)).Value

    let simpleNameElim1 =
        [
        nameAndAffixes1
        nameAndAffixes2
        nameAndAffixes3 
        simpleName1  //non-deterministically eliminates into one of the others
        fullName1
        ]

    let simpleNameElim2 =
        [
        nameAndAffixes1
        nameAndAffixes2
        nameAndAffixes3
        nameAndAffixesFoo1
        simpleName1  //non-deterministically eliminates into one of the others
        simpleName2
        fullNameFoo2
        ]

    let simpleNameElim3 =
        [
        nameAndAffixesFoo1
        fullNameFoo2
        simpleName2 
        simpleName1  //deterministically eliminated by fullName1
        fullName1
        ]

    let simpleNameElim4 =
        [
        nameAndAffixesFoo1
        fullNameFoo2
        simpleName2 
        simpleName1  //deterministically eliminated by nameAndAffixes1
        nameAndAffixes1
        ]

    let simpleNameElim5 =
        [
        nameAndAffixesFoo1
        fullNameFoo2
        simpleName2 
        simpleName1  
        ContactName.SimpleName <| (SimpleName.TryParse ("Anthony Foo", Set.empty, Set.empty)).Value //eliminated by simpleName1
        ]

    let simpleNameElim6 =
        [
        nameAndAffixesFoo2
        fullNameFoo2
        simpleName2 
        simpleName1  
        ContactName.SimpleName <| (SimpleName.TryParse ("Anthony Foo", Set.empty, Set.empty)).Value //eliminated by simpleName1
        simpleNameFoo1
        ContactName.SimpleName <| (SimpleName.TryParse ("Foo1 Foo",    Set.empty, Set.empty)).Value //eliminated by simpleNameFoo1
        simpleNameFoo3
        ]

    let simpleNameTagMerge =
        [
        simpleName1  
        ContactName.SimpleName <| (SimpleName.TryParse ("Anthony Foo", Tags.tagSet1b, Set.empty)).Value //eliminated by simpleName1
        ]

    let fullNameElim1 =
        [
        nameAndAffixes1
        nameAndAffixes2
        nameAndAffixes3 
        simpleName2  
        fullName1
        ContactName.FullName <| (FullName.TryParse (Some "Anthony", [], Some "Foo", NameOrder.Western, Set.empty, Set.empty)).Value
        ]

    let fullNameTagMerge =
        [ 
        fullName1
        ContactName.FullName <| (FullName.TryParse (Some "Anthony", [], Some "Foo", NameOrder.Western, Tags.tagSet2b, Set.empty)).Value
        ]

    let nameAndAffixesElim1 =
        [
        nameAndAffixes1
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Herr";"Dr"; "Dr"], "Anthony Foo", ["esq"; "IV"], Set.empty, Set.empty)).Value
        nameAndAffixes2
        nameAndAffixes3 
        simpleName2  
        fullName1
        ]

    let nameAndAffixesTagMerge =
        [
        nameAndAffixes1
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Herr";"Dr"; "Dr"], "Anthony Foo", ["esq"; "IV"], Tags.tagSet3b, Set.empty)).Value
        ]

module List = 
    // http://fssnip.net/4u/title/Very-Fast-Permutations
    // From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
    // Much faster than anything else I've tested

    let rec private insertions x = function
        | []             -> [[x]]
        | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))