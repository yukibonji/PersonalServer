namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open System 

module Sources =
    let primarySource = (TrimNonEmptyString.TryParse "source").Value
    let secondarySource = (TrimNonEmptyString.TryParse "second").Value
    let utcDateTime = UtcDateTime DateTime.UtcNow
    let sourceSet = Source(primarySource, None, utcDateTime, utcDateTime) |> NonEmptySet.Singleton

    let time1 =  UtcDateTime <| DateTime(2017, 2, 1)
    let time2 =  UtcDateTime <| DateTime(2017, 3, 2)
    let time3 =  UtcDateTime <| DateTime(2017, 4, 3)

    let source1 = NonEmptySet.Singleton <| Source(primarySource, None, time1, time1)
    let source2 = NonEmptySet.Singleton <| Source(primarySource, None, time2, time2)
    let source3 = NonEmptySet.Singleton <| Source(primarySource, None, time3, time3)

    let source1b = NonEmptySet.Singleton <| Source(primarySource, Some secondarySource, time1, time1)
    let source2b = NonEmptySet.Singleton <| Source(primarySource, Some secondarySource, time2, time2)
    let source3b = NonEmptySet.Singleton <| Source(primarySource, Some secondarySource, time3, time3)

module Tags =
    let tagSet1 = [Tag.TryParse ("foo1", Sources.sourceSet);Tag.TryParse ("foo2", Sources.sourceSet);Tag.TryParse ("foo3", Sources.sourceSet);Tag.TryParse ("foo4", Sources.sourceSet)] |> List.choose id |> Set.ofList
    let tagSet1a = [Tag.TryParse ("foo1", Sources.sourceSet);Tag.TryParse ("foo3", Sources.sourceSet);] |> List.choose id |> Set.ofList
    let tagSet1b = [Tag.TryParse ("foo1", Sources.sourceSet);Tag.TryParse ("foo2", Sources.sourceSet);Tag.TryParse ("foo4", Sources.sourceSet);] |> List.choose id |> Set.ofList

    let tagSet2 = [Tag.TryParse ("bar1", Sources.sourceSet);Tag.TryParse ("bar2", Sources.sourceSet);Tag.TryParse ("bar3", Sources.sourceSet);Tag.TryParse ("bar4", Sources.sourceSet)] |> List.choose id |> Set.ofList
    let tagSet2a = [Tag.TryParse ("bar1", Sources.sourceSet);Tag.TryParse ("bar3", Sources.sourceSet);] |> List.choose id |> Set.ofList
    let tagSet2b = [Tag.TryParse ("bar2", Sources.sourceSet);Tag.TryParse ("bar4", Sources.sourceSet);] |> List.choose id |> Set.ofList

    let tagSet3 = [Tag.TryParse ("zip1", Sources.sourceSet);Tag.TryParse ("zip2", Sources.sourceSet);Tag.TryParse ("zip3", Sources.sourceSet);Tag.TryParse ("zip4", Sources.sourceSet)] |> List.choose id |> Set.ofList
    let tagSet3a = [Tag.TryParse ("zip1", Sources.sourceSet);Tag.TryParse ("zip3", Sources.sourceSet);] |> List.choose id |> Set.ofList
    let tagSet3b = [Tag.TryParse ("zip2", Sources.sourceSet);Tag.TryParse ("zip4", Sources.sourceSet);] |> List.choose id |> Set.ofList

module Contacts =
    let nameAndAffixes1 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Herr";"Dr"; "Dr"], "Anthony Foo", ["esq"; "IV"], Tags.tagSet3a, Sources.sourceSet)).Value
    let nameAndAffixes2 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Dr"; "Dr"], "Anthony Foo", [],                       Set.empty, Sources.sourceSet)).Value
    let nameAndAffixes3 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Dr"], "Anthony Foo", ["IV"],                         Set.empty, Sources.sourceSet)).Value

    let nameAndAffixesFoo1 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Foo"], "Foo1 Foo", ["Foo"],                          Set.empty, Sources.sourceSet)).Value
    let nameAndAffixesFoo2 =
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Foo"], "Foo2 Foo", ["Foo"],                          Set.empty, Sources.sourceSet)).Value 

    let simpleName1 =
        ContactName.SimpleName     <| (SimpleName.TryParse ("Anthony Foo",                                         Tags.tagSet1a, Sources.sourceSet)).Value
    let simpleName2 =
        ContactName.SimpleName     <| (SimpleName.TryParse ("Bar Bar",                                                 Set.empty, Sources.sourceSet)).Value

    let simpleNameFoo1 =
        ContactName.SimpleName     <| (SimpleName.TryParse ("Foo1 Foo",                                                Set.empty, Sources.sourceSet)).Value
    let simpleNameFoo3 =
        ContactName.SimpleName     <| (SimpleName.TryParse ("Foo3 Foo",                                                Set.empty, Sources.sourceSet)).Value


    let fullName1 =
        ContactName.FullName       <| (FullName.TryParse (Some "Anthony", [], Some "Foo", NameOrder.Western,       Tags.tagSet2a, Sources.sourceSet)).Value

    let fullNameFoo1 =
        ContactName.FullName       <| (FullName.TryParse (Some "Foo", [], Some "Foo1", NameOrder.Western,              Set.empty, Sources.sourceSet)).Value

    let fullNameFoo2 =
        ContactName.FullName       <| (FullName.TryParse (Some "Foo", [], Some "Foo2", NameOrder.Western,              Set.empty, Sources.sourceSet)).Value

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
        ContactName.SimpleName <| (SimpleName.TryParse ("Anthony Foo", Set.empty, Sources.sourceSet)).Value //eliminated by simpleName1
        ]

    let simpleNameElim6 =
        [
        nameAndAffixesFoo2
        fullNameFoo2
        simpleName2 
        simpleName1  
        ContactName.SimpleName <| (SimpleName.TryParse ("Anthony Foo", Set.empty, Sources.sourceSet)).Value //eliminated by simpleName1
        simpleNameFoo1
        ContactName.SimpleName <| (SimpleName.TryParse ("Foo1 Foo",    Set.empty, Sources.sourceSet)).Value //eliminated by simpleNameFoo1
        simpleNameFoo3
        ]

    let simpleNameTagMerge =
        [
        simpleName1  
        ContactName.SimpleName <| (SimpleName.TryParse ("Anthony Foo", Tags.tagSet1b, Sources.sourceSet)).Value //eliminated by simpleName1
        ]

    let fullNameElim1 =
        [
        nameAndAffixes1
        nameAndAffixes2
        nameAndAffixes3 
        simpleName2  
        fullName1
        ContactName.FullName <| (FullName.TryParse (Some "Anthony", [], Some "Foo", NameOrder.Western, Set.empty, Sources.sourceSet)).Value
        ]

    let fullNameTagMerge =
        [ 
        fullName1
        ContactName.FullName <| (FullName.TryParse (Some "Anthony", [], Some "Foo", NameOrder.Western, Tags.tagSet2b, Sources.sourceSet)).Value
        ]

    let nameAndAffixesElim1 =
        [
        nameAndAffixes1
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Herr";"Dr"; "Dr"], "Anthony Foo", ["esq"; "IV"], Set.empty, Sources.sourceSet)).Value
        nameAndAffixes2
        nameAndAffixes3 
        simpleName2  
        fullName1
        ]

    let nameAndAffixesTagMerge =
        [
        nameAndAffixes1
        ContactName.NameAndAffixes <| (NameAndAffixes.TryParse (["Herr";"Dr"; "Dr"], "Anthony Foo", ["esq"; "IV"], Tags.tagSet3b, Sources.sourceSet)).Value
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