namespace PersonalServer.Tests

open DomainGeneratorsCode
open Jackfoxy.PersonalServer
open Expecto
open FsCheck

module DomainTypes =

    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000; arbitrary = [typeof<DomainGenerators>] }
//    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with maxTest = 10000 ; replay = Some <| (1940624926, 296296394) } // ; arbitrary = [typeof<DomainGenerators>] }  //see Tips & Tricks for FsCheck

    [<Tests>]
    let testTag =

        let distinctList nonEmptyStrings = 
            nonEmptyStrings
            |> List.map (fun x -> x.ToString().Trim())
            |> List.distinct
            |> List.sort

        let tagSet distinctList =
            distinctList
            |> List.choose (fun x -> Tag.TryParse <| x.ToString())
            |> Set.ofList

        let listFromSetOftags setOfTags = 
            setOfTags
            |> Set.toList
            |> List.map (fun x -> x.ToString())
            |> List.sort
        
        testList "DomainTypes.Tag" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Tag.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = Tag.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = Tag.TryParse x
                            x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = Tag.TryParse <| x.ToString()
                    match t with
                    | Some tag ->
                        t = Tag.TryParse tag.Value
                    | None ->
                        t = (Tag.TryParse <| x.ToString())

            testPropertyWithConfig config10k "is trim" <|
                fun  (x : NonEmptyString) ->

                    let t = Tag.TryParse <| x.ToString()
                    match t with
                    | Some tag ->
                        x.ToString().Trim() = tag.Value
                    | None ->
                        t = t
                    
            testPropertyWithConfig config10k "set creation" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genNonEmptyNonAllWhitespaceStringList())
                        (fun (xs : string list) -> 
                            let distinctList = distinctList xs  
                            let setOfTags = tagSet distinctList
                            let returnedList = listFromSetOftags setOfTags
                   
                            distinctList = returnedList)

            testPropertyWithConfig config10k "set is unique" <|
                fun  (xs : list<NonEmptyString>) ->
                    let distinctList = distinctList xs                   
                    let setOfTags = tagSet (distinctList @ distinctList)
                    let returnedList = listFromSetOftags setOfTags

                    let filteredDistinctList =
                        distinctList 
                        |> List.map (fun x -> x.Trim())
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                   
                    filteredDistinctList = returnedList

            testPropertyWithConfig config10k "ordered" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genNonEmptyNonAllWhitespaceStringList())
                        (fun (xs : string list) -> 
                            let listOfTags = 
                                xs  
                                |> List.choose Tag.TryParse

                            let stringFromTagsOrdered =
                                listOfTags
                                |> List.map (fun x -> x.Value )
                                |> List.sort

                            let orderedTags = listOfTags |> List.sort

                            let tagsFromOrderedList =
                                stringFromTagsOrdered
                                |> List.choose Tag.TryParse
                                
                            tagsFromOrderedList = orderedTags)
        ]

    [<Tests>]
    let testTrimNonEmptyString =
        testList "DomainTypes.TrimNonEmptyString" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryParse x
                            x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.TryParse 
                    match t with
                    | Some s ->
                        t = TrimNonEmptyString.TryParse s.Value
                    | None ->
                        let t2 = 
                            x.ToString()
                            |> TrimNonEmptyString.TryParse
                        t = t2

            testPropertyWithConfig config10k "is trim" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> TrimNonEmptyString.TryParse
                    match t with
                    | Some s ->
                        x.ToString().Trim() = s.Value
                    | None ->
                        t = t

            testCase "TryParse None on None" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryParse None) "Expected None"

            testPropertyWithConfig config10k "TryParse on Some x" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryParse (Some x)
                            x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "Parse on string list" <|
                    fun  (xs : list<string>) ->

                    let listNonEmptyStringSorted = 
                        TrimNonEmptyString.Parse xs
                        |> List.sort
                        |> List.map (fun x -> x.Value)

                    let filteredListSorted =
                        xs
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                        |> List.map (fun x -> x.Trim())
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                        |> List.sort
                   
                    filteredListSorted = listNonEmptyStringSorted

            testPropertyWithConfig config10k "list equality" <|
                    fun  (xs : list<string>) ->

                    let listTrimNonEmptyStringSorted = 
                        TrimNonEmptyString.Parse xs

                    let list2 =
                        listTrimNonEmptyStringSorted
                        |> List.map (fun x -> x.Value)
                        |> TrimNonEmptyString.Parse 

                    list2 = listTrimNonEmptyStringSorted
        ]

    [<Tests>]
    let testDigits =
        testList "DomainTypes.Digits" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Digits.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits.TryParse x
                           t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let t = Digits.TryParse <| digits.ToString()
                    (digits.ToString()) = t.Value.Value

                    //to do: digit strings wrapped in whitespace
            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsInWhiteSpace())
                        (fun (x : string) -> 
                           let t = Digits.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let t = Digits.TryParse <| digits.ToString()
                    let t2 = Digits.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits.TryParse "555"; Digits.TryParse "111"; Digits.TryParse "33"; Digits.TryParse "2"; ]
                    |> List.sort
                Expect.isTrue (ordered = [Digits.TryParse "111"; Digits.TryParse "2"; Digits.TryParse "33"; Digits.TryParse "555"; ])
                    "expected equality"
        ]

    [<Tests>]
    let testDigits2 =
        testList "DomainTypes.Digits2" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Digits2.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits2.TryParse x
                           t.IsNone)

            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = Digits2.TryParse <| invalidDigits digits 2
                    t.IsNone

            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.TryParse validDigit
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 2)
                        (fun (x : string) -> 
                           let t = Digits2.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = Digits2.TryParse validDigit
                    let t2 = Digits2.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits2.TryParse "55"; Digits2.TryParse "11"; Digits2.TryParse "33"; Digits2.TryParse "22"; ]
                    |> List.sort
                Expect.isTrue (ordered = [Digits2.TryParse "11"; Digits2.TryParse "22"; Digits2.TryParse "33"; Digits2.TryParse "55"; ])
                    "expected equality"
        ]

    [<Tests>]
    let testDigits3 =
        testList "DomainTypes.Digits3" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Digits3.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits3.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = Digits3.TryParse <| invalidDigits digits 3
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 3
                    let t = Digits3.TryParse validDigit
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 3)
                        (fun (x : string) -> 
                           let t = Digits3.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 3
                    let t = Digits3.TryParse validDigit
                    let t2 = Digits3.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits3.TryParse "555"; Digits3.TryParse "111"; Digits3.TryParse "223"; Digits3.TryParse "222"; ]
                    |> List.sort
                Expect.isTrue (ordered = [Digits3.TryParse "111"; Digits3.TryParse "222"; Digits3.TryParse "223"; Digits3.TryParse "555"; ])
                    "expected equality"
        ]

    [<Tests>]
    let testDigits4 =
        testList "DomainTypes.Digits4" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (Digits4.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = Digits4.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = Digits4.TryParse <| invalidDigits digits 4
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 4
                    let t = Digits4.TryParse validDigit
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 4)
                        (fun (x : string) -> 
                           let t = Digits4.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 4
                    let t = Digits4.TryParse validDigit 
                    let t2 = Digits4.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [Digits4.TryParse "5555"; Digits4.TryParse "1111"; Digits4.TryParse "2232"; Digits4.TryParse "2222"; ]
                    |> List.sort
                Expect.isTrue (ordered = [Digits4.TryParse "1111"; Digits4.TryParse "2222"; Digits4.TryParse "2232"; Digits4.TryParse "5555"; ])
                    "expected equality"
        ]

    [<Tests>]
    let testFullName =
        testList "DomainTypes.FullName" [
            testCase "TryParse None on all empty" <| fun () ->
                Expect.isNone (FullName.TryParse (None, [], (Some System.String.Empty), NameOrder.Western, Set.empty<Tag>) ) "Expected None"

            testPropertyWithConfig config10k "equality" <|
                fun  (fullName : FullName) ->
                    let first = fullName.First |> Option.map (fun x -> x.Value)
                    let middle = fullName.Middle |> List.map (fun x -> x.ToString())
                    let family = fullName.Family |> Option.map (fun x -> x.Value)

                    let t = FullName.TryParse (first, middle, family, NameOrder.Western, Set.empty<Tag>)
 
                    t.Value = fullName

            testCase "ordered on first name" <| fun () ->
                let ordered =
                    [FullName.TryParse (Some "5555", ["1 middle"; "2 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "1111", ["1 middle"; "2 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "2232", ["1 middle"; "2 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "2222", ["1 middle"; "2 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [FullName.TryParse (Some "1111", ["1 middle"; "2 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                                            FullName.TryParse (Some "2222", ["1 middle"; "2 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                                            FullName.TryParse (Some "2232", ["1 middle"; "2 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                                            FullName.TryParse (Some "5555", ["1 middle"; "2 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); ])
                    "expected equality"

            testCase "ordered on middle names" <| fun () ->
                let ordered =
                    [FullName.TryParse (Some "John", ["3 middle"; "4 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "John", ["4 middle"; "5 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "John", ["4 middle"; "4 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "John", ["3 middle";], Some "SameLast", NameOrder.Western, Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [FullName.TryParse (Some "John", ["3 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                                            FullName.TryParse (Some "John", ["3 middle"; "4 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                                            FullName.TryParse (Some "John", ["4 middle"; "4 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); 
                                            FullName.TryParse (Some "John", ["4 middle"; "5 middle"], Some "SameLast", NameOrder.Western, Set.empty<Tag>); ])
                    "expected equality"

            testCase "ordered on family" <| fun () ->
                let ordered =
                    [FullName.TryParse (Some "John", ["3 middle"; "4 middle"], Some "b", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "John", ["4 middle"; "5 middle"], Some "aaa", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "John", ["4 middle"; "4 middle"], Some "d", NameOrder.Western, Set.empty<Tag>); 
                    FullName.TryParse (Some "John", ["3 middle";], Some "cc", NameOrder.Western, Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [FullName.TryParse (Some "John", ["4 middle"; "5 middle"], Some "aaa", NameOrder.Western, Set.empty<Tag>);
                                            FullName.TryParse (Some "John", ["3 middle"; "4 middle"], Some "b", NameOrder.Western, Set.empty<Tag>);
                                            FullName.TryParse (Some "John", ["3 middle";], Some "cc", NameOrder.Western, Set.empty<Tag>); 
                                            FullName.TryParse (Some "John", ["4 middle"; "4 middle"], Some "d", NameOrder.Western, Set.empty<Tag>); ])
                    "expected equality"
        ]

    [<Tests>]
    let testPersonName =
        testList "DomainTypes.PersonName" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (PersonName.TryParse (System.String.Empty, Set.empty<Tag>)) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = PersonName.TryParse (x, Set.empty<Tag>)
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = PersonName.TryParse (x, Set.empty<Tag>)
                            x.Trim() = t.Value.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        (x.ToString(), Set.empty<Tag>)
                        |> PersonName.TryParse 
                    match t with
                    | Some s ->
                        t = PersonName.TryParse (s.Value.Value, Set.empty<Tag>)
                    | None ->
                        let t2 = 
                            (x.ToString(), Set.empty<Tag>)
                            |> PersonName.TryParse
                        t = t2

            testCase "ordered" <| fun () ->
                let ordered =
                    [PersonName.TryParse ("b", Set.empty<Tag>); 
                    PersonName.TryParse ("aaa", Set.empty<Tag>); 
                    PersonName.TryParse ("d", Set.empty<Tag>); 
                    PersonName.TryParse ("cc", Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [PersonName.TryParse ("aaa", Set.empty<Tag>);
                                            PersonName.TryParse ("b", Set.empty<Tag>);
                                            PersonName.TryParse ("cc", Set.empty<Tag>); 
                                            PersonName.TryParse ("d", Set.empty<Tag>); ])
                    "expected equality"
        ]

    [<Tests>]
    let testNameAndAffixes =
        testList "DomainTypes.NameAndAffixes" [
            testCase "TryParse None on all empty" <| fun () ->
                Expect.isNone (NameAndAffixes.TryParse ([], System.String.Empty,[], Set.empty<Tag>) ) "Expected None"

            testPropertyWithConfig config10k "equality" <|
                fun  () ->
                    Prop.forAll (DomainGenerators.NameAndAffixes()) // (Arb.fromGen <| genNameAndAffixes())
                        (fun (nameAndAffixes : NameAndAffixes) -> 

                                let t =  
                                    let salutations = nameAndAffixes.Salutations |> List.map (fun x -> x.Value)
                                    let personName = nameAndAffixes.PersonName.Value.Value
                                    let suffixes = nameAndAffixes.Suffixes |> List.map (fun x -> x.Value)
                                    NameAndAffixes.TryParse (salutations, personName, suffixes, Set.empty<Tag>)

                                t.Value = nameAndAffixes
                            )

            testCase "ordered" <| fun () ->
                let ordered =
                    [NameAndAffixes.TryParse ([], "bb", [], Set.empty<Tag>); 
                    NameAndAffixes.TryParse ([], "b", ["IV"], Set.empty<Tag>); 
                    NameAndAffixes.TryParse ([], "b", ["III"], Set.empty<Tag>);
                    NameAndAffixes.TryParse (["Mr"], "b", [], Set.empty<Tag>); 
                    NameAndAffixes.TryParse (["Mr"], "c", [], Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [NameAndAffixes.TryParse ([], "b", ["III"], Set.empty<Tag>);
                                            NameAndAffixes.TryParse  ([], "b", ["IV"], Set.empty<Tag>);
                                            NameAndAffixes.TryParse (["Mr"], "b", [], Set.empty<Tag>);
                                            NameAndAffixes.TryParse ([], "bb", [], Set.empty<Tag>);
                                            NameAndAffixes.TryParse (["Mr"], "c", [], Set.empty<Tag>);])
                    "expected equality"
        ]

    [<Tests>]
    let testZipCode5 =
        testList "DomainTypes.ZipCode5" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (ZipCode5.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = ZipCode5.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = ZipCode5.TryParse <| invalidDigits digits 5
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 5
                    let t = ZipCode5.TryParse validDigit
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genDigitsOfLengthInWhiteSpace 5)
                        (fun (x : string) -> 
                           let t = ZipCode5.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 5
                    let t = ZipCode5.TryParse validDigit
                    let t2 = ZipCode5.TryParse t.Value.Value
                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [ZipCode5.TryParse "55555"; ZipCode5.TryParse "11111"; ZipCode5.TryParse "22322"; ZipCode5.TryParse "22222"; ]
                    |> List.sort
                Expect.isTrue (ordered = [ZipCode5.TryParse "11111"; ZipCode5.TryParse "22222"; ZipCode5.TryParse "22322"; ZipCode5.TryParse "55555"; ])
                    "expected equality"
        ]

    [<Tests>]
    let testZipCode5Plus4 =
        let valid5Plus4 seed =
            let validDigit = validDigits seed 9
            ZipCode5Plus4.TryParse validDigit
            
        testList "DomainTypes.ZipCode5Plus4" [
            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (ZipCode5Plus4.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonDigitalString())
                        (fun (x : string) -> 
                           let t = ZipCode5Plus4.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = ZipCode5Plus4.TryParse <| invalidDigits digits 9
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 9
                    let t = ZipCode5Plus4.TryParse validDigit
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "TryParse trims" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| inputZip5Plus4())
                        (fun (x : string) -> 
                           let t = ZipCode5Plus4.TryParse x
                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality 1" <|
                fun  (digits : NonNegativeInt) ->
                    let t = valid5Plus4 digits
                    let t2 = ZipCode5Plus4.TryParse t.Value.Value

                    t2 = t

            testPropertyWithConfig config10k "equality 2" <|
                fun  (digits : NonNegativeInt) ->
                    let t = valid5Plus4 digits
                    
                    let zip5 = t.Value.Value.Substring(0,5)
                    let suffix = t.Value.Value.Substring(5)

                    let t2 = ZipCode5Plus4.TryParse <| sprintf "%s-%s" zip5 suffix

                    t2 = t
            testPropertyWithConfig config10k "equality 3" <|
                fun  (digits : NonNegativeInt) ->
                    let t = valid5Plus4 digits
                    
                    let zip5 = t.Value.Value.Substring(0,5)
                    let suffix = t.Value.Value.Substring(5)

                    let t2 = ZipCode5Plus4.TryParse <| sprintf " %s - %s " zip5 suffix

                    t2 = t

            testPropertyWithConfig config10k "equality 4" <|
                fun  (digits : NonNegativeInt) ->
                    let t = valid5Plus4 digits
                    
                    let zip5 = t.Value.Value.Substring(0,5)
                    let suffix = t.Value.Value.Substring(5)

                    let t2 = ZipCode5Plus4.TryParse <| sprintf " %s   %s " zip5 suffix

                    t2 = t

            testCase "ordered" <| fun () ->
                let ordered =
                    [ZipCode5Plus4.TryParse "555559999"; ZipCode5Plus4.TryParse "111119999"; ZipCode5Plus4.TryParse "223229999"; ZipCode5Plus4.TryParse "222229999"; ]
                    |> List.sort
                Expect.isTrue (ordered = [ZipCode5Plus4.TryParse "111119999"; ZipCode5Plus4.TryParse "222229999"; ZipCode5Plus4.TryParse "223229999"; ZipCode5Plus4.TryParse "555559999"; ])
                    "expected equality"
        ]

    [<Tests>]
    let testNonUsPostalCode =
        testList "DomainTypes.NonUsPostalCode" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (NonUsPostalCode.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = NonUsPostalCode.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = NonUsPostalCode.TryParse x
                            x.Trim() = t.Value.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = 
                        x.ToString()
                        |> NonUsPostalCode.TryParse 
                    match t with
                    | Some s ->
                        t = NonUsPostalCode.TryParse s.Value.Value
                    | None ->
                        let t2 = 
                            x.ToString()
                            |> NonUsPostalCode.TryParse
                        t = t2

            testCase "ordered" <| fun () ->
                let ordered =
                    [NonUsPostalCode.TryParse "a555559999"; NonUsPostalCode.TryParse "a111119999"; NonUsPostalCode.TryParse "a223229999"; NonUsPostalCode.TryParse "a222229999"; ]
                    |> List.sort
                Expect.isTrue (ordered = [NonUsPostalCode.TryParse "a111119999"; NonUsPostalCode.TryParse "a222229999"; NonUsPostalCode.TryParse "a223229999"; NonUsPostalCode.TryParse "a555559999"; ])
                    "expected equality"
        ]

    let zip555559999 = (ZipCode5Plus4.TryParse "555559999").Value
    let zip223221111 = (ZipCode5Plus4.TryParse "223221111").Value
    let zip55555 = ZipCode.ZipCode5 (ZipCode5.TryParse "55555").Value
    let zip22322 = ZipCode.ZipCode5 (ZipCode5.TryParse "22322").Value

    [<Tests>]
    let testZipCode =
        testList "DomainTypes.ZipCode" [
            testCase "ordered" <| fun () ->
                
                let ordered =
                    [ZipCode.ZipCode5 (ZipCode5.TryParse "55555").Value; 
                    ZipCode.ZipCode5Plus4 zip555559999; 
                    ZipCode.ZipCode5Plus4 zip223221111; 
                    ZipCode.ZipCode5 (ZipCode5.TryParse "22322").Value; ]
                    |> List.sort
                Expect.isTrue (ordered = [ZipCode.ZipCode5 (ZipCode5.TryParse "22322").Value; ZipCode.ZipCode5Plus4 zip223221111 ; ZipCode.ZipCode5 (ZipCode5.TryParse "55555").Value; ZipCode.ZipCode5Plus4 zip555559999; ])
                    "expected equality"
        ]

    [<Tests>]
    let testPostalCode =
        testList "DomainTypes.PostalCode" [
            testCase "ordered" <| fun () ->
                let ordered =
                    [PostalCode.ZipCode zip55555; 
                    PostalCode.NonUsPostalCode (NonUsPostalCode.TryParse "55555a").Value; 
                    PostalCode.NonUsPostalCode (NonUsPostalCode.TryParse "22322a").Value;
                    PostalCode.ZipCode zip22322; ]
                    |> List.sort
                Expect.isTrue (ordered = [PostalCode.ZipCode zip22322; 
                                            PostalCode.NonUsPostalCode (NonUsPostalCode.TryParse "22322a").Value;  
                                            PostalCode.ZipCode zip55555; 
                                            PostalCode.NonUsPostalCode (NonUsPostalCode.TryParse "55555a").Value; ])
                    "expected equality"
        ]

    [<Tests>]
    let testPhysicalAddress =
        testList "DomainTypes.PhysicalAddress" [
            testCase "TryParse None on all empty" <| fun () ->
                Expect.isNone (PhysicalAddress.TryParse ([], (Some System.String.Empty), None, None, None, Set.empty<Tag>) ) "Expected None"

            testPropertyWithConfig config10k "equality" <|
                 fun  () ->
                    Prop.forAll (Arb.fromGen <| genPhysicalAddress())
                        (
                            fun  (physicalAddress : PhysicalAddress) ->
                                let streetAddress = physicalAddress.StreetAddress |> List.map (fun x -> x.ToString())
                                let city = physicalAddress.City |> Option.map (fun x -> x.Value)
                                let state = physicalAddress.State |> Option.map (fun x -> x.Value)
                                let postalCode = physicalAddress.PostalCode |> Option.map (fun x -> x.ToString())
                                let country = physicalAddress.Country |> Option.map (fun x -> x.Value)

                                let t = PhysicalAddress.TryParse (streetAddress, city, state, postalCode, country, Set.empty<Tag>)
 
                                t.Value = physicalAddress
                        )

            testCase "ordered on country" <| fun () ->
                let ordered =
                    [PhysicalAddress.TryParse ([], None, None, None, Some "a", Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, None, None, Some "c", Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, None, None, Some "b", Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, None, None, Some "d", Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [PhysicalAddress.TryParse ([], None, None, None, Some "a",  Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], None, None, None, Some "b", Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], None, None, None, Some "c", Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], None, None, None, Some "d", Set.empty<Tag>); ])
                    "expected equality"

            testCase "ordered on postalcode" <| fun () ->
                let ordered =
                    [PhysicalAddress.TryParse ([], None, None, Some "55555", None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, None, Some "55555a", None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, None, Some "22322a", None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, None, Some "22322", None,  Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [PhysicalAddress.TryParse ([], None, None, Some "22322", None, Set.empty<Tag>);
                                            PhysicalAddress.TryParse ([], None, None, Some "22322a", None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], None, None, Some "55555", None,  Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], None, None, Some "55555a", None, Set.empty<Tag>); ])
                    "expected equality"

            testCase "ordered on state" <| fun () ->
                let ordered =
                    [PhysicalAddress.TryParse ([], None, Some "a", None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, Some "c", None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, Some "b", None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], None, Some "d", None, None, Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [PhysicalAddress.TryParse ([], None, Some "a",  None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], None, Some "b", None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], None, Some "c", None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], None, Some "d", None, None, Set.empty<Tag>); ])
                    "expected equality"

            testCase "ordered on city" <| fun () ->
                let ordered =
                    [PhysicalAddress.TryParse ([], Some "a", None, None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], Some "c", None, None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], Some "b", None, None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse ([], Some "d", None, None, None, Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [PhysicalAddress.TryParse ([], Some "a",  None, None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], Some "b", None, None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], Some "c", None, None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse ([], Some "d", None, None, None, Set.empty<Tag>); ])
                    "expected equality"

            testCase "ordered on address" <| fun () ->
                let ordered =
                    [PhysicalAddress.TryParse (["5 address"; "5 address"], None, None, None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse (["1 address"; "1 address"], None, None, None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse (["5 address"], None, None, None, None, Set.empty<Tag>); 
                    PhysicalAddress.TryParse (["1 address"; "2 address"], None, None, None, None, Set.empty<Tag>); ]
                    |> List.sort
                Expect.isTrue (ordered = [PhysicalAddress.TryParse (["1 address"; "1 address"], None,  None, None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse (["1 address"; "2 address"], None, None, None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse (["5 address"], None, None, None, None, Set.empty<Tag>); 
                                            PhysicalAddress.TryParse (["5 address"; "5 address"], None, None, None, None, Set.empty<Tag>); ])
                    "expected equality"
        ]

    [<Tests>]
    let testEmailAddress =

        let makeList nonEmptyStrings = 
            nonEmptyStrings
            |> List.choose (fun x -> EmailAddress.TryParse (x, Set.empty<Tag>))
        
        testList "DomainTypes.EmailAddress" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (EmailAddress.TryParse (System.String.Empty, Set.empty<Tag>)) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = EmailAddress.TryParse (x, Set.empty<Tag>)
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = EmailAddress.TryParse (x, Set.empty<Tag>)
                            match t with
                            | Some emailAddress ->
                                x.Trim() = emailAddress.Value
                            | None -> 
                                x = x)

            testPropertyWithConfig config10k "equality" <|
                fun  (x : NonEmptyString) ->

                    let t = EmailAddress.TryParse (x.ToString(), Set.empty<Tag>)
                    match t with
                    | Some emailAddress ->
                        t = EmailAddress.TryParse (emailAddress.Value, Set.empty<Tag>)
                    | None ->
                        t = EmailAddress.TryParse (x.ToString(), Set.empty<Tag>)

            testPropertyWithConfig config10k "is trim" <|
                fun  (x : NonEmptyString) ->

                    let t = EmailAddress.TryParse (x.ToString(), Set.empty<Tag>)
                    match t with
                    | Some emailAddress ->
                        x.ToString().Trim() = emailAddress.Value
                    | None ->
                        t = t

            testPropertyWithConfig config10k "ordered" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genNonEmptyNonAllWhitespaceStringList())
                        (fun (xs : string list) -> 
                            let listOfEmailAddresss = makeList xs

                            let stringFromEmailAddresssOrdered =
                                listOfEmailAddresss
                                |> List.map (fun x -> x.Value )
                                |> List.sort

                            let orderedEmailAddresss = listOfEmailAddresss |> List.sort

                            let emailAddresssFromOrderedList = makeList stringFromEmailAddresssOrdered
                                
                            emailAddresssFromOrderedList = orderedEmailAddresss
                            )
        ]

    [<Tests>]
    let testUsPhone =

        testList "DomainTypes.UsPhone" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (UsPhone.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = UsPhone.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genUsPhone())
                        (fun (x : string) -> 
                            let t = UsPhone.TryParse x 
                            t.IsSome)
                            
            testPropertyWithConfig config10k "equality" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genUsPhone())
                        (fun  (x : string) ->

                            let t = UsPhone.TryParse x
                            match t with
                            | Some usPhone ->
                                t = UsPhone.TryParse usPhone.Value.Value
                            | None ->
                                t = UsPhone.TryParse x
                        )

            testPropertyWithConfig config10k "ordered" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genUsPhoneList())
                        (fun (xs : string list) -> 
                            let listOfUsPhones =
                                xs 
                                |> List.choose UsPhone.TryParse

                            let stringFromUsPhonesOrdered =
                                let phone10s, phone7s =
                                    listOfUsPhones
                                    |> List.partition (fun x -> x.Value.Value.Length = 10)
                                    
                                let phone10Ordered =
                                    phone10s
                                    |> List.map (fun x -> x.Value.Value)
                                    |> List.sort

                                let phone7Ordered =
                                    phone7s
                                    |> List.map (fun x -> x.Value.Value)
                                    |> List.sort

                                phone10Ordered @ phone7Ordered

                            let orderedUsPhones = 
                                listOfUsPhones 
                                |> List.sort 
                                |> List.map (fun x -> x.Value.Value)
                                
                            stringFromUsPhonesOrdered = orderedUsPhones
                            )
        ]

    [<Tests>]
    let tesuOtherPhone =

        testList "DomainTypes.OtherPhone" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (OtherPhone.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| whitespaceString())
                        (fun (x : string) -> 
                            let t = OtherPhone.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genOtherPhone())
                        (fun (x : string) -> 
                            let t = OtherPhone.TryParse x 
                            t.IsSome)
                            
            testPropertyWithConfig config10k "equality" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genOtherPhone())
                        (fun  (x : string) ->

                            let t = OtherPhone.TryParse x
                            match t with
                            | Some usPhone ->
                                t = OtherPhone.TryParse usPhone.Value.Value
                            | None ->
                                t = OtherPhone.TryParse x
                        )

            testPropertyWithConfig config10k "ordered" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genOtherPhoneList())
                        (fun (xs : string list) -> 
                            
                            let listOfOtherPhones : OtherPhone list =
                                xs 
                                |> List.choose OtherPhone.TryParse

                            let stringFromOtherPhonesOrdered =

                                listOfOtherPhones
                                |> List.map (fun x -> x.Value.Value)
                                |> List.sort

                            let orderedOtherPhones = 
                                listOfOtherPhones 
                                |> List.sort 
                                |> List.map (fun x -> x.Value.Value)
                                
                            stringFromOtherPhonesOrdered = orderedOtherPhones
                            )
        ]

    [<Tests>]
    let testPhone =
        testList "DomainTypes.Phone" [
            testPropertyWithConfig config10k "ordered" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genPhoneList())
                        (fun (xs : string list) -> 
                            let listOfPhones =
                                xs 
                                |> List.choose Phone.TryParse

                            let stringPhonesOrdered =
                                listOfPhones
                                |> List.map (fun x -> x.Value.Value)
                                |> List.sort

                            let orderedPhones = 
                                listOfPhones 
                                |> List.sort 
                                |> List.map (fun x -> x.Value.Value)
                                
                            stringPhonesOrdered = orderedPhones
                            )
        ]

    [<Tests>]
    let testPhoneNumber =
        testList "DomainTypes.PhoneNumber" [
             testPropertyWithConfig config10k "equality" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genPhoneNumber())
                        (fun  (callingCode, phoneRaw, extension, phoneNumber) ->

                            let phone = Phone.TryParse phoneRaw
                            let t1 = PhoneNumber.TryParse (callingCode, phone.Value, extension, Set.empty<Tag>)
                            let t2 = PhoneNumber.TryParse (phoneNumber, Set.empty<Tag>)

                            t1 = t2
                        )
        ]

    [<Tests>]
    let testUriTagged =
        testList "DomainTypes.PhoneNumber" [
             testPropertyWithConfig config10k "equality" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genUriTagged())
                        (fun  (uri) ->

                            let t1 = UriTagged.TryParse (uri, Set.empty<Tag>)

                            match t1 with
                            | Some x -> 
                                let t2 = UriTagged.TryParse (x.ToString(), Set.empty<Tag>)
                                t1.Value.Uri = x.Uri
                            | None ->
                                true
                        )
        ]
