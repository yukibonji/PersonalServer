namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open Expecto
open FsCheck

module DomainTypes =

// arbitrary = [typeof<DomainGenerators>] breaks visual studio test discovery
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 ; arbitrary = [typeof<DomainGenerators>] }
//    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    let validDigits digits length =
        if digits.ToString().Length = length then
            digits.ToString()
        elif digits.ToString().Length < length then
            digits.ToString().PadLeft(length, '0')
        else
            digits.ToString().Substring(0, length)

    let invalidDigits digits length =
        if digits.ToString().Length = length then
            sprintf "0%s" <| digits.ToString()
        else
            digits.ToString()

    [<Tests>]
    let testTag =

        let distinctList nonEmptyStrings = 
            nonEmptyStrings
            |> List.map (fun x -> x.ToString().Trim())
            |> List.distinct
            |> List.sort

        let tagSet distinctList =
            distinctList
            |> List.map (fun x -> Tag.TryParse <| x.ToString())
            |> List.choose id
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
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.whitespaceString())
                        (fun (x : string) -> 
                            let t = Tag.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonEmptyNonAllWhitespaceString())
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
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.genNonEmptyNonAllWhitespaceStringList())
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
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.genNonEmptyNonAllWhitespaceStringList())
                        (fun (xs : string list) -> 
                            let listOfTags = 
                                xs  
                                |> List.map Tag.TryParse
                                |> List.choose id

                            let stringFromTagsOrdered =
                                listOfTags
                                |> List.map (fun x -> x.Value )
                                |> List.sort

                            let orderedTags = listOfTags |> List.sort

                            let tagsFromOrderedList =
                                stringFromTagsOrdered
                                |> List.map Tag.TryParse
                                |> List.choose id
                                
                            tagsFromOrderedList = orderedTags)
            ]

    [<Tests>]
    let testTrimNonEmptyString =
        testList "DomainTypes.TrimNonEmptyString" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (TrimNonEmptyString.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.whitespaceString())
                        (fun (x : string) -> 
                            let t = TrimNonEmptyString.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonEmptyNonAllWhitespaceString())
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
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonEmptyNonAllWhitespaceString())
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
    let testDigitString =
        testList "DomainTypes.DigitString" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
                        (fun (x : string) -> 
                           let t = DigitString.TryParse x
                           t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString.TryParse <| digits.ToString()
                    (digits.ToString()) = t.Value.Value

                    //to do: digit strings wrapped in whitespace
//            testPropertyWithConfig config10k "TryParse trims" <|
//                fun  () ->
//                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.genDigitsInWhiteSpace())
//                        (fun (x : string) -> 
//                           let t = DigitString.TryParse x
//                           x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString.TryParse <| digits.ToString()
                    let t2 = DigitString.TryParse t.Value.Value
                    t2 = t
            ]

    [<Tests>]
    let testDigitString2 =
        testList "DomainTypes.DigitString2" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString2.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
                        (fun (x : string) -> 
                           let t = DigitString2.TryParse x
                           t.IsNone)

            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString2.TryParse <| invalidDigits digits 2
                    t.IsNone

            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = DigitString2.TryParse <| validDigits digits 2
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 2
                    let t = DigitString2.TryParse validDigit
                    let t2 = DigitString2.TryParse t.Value.Value
                    t2 = t
            ]

    [<Tests>]
    let testDigitString3 =
        testList "DomainTypes.DigitString3" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString3.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
                        (fun (x : string) -> 
                           let t = DigitString3.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString3.TryParse <| invalidDigits digits 3
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 3
                    let t = DigitString3.TryParse <| validDigits digits 3
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 3
                    let t = DigitString3.TryParse validDigit
                    let t2 = DigitString3.TryParse t.Value.Value
                    t2 = t
            ]

    [<Tests>]
    let testDigitString4 =
        testList "DomainTypes.DigitString4" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString4.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
                        (fun (x : string) -> 
                           let t = DigitString4.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "TryParse None on wrong length digital string" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString4.TryParse <| invalidDigits digits 4
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits
                    let t = DigitString4.TryParse <| validDigits digits 4
                    validDigit 4 = t.Value.Value

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 4
                    let t = DigitString4.TryParse validDigit 
                    let t2 = DigitString4.TryParse t.Value.Value
                    t2 = t
            ]
            //TO DO: test white space characters
    [<Tests>]
    let testFullName =
        testList "DomainTypes.FullName" [
            testCase "TryParse None on all empty" <| fun () ->
                Expect.isNone (FullName.TryParse (None, [], (Some System.String.Empty), NameOrder.Western, Set.empty<Tag>) ) "Expected None"


// this test requires  arbitrary = [typeof<DomainGenerators>] 

//            testPropertyWithConfig config10k "equality" <|
//                fun  (fullName : FullName) ->
//                    let first = fullName.First |> Option.map (fun x -> x.Value)
//                    let middle = fullName.Middle |> List.map (fun x -> x.ToString())
//                    let family = fullName.Family |> Option.map (fun x -> x.Value)
//
//                    let t = FullName.TryParse (first, middle, family,  NameOrder.Western, Set.empty<Tag>)
// 
//                    t.Value = fullName
            ]

    [<Tests>]
    let testPersonName =
        testList "DomainTypes.PersonName" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (PersonName.TryParse (System.String.Empty, Set.empty<Tag>)) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.whitespaceString())
                        (fun (x : string) -> 
                            let t = PersonName.TryParse (x, Set.empty<Tag>)
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonEmptyNonAllWhitespaceString())
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
            ]

    [<Tests>]
    let testNameAndAffixes =
        testList "DomainTypes.NameAndAffixes" [
            testCase "TryParse None on all empty" <| fun () ->
                Expect.isNone (NameAndAffixes.TryParse ([], System.String.Empty,[], Set.empty<Tag>) ) "Expected None"

            testPropertyWithConfig config10k "equality" <|
                fun  () ->
                    Prop.forAll (DomainGenerators.NameAndAffixes()) // (Arb.fromGen <| DomainGeneratorsCode.genNameAndAffixes())
                        (fun (nameAndAffixes : NameAndAffixes) -> 

                                let t =  
                                    let salutations = nameAndAffixes.Salutations |> List.map (fun x -> x.Value)
                                    let personName = nameAndAffixes.PersonName.Value.Value
                                    let suffixes = nameAndAffixes.Suffixes |> List.map (fun x -> x.Value)
                                    NameAndAffixes.TryParse (salutations, personName, suffixes, Set.empty<Tag>)

                                t.Value = nameAndAffixes
                            )
            ]

    [<Tests>]
    let testZipCode5 =
        testList "DomainTypes.ZipCode5" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (ZipCode5.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
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
                    let t = ZipCode5.TryParse <| validDigits digits 5
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = validDigits digits 5
                    let t = ZipCode5.TryParse validDigit
                    let t2 = ZipCode5.TryParse t.Value.Value
                    t2 = t
            ]

    [<Tests>]
    let testNonUsPostalCode =
        testList "DomainTypes.NonUsPostalCode" [

            testCase "TryParse None on empty string" <| fun () ->
                Expect.isNone (NonUsPostalCode.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "TryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.whitespaceString())
                        (fun (x : string) -> 
                            let t = NonUsPostalCode.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "TryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonEmptyNonAllWhitespaceString())
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
            ]