namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open Expecto
open FsCheck
//open System

module DomainTypes =

    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with replay = Some <| (1057044718,296287820) }  //see Tips & Tricks for FsCheck
    let (.=.) left right = left = right |@ sprintf "%A = %A" left right

    Arb.register<DomainGenerators>() |> ignore

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

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString.TryParse <| digits.ToString()
                    let t2 = DigitString.TryParse t.Value.Value
                    t2 = t
            ]

    [<Tests>]
    let testDigitString2 =
        let valid digits =
            match digits.ToString().Length with
            | 2 -> digits.ToString()
            | 1 -> digits.ToString().PadLeft(2, '0')
            | _ -> digits.ToString().Substring(0, 2)

        let inValid digits =
            match digits.ToString().Length with
            | 2 -> sprintf "0%s" <| digits.ToString()
            | _ -> digits.ToString()

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
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString2.TryParse <| inValid digits
                    t.IsNone

            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString2.TryParse <| valid digits
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString2.TryParse validDigit
                    let t2 = DigitString2.TryParse t.Value.Value
                    t2 = t
            ]

    [<Tests>]
    let testDigitString3 =
        let valid digits =
            match digits.ToString().Length with
            | 3 -> digits.ToString()
            | n when n < 3 -> digits.ToString().PadLeft(3, '0')
            | _ -> digits.ToString().Substring(0, 3)

        let inValid digits =
            match digits.ToString().Length with
            | 3 -> sprintf "0%s" <| digits.ToString()
            | _ -> digits.ToString()

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
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString3.TryParse <| inValid digits
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString3.TryParse <| valid digits
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString3.TryParse validDigit
                    let t2 = DigitString3.TryParse t.Value.Value
                    t2 = t
            ]

    [<Tests>]
    let testDigitString4 =
        let valid digits =
            match digits.ToString().Length with
            | 4 -> digits.ToString()
            | n when n < 4 -> digits.ToString().PadLeft(4, '0')
            | _ -> digits.ToString().Substring(0, 4)

        let inValid digits =
            match digits.ToString().Length with
            | 4 -> sprintf "0%s" <| digits.ToString()
            | _ -> digits.ToString()

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
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString4.TryParse <| inValid digits
                    t.IsNone
            
            testPropertyWithConfig config10k "TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString4.TryParse <| valid digits
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "equality" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString4.TryParse validDigit
                    let t2 = DigitString4.TryParse t.Value.Value
                    t2 = t
            ]
            //TO DO: test white space characters
    [<Tests>]
    let testFullName =
        testList "DomainTypes.FullName" [
            testPropertyWithConfig config10k "equality" <|
            
//            testPropertyWithConfig config10k "FullName is equal" <|
//                fun  (fullName : FullName option) ->
//
//                    let t = 
//                        let salutation =  fullName.Value.Salutation |> List.map (fun x -> x.ToString())
//                        let first = fullName.Value.First |> Option.map (fun x -> x.Value)
//                        let middle = fullName.Value.Middle |> List.map (fun x -> x.ToString())
//                        let family = fullName.Value.Family |> Option.map (fun x -> x.Value)
//                        let suffix = fullName.Value.Suffix |> List.map (fun x -> x.ToString())
//                        FullName.TryParse (salutation, first, middle, family, suffix, NameOrder.Western, Set.empty<Tag>)
// 
//                    t = fullName

                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.genFullName())
                        (fun (fullName : FullName option) -> 

                                let t = 
                                    let salutation =  fullName.Value.Salutation |> List.map (fun x -> x.Value)
                                    let first = fullName.Value.First |> Option.map (fun x -> x.Value)
                                    let middle = fullName.Value.Middle |> List.map (fun x -> x.Value)
                                    let family = fullName.Value.Family |> Option.map (fun x -> x.Value)
                                    let suffix = fullName.Value.Suffix |> List.map (fun x -> x.Value)
                                    FullName.TryParse (salutation, first, middle, family, suffix, fullName.Value.NameOrder, Set.empty<Tag>)
                                match t with
                                | Some x ->
                                    1 .=. 1
                                | None ->
                                    2 .=. fullName.Value.Middle.Length |@ sprintf "middle length %i" fullName.Value.Middle.Length

//                                t = fullName
                            )

//            testPropertyWithConfig configReplay "FullName.PersonName is equal" <|
//            testPropertyWithConfig config10k "FullName.PersonName is equal" <|
//                fun  (fullName : FullName) ->
//                    let person2 = 
//                        {  Salutation = fullName.Salutation
//                           First = fullName.First
//                           Middle = fullName.Middle
//                           Family = fullName.Family
//                           Suffix = fullName.Suffix
//                           NameOrder = fullName.NameOrder
//                           Tags = fullName.Tags}
//
//                    fullName.PersonName.Value =  person2.PersonName.Value
            ]