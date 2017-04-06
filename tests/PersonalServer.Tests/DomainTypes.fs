namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open Expecto
open FsCheck
//open System

module DomainTypes =

    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { FsCheckConfig.defaultConfig with replay = Some <| (1518258788,296285147) }  //see Tips & Tricks for FsCheck

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

            testCase "tryParse None on empty string" <| fun () ->
                Expect.isNone (Tag.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "tryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.whitespaceString())
                        (fun (x : string) -> 
                            let t = Tag.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "tryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = Tag.TryParse x
                            x.Trim() = t.Value.Value)

            testPropertyWithConfig config10k "set creation" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.genNonEmptyNonAllWhitespaceStringList())
                        (fun (xs : string list) -> 
                            let distinctList = distinctList xs  
                            let setOfTags = tagSet distinctList

                            let setOfTags2 =
                                distinctList
                                |> List.map (fun x -> Tag.TryParse <| x.ToString())
                                |> List.choose id
                                |> List.fold (fun (s : Set<Tag>) t -> s.Add t ) setOfTags

                            let returnedList = listFromSetOftags setOfTags2
                   
                            distinctList = returnedList)

            testPropertyWithConfig config10k "set is unique" <|
                fun  (xs : list<NonEmptyString>) ->
                    let distinctList = distinctList xs                   
                    let setOfTags = tagSet distinctList
                    let returnedList = listFromSetOftags setOfTags

                    let filteredDistinctList =
                        distinctList
                        |> List.map (fun x -> x.Trim())
                        |> List.filter (System.String.IsNullOrWhiteSpace >> not)
                   
                    filteredDistinctList = returnedList
            ]

    [<Tests>]
    let testNonEmptyString =
        testList "DomainTypes.NonEmptyString" [

            testCase "tryParse None on empty string" <| fun () ->
                Expect.isNone (Jackfoxy.PersonalServer.NonEmptyString.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "tryParse None on all white space string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.whitespaceString())
                        (fun (x : string) -> 
                            let t = Jackfoxy.PersonalServer.NonEmptyString.TryParse x
                            t.IsNone)

            testPropertyWithConfig config10k "tryParse" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = Jackfoxy.PersonalServer.NonEmptyString.TryParse x
                            x.Trim() = t.Value.Value)

            testCase "tryParse None on None" <| fun () ->
                Expect.isNone (Jackfoxy.PersonalServer.NonEmptyString.TryParse None) "Expected None"

            testPropertyWithConfig config10k "tryParse on Some x" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonEmptyNonAllWhitespaceString())
                        (fun (x : string) -> 
                            let t = Jackfoxy.PersonalServer.NonEmptyString.TryParse (Some x)
                            x.Trim() = t.Value.Value)
            //to do:
            //testPropertyWithConfig config10k "tryParse on string list" <|
            ]

    [<Tests>]
    let testDigitString =
        testList "DomainTypes.DigitString" [

            testCase "tryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "tryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
                        (fun (x : string) -> 
                           let t = DigitString.TryParse x
                           t.IsNone)

            testPropertyWithConfig config10k "tryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString.TryParse <| digits.ToString()
                    (digits.ToString()) = t.Value.Value
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

            testCase "tryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString2.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "tryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
                        (fun (x : string) -> 
                           let t = DigitString2.TryParse x
                           t.IsNone)

            testPropertyWithConfig config10k "tryParse None on wrong length digital string" <|
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString2.TryParse <| inValid digits
                    t.IsNone

            testPropertyWithConfig config10k "tryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString2.TryParse <| valid digits
                    validDigit = t.Value.Value
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

            testCase "tryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString3.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "tryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
                        (fun (x : string) -> 
                           let t = DigitString3.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "tryParse None on wrong length digital string" <|
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString3.TryParse <| inValid digits
                    t.IsNone
            
            testPropertyWithConfig config10k "tryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString3.TryParse <| valid digits
                    validDigit = t.Value.Value
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

            testCase "tryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString4.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "tryParse None on non-digital string" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| DomainGeneratorsCode.nonDigitalString())
                        (fun (x : string) -> 
                           let t = DigitString4.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "tryParse None on wrong length digital string" <|
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString4.TryParse <| inValid digits
                    t.IsNone
            
            testPropertyWithConfig config10k "tryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString4.TryParse <| valid digits
                    validDigit = t.Value.Value
            ]
            //TO DO: test white space characters
//    [<Tests>]
//    let testNameOfPerson =
//        testList "DomainTypes.NameOfPerson" [
//            testPropertyWithConfig config10k "PersonName is equal" <|
//                fun  (nonEmptyString : NonEmptyString) ->
//                    let t = PersonName ((Jackfoxy.PersonalServer.NonEmptyString (nonEmptyString.ToString())), Set.empty)
//                    let t2 = PersonName ((Jackfoxy.PersonalServer.NonEmptyString (nonEmptyString.ToString())), Set.empty)
//                    t =  t2
//
////            testPropertyWithConfig configReplay "FullName.PersonName is equal" <|
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
//            ]