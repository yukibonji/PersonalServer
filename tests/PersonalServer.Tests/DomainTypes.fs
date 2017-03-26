namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open Expecto
open FsCheck
open System

module DomainTypes =

    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }
    let configReplay = { Config.Quick with Replay = Some <| Random.StdGen(123, 456) }  //see Tips & Tricks for FsCheck

    let nonDigitalString = 
        gen {  
                let! a = Arb.generate<NonEmptyString> 
                return! Gen.elements [a.ToString()] 
            }
            |> Gen.filter(fun x -> 
                            let (isInt, _) =Int32.TryParse x 
                            not isInt) 

    [<Tests>]
    let testTag =
        testList "DomainTypes.Tag" [

            testCase "Tag throws on empty string" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  Tag System.String.Empty |> ignore)
                                            "Expected argument exception.")

            testCase "Tag throws on null" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  Tag null |> ignore)
                                            "Expected argument exception.")

            testCase "Tag tryParse None on empty string" <| fun () ->
                Expect.isNone (Tag.TryParse System.String.Empty) "Expected None"

            testCase "Tag tryParse None on null" <| fun () ->
                Expect.isNone (Tag.TryParse null) "Expected None"

            testPropertyWithConfig config10k "Tag creation" <|
                fun  (tag : NonEmptyString) ->
                    let t = Tag (tag.ToString())
                    let t' = t.ToString()
                    (tag.ToString()) = t'

            testPropertyWithConfig config10k "Tag TryPrse" <|
                fun  (tag : NonEmptyString) ->
                    let t = Tag.TryParse (tag.ToString())
                    (tag.ToString()) = t.Value.Value

            testPropertyWithConfig config10k "Tag set creation" <|
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

            testPropertyWithConfig config10k "Tag set is unique" <|
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

    [<Tests>]
    let testNonEmptyString =
        testList "DomainTypes.NonEmptyString" [

            testCase "NonEmptyString throws on empty string" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  Jackfoxy.PersonalServer.NonEmptyString System.String.Empty |> ignore)
                                            "Expected argument exception.")

            testCase "NonEmptyString throws on null" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  Jackfoxy.PersonalServer.NonEmptyString null |> ignore)
                                            "Expected argument exception.")

            testCase "NonEmptyString tryParse None on empty string" <| fun () ->
                Expect.isNone (Jackfoxy.PersonalServer.NonEmptyString.TryParse System.String.Empty) "Expected None"

            testCase "NonEmptyString tryParse None on null" <| fun () ->
                Expect.isNone (Jackfoxy.PersonalServer.NonEmptyString.TryParse null) "Expected None"

            testPropertyWithConfig config10k "NonEmptyString creation" <|
                fun  (nonEmptyString : NonEmptyString) ->
                    let t = Jackfoxy.PersonalServer.NonEmptyString (nonEmptyString.ToString())
                    let t' = t.ToString()
                    (nonEmptyString.ToString()) = t'

            testPropertyWithConfig config10k "NonEmptyString TryPrse" <|
                fun  (nonEmptyString : NonEmptyString) ->
                    let t = Jackfoxy.PersonalServer.NonEmptyString.TryParse (nonEmptyString.ToString())
                    (nonEmptyString.ToString()) = t.Value.Value
            ]

    [<Tests>]
    let testNonEmptyStringOption =
        testList "DomainTypes.NonEmptyStringOption" [

            testCase "NonEmptyString None on empty string" <| fun () ->
                Expect.isNone (NonEmptyStringOption System.String.Empty).Value "Expected None"

            testCase "NonEmptyString None on null" <| fun () ->
                Expect.isNone (NonEmptyStringOption null).Value "Expected None"

            testPropertyWithConfig config10k "NonEmptyString creation" <|
                fun  (nonEmptyString : NonEmptyString) ->
                    let t = NonEmptyStringOption (nonEmptyString.ToString())
                    let t' = t.Value
                    (Some (nonEmptyString.ToString())) = t'
            ]

    [<Tests>]
    let testDigitString =
        testList "DomainTypes.DigitString" [

            testCase "DigitString throws on empty string" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  DigitString System.String.Empty |> ignore)
                                            "Expected argument exception.")

            testCase "DigitString throws on null" (fun _ ->
                Expect.throwsT<NullReferenceException> (fun _ ->  DigitString null |> ignore)
                                            "Expected argument exception.")

            testPropertyWithConfig config10k "DigitString throws on non-digital string" <| fun  () ->
                    let nonDigitString() =
                        let x = 
                            Gen.sample 1 1 nonDigitalString
                            |> List.head

                        DigitString x

                    Prop.throws<ArgumentException,_> <| lazy nonDigitString()

            testCase "DigitString tryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "DigitString creation" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString <| digits.ToString()
                    (digits.ToString()) = t.Value

            testPropertyWithConfig config10k "DigitString TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let t = DigitString.TryParse <| digits.ToString()
                    (digits.ToString()) = t.Value.Value

            testPropertyWithConfig config10k "DigitString tryParse non-digital string is None" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen nonDigitalString)
                        (fun (x : string) -> 
                           let t = DigitString.TryParse x
                           t.IsNone)
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

            testCase "DigitString2 throws on empty string" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  DigitString2 System.String.Empty |> ignore)
                                            "Expected argument exception.")

            testCase "DigitString2 throws on null" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  DigitString2 null |> ignore)
                                            "Expected argument exception.")

            testPropertyWithConfig config10k "DigitString2 throws on non-digital string" <| fun  () ->
                    let nonDigitString() =
                        let x = 
                            Gen.sample 1 1 nonDigitalString
                            |> List.head

                        DigitString2 x

                    Prop.throws<ArgumentException,_> <| lazy nonDigitString()

            testPropertyWithConfig config10k "DigitString2 throws on wrong length digital string" <| fun  () ->
                    let nonDigitString() =
                        let x = 
                            Gen.sample 1 1 Arb.generate<NonNegativeInt>
                            |> List.head
                            |> inValid
                        DigitString2 x

                    Prop.throws<ArgumentException,_> <| lazy nonDigitString()

            testCase "DigitString2 tryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString2.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "DigitString2 creation" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString2 validDigit
                    validDigit = t.Value

            testPropertyWithConfig config10k "DigitString2 TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString2.TryParse <| valid digits
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "DigitString2 tryParse non-digital string is None" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen nonDigitalString)
                        (fun (x : string) -> 
                           let t = DigitString2.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "DigitString2 tryParse wrong length digital string is None" <|
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString2.TryParse <| inValid digits
                    t.IsNone
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

            testCase "DigitString3 throws on empty string" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  DigitString3 System.String.Empty |> ignore)
                                            "Expected argument exception.")

            testCase "DigitString3 throws on null" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  DigitString3 null |> ignore)
                                            "Expected argument exception.")

            testPropertyWithConfig config10k "DigitString3 throws on non-digital string" <| fun  () ->
                    let nonDigitString() =
                        let x = 
                            Gen.sample 1 1 nonDigitalString
                            |> List.head

                        DigitString3 x

                    Prop.throws<ArgumentException,_> <| lazy nonDigitString()

            testPropertyWithConfig config10k "DigitString3 throws on wrong length digital string" <| fun  () ->
                    let nonDigitString() =
                        let x = 
                            Gen.sample 1 1 Arb.generate<NonNegativeInt>
                            |> List.head
                            |> inValid
                        DigitString3 x

                    Prop.throws<ArgumentException,_> <| lazy nonDigitString()

            testCase "DigitString3 tryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString3.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "DigitString3 creation" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString3 validDigit
                    validDigit = t.Value

            testPropertyWithConfig config10k "DigitString3 TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString3.TryParse <| valid digits
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "DigitString3 tryParse non-digital string is None" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen nonDigitalString)
                        (fun (x : string) -> 
                           let t = DigitString3.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "DigitString3 tryParse wrong length digital string is None" <|
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString3.TryParse <| inValid digits
                    t.IsNone
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

            testCase "DigitString4 throws on empty string" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  DigitString4 System.String.Empty |> ignore)
                                            "Expected argument exception.")

            testCase "DigitString4 throws on null" (fun _ ->
                Expect.throwsT<ArgumentException> (fun _ ->  DigitString4 null |> ignore)
                                            "Expected argument exception.")

            testPropertyWithConfig config10k "DigitString4 throws on non-digital string" <| fun  () ->
                    let nonDigitString() =
                        let x = 
                            Gen.sample 1 1 nonDigitalString
                            |> List.head

                        DigitString4 x

                    Prop.throws<ArgumentException,_> <| lazy nonDigitString()

            testPropertyWithConfig config10k "DigitString4 throws on wrong length digital string" <| fun  () ->
                    let nonDigitString() =
                        let x = 
                            Gen.sample 1 1 Arb.generate<NonNegativeInt>
                            |> List.head
                            |> inValid
                        DigitString4 x

                    Prop.throws<ArgumentException,_> <| lazy nonDigitString()

            testCase "DigitString4 tryParse None on empty string" <| fun () ->
                Expect.isNone (DigitString4.TryParse System.String.Empty) "Expected None"

            testPropertyWithConfig config10k "DigitString4 creation" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString4 validDigit
                    validDigit = t.Value

            testPropertyWithConfig config10k "DigitString4 TryParse" <|
                fun  (digits : NonNegativeInt) ->
                    let validDigit = valid digits
                    let t = DigitString4.TryParse <| valid digits
                    validDigit = t.Value.Value

            testPropertyWithConfig config10k "DigitString4 tryParse non-digital string is None" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen nonDigitalString)
                        (fun (x : string) -> 
                           let t = DigitString4.TryParse x
                           t.IsNone)
            testPropertyWithConfig config10k "DigitString4 tryParse wrong length digital string is None" <|
                fun  () ->
                    fun  (digits : NonNegativeInt) ->
                    let t = DigitString4.TryParse <| inValid digits
                    t.IsNone
            ]