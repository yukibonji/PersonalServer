namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open Expecto
open FsCheck

module DomainTypes =

// arbitrary = [typeof<DomainGenerators>] breaks visual studio test discovery
    let config10kArb = { FsCheckConfig.defaultConfig with maxTest = 10000 ; arbitrary = [typeof<DomainGenerators>] }
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000 }

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
            
    [<Tests>]
    let testFullName =
        testList "DomainTypes.FullName" [
            testCase "TryParse None on all empty" <| fun () ->
                Expect.isNone (FullName.TryParse (None, [], (Some System.String.Empty), NameOrder.Western, Set.empty<Tag>) ) "Expected None"

// this test requires  arbitrary = [typeof<DomainGenerators>] 
// comment this test and expecto test adapter discovers tests

            testPropertyWithConfig config10kArb "equality" <|
                fun  (fullName : FullName) ->
                    let first = fullName.First |> Option.map (fun x -> x.Value)
                    let middle = fullName.Middle |> List.map (fun x -> x.ToString())
                    let family = fullName.Family |> Option.map (fun x -> x.Value)

                    let t = FullName.TryParse (first, middle, family,  NameOrder.Western, Set.empty<Tag>)
 
                    t.Value = fullName
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
