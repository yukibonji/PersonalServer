﻿namespace PersonalServer.Tests

open Jackfoxy.PersonalServer
open FsCheck
open System

module DomainGeneratorsCode =

//https://msdn.microsoft.com/en-us/library/system.char.iswhitespace(v=vs.110).aspx
    let spaceSeparator = [
        '\u0020'
        '\u1680'
        '\u2000'
        '\u2001'
        '\u2002'
        '\u2003'
        '\u2004'
        '\u2005'
        '\u2006'
        '\u2007'
        '\u2008'
        '\u2009'
        '\u200A'
        '\u202F'
        '\u205F'
        '\u3000'
    ]

    let lineSeparator = ['\u2028']

    let paragraphSeparator = ['\u2029']

    let miscWhitespace = [
        '\u0009'
        '\u000A'
        '\u000B'
        '\u000C'
        '\u000D'
        '\u0085'
        '\u00A0'
    ]

    let whiteSpace = 
        List.concat [spaceSeparator; lineSeparator; paragraphSeparator; miscWhitespace]

    let personalServerNonEmptyString() =
        gen {
                let! nonEmptyString = Arb.generate<NonEmptyString>
                return!  
                    [TrimNonEmptyString.TryParse (nonEmptyString.ToString())]
                    |> List.choose id
                    |> Gen.elements
        }

    let nonDigitalString() = 
        gen {  
                let! a = Arb.generate<NonEmptyString> 
                return! Gen.elements [a.ToString()] 
        }
        |> Gen.filter(fun x -> 
                        let (isInt, _) =Int32.TryParse x 
                        not isInt) 

    let whitespaceString() =
        let length = 
            Gen.sample 1 1 <| Gen.choose (1, 30)
            |> List.head
            |> int

        Gen.arrayOfLength length <| Gen.elements whiteSpace
        |> Gen.map (fun x -> new string(x))

    let nonEmptyNonAllWhitespaceString() =
        gen {
            return!
                Arb.generate<NonEmptyString> 
        }
        |> Gen.filter (fun x -> 
            let charA = x.ToString().ToCharArray()
            Array.fold (fun s t -> 
                if List.exists (fun x' -> x' = t) whiteSpace |> not then true
                else s
                    ) false charA )
        |> Gen.map (fun x -> x.ToString())

    let genNonEmptyNonAllWhitespaceStringList() =
        let positiveInt = Arb.generate<PositiveInt> 
        let length = 
            Gen.sample 30 1 positiveInt
            |> List.head
            |> int

        Gen.listOfLength length <| nonEmptyNonAllWhitespaceString()

    let genFullName() =
        gen { 
                let! first = Arb.generate<string option>
                let! middle = Arb.generate<string list> 
                let! family = Arb.generate<string option>

                return
                    FullName.TryParse (first, middle, family, NameOrder.Western, Set.empty<Tag>)
        }
        |> Gen.filter Option.isSome
        |> Gen.map (fun x -> x.Value)

    let genNameAndAffixes() =
        gen { 
                let! salutations = Arb.generate<string list>
                let! personName = Arb.generate<NonEmptyString>
                let! suffixes = Arb.generate<string list>

                return
                    NameAndAffixes.TryParse (salutations, personName.ToString(), suffixes, Set.empty<Tag>)
        }
        |> Gen.filter Option.isSome
        |> Gen.map (fun x -> x.Value)

    let genDigitsInWhiteSpace () =
        gen {
                let! frontWhitespace = whitespaceString()
                let! digits = Arb.generate<NonNegativeInt>
                let! endWhitespace = whitespaceString()
                return sprintf "%s%s%s" frontWhitespace (digits.ToString()) endWhitespace
        }

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

    let genDigitsOfLengthInWhiteSpace length =
        gen {
                let! frontWhitespace = whitespaceString()
                let! digits = Arb.generate<NonNegativeInt>
                let! endWhitespace = whitespaceString()
                return sprintf "%s%s%s" frontWhitespace (validDigits digits length) endWhitespace
        }

    let inputZip5Plus4() =
        gen {
            let! digits = Arb.generate<NonNegativeInt>
            return validDigits digits 9
        }

    let genPhysicalAddress() =
        gen { 
                let! streetAddress = Arb.generate<string list> 
                let! city = Arb.generate<string option>
                let! state = Arb.generate<string option>
                let! postalCodeString = Arb.generate<string>
                let! digits = Arb.generate<NonNegativeInt>
                let postalCodeZip = validDigits digits 5
                let! postalCodeZip5Plus4 = inputZip5Plus4()
                let! country = Arb.generate<string option>

                let! postalCode = Gen.elements [Some postalCodeString; Some postalCodeZip; Some postalCodeZip5Plus4; None]

                return
                    PhysicalAddress.TryParse (streetAddress, city, state, postalCode, country, Set.empty<Tag>)
        }
        |> Gen.filter Option.isSome
        |> Gen.map (fun x -> x.Value)
        
type DomainGenerators =
        static member FullName() =
            {new Arbitrary<FullName>() with
                override __.Generator = 
                    DomainGeneratorsCode.genFullName()
                    }
        static member NameAndAffixes() =
            {new Arbitrary<NameAndAffixes>() with
                override __.Generator = 
                    DomainGeneratorsCode.genNameAndAffixes()
                    }
        static member NonEmptyStringList() =
            {new Arbitrary<string list>() with
                override __.Generator = 
                    DomainGeneratorsCode.genNonEmptyNonAllWhitespaceStringList()
                    }
        static member PhysicalAddress() =
            {new Arbitrary<PhysicalAddress>() with
                override __.Generator = 
                    DomainGeneratorsCode.genPhysicalAddress()
                    }
                    

