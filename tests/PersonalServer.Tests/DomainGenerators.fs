namespace PersonalServer.Tests

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
                let! salutation =  Arb.generate<string list> 
                let! first = Arb.generate<string option>
                let! middle = Arb.generate<string list> 
                let! family = Arb.generate<string option>
                let! suffix = Arb.generate<string list> 

                return
                    FullName.TryParse (salutation, first, middle, family, suffix, NameOrder.Western, Set.empty<Tag>)
        }
        |> Gen.filter Option.isSome

    let genWhiteSpace =
        System.Globalization.UnicodeCategory.SpaceSeparator
        
        
type DomainGenerators =
        static member FullName() =
            {new Arbitrary<FullName option>() with
                override __.Generator = 
                    DomainGeneratorsCode.genFullName()
                     }
        static member NonEmptyStringList() =
            {new Arbitrary<string list>() with
                override __.Generator = DomainGeneratorsCode.genNonEmptyNonAllWhitespaceStringList()}

