namespace Jackfoxy.PersonalServer

open System
open System.Text.RegularExpressions
open Utilities

type Tag internal (tag: string) =
    member __.Value = tag
    override __.ToString() = tag
    override __.Equals(yobj) = 
        match yobj with
        |  :? Tag as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash tag
    static member TryParse (tag : string) = verifyTrimNonEmptyString tag Tag
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? Tag as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "Tag" "cannot compare values of different types"

type TrimNonEmptyString internal (value : string) =
    member __.Value = value
    override __.ToString() =  value
    override __.Equals(yobj) = 
        match yobj with
        |  :? TrimNonEmptyString as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse (value : string option) =
        match value with
        | Some x ->
            verifyTrimNonEmptyString x TrimNonEmptyString
        | None -> None
    static member TryParse (value : string) = verifyTrimNonEmptyString value TrimNonEmptyString
    static member Parse (xs : string list) =
        xs
        |> List.choose TrimNonEmptyString.TryParse 
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? TrimNonEmptyString as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "TrimNonEmptyString" "cannot compare values of different types"

type Digits internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? Digits as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse (value : string) = 
        if String.IsNullOrWhiteSpace value then
            None
        else
            let s' = value.Trim()
            if String.length(s') = 0 then 
                None
            else
                let regex = new Regex("^[0-9]+$")

                if regex.IsMatch s' then 
                    Some <| Digits s'
                else 
                    None
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? Digits as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "Digits" "cannot compare values of different types"

type Digits2 internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? Digits2 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse value = verifyStringInt value 2 Digits2
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? Digits2 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "Digits2" "cannot compare values of different types"

type Digits3 internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? Digits3 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse value = verifyStringInt value 3 Digits3
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? Digits3 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "Digits3" "cannot compare values of different types"

type Digits4 internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? Digits4 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse value = verifyStringInt value 4 Digits4
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? Digits4 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "Digits4" "cannot compare values of different types"

type FullName internal (first, middle, family, nameOrder, tags) =
    member __.First : TrimNonEmptyString option = first 
    member __.Middle : TrimNonEmptyString list = middle 
    member __.Family : TrimNonEmptyString option = family 
    member __.NameOrder : NameOrder = nameOrder
    member __.Tags : Tag Set = tags
    member __.SimpleName =
        let combineName nameParts =
            let name =
                nameParts
                |> List.concat
                |> String.concat " "
            SimpleName(name, __.Tags)

        match __.NameOrder with
        | Western -> 
            [Option.toList <| Option.map (fun x -> x.ToString()) __.First;
            __.Middle |> List.map (fun (x : TrimNonEmptyString) -> x.Value);
            Option.toList <| Option.map (fun x -> x.ToString()) __.Family]
            |> combineName
        | FamilyFirst ->
            [Option.toList <| Option.map (fun x -> x.ToString()) __.Family;
            Option.toList <| Option.map (fun x -> x.ToString()) __.First;
            __.Middle |> List.map (fun x -> x.Value)]
            |> combineName
        | Custom f -> f __
    override __.Equals(yobj) = 
        match yobj with
        |  :? FullName as y -> (__.SimpleName = y.SimpleName)
        | _ -> false
    override __.GetHashCode() = hash __
    override __.ToString() = __.SimpleName.ToString()
    static member TryParse ((first : string option), middle, (family : string option), nameOrder, tags) =
        let fi = TrimNonEmptyString.TryParse first
        let m = TrimNonEmptyString.Parse middle
        let fa = TrimNonEmptyString.TryParse family
        match fi, m, fa with
        | Some _, _, _ -> FullName (fi, m, fa, nameOrder, tags) |> Some
        | _, x, _ when x.Length > 0 -> FullName (fi, m, fa, nameOrder, tags) |> Some 
        | _, _, Some _ -> FullName (fi, m, fa, nameOrder, tags) |> Some 
        | _ -> None
    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? FullName as y -> 
                if __.Family > y.Family then 1
                elif __.Family < y.Family then -1
                elif __.First > y.First then 1
                elif __.First < y.First then -1
                elif __.Middle > y.Middle then 1
                elif __.Middle < y.Middle then -1
                else 0
            | _ -> invalidArg "FullName" "cannot compare values of different types"
and NameOrder =
    | Western
    | FamilyFirst
    | Custom of (FullName -> SimpleName)
and SimpleName internal (name: string, tags : Tag Set) = 
    member __.Value = TrimNonEmptyString name
    member __.Tags = tags
    override __.ToString() = name
    override __.Equals(yobj) = 
        match yobj with
        |  :? SimpleName as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (name, tags) =
        if String.IsNullOrWhiteSpace name then
            None        
        else 
            Some <| SimpleName ((name.Trim()), tags)
    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? SimpleName as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "SimpleName" "cannot compare values of different types"
and NameAndAffixes (salutations, simpleName, suffixes) =
    member __.Salutations = salutations 
    member __.SimpleName : SimpleName = simpleName
    member __.Suffixes = suffixes
    member __.Value =
        TrimNonEmptyString <| __.ToString()
    override __.ToString() = 
        [salutations |> List.map (fun (x : TrimNonEmptyString) -> x.Value);
        [simpleName.ToString()];
        suffixes |> List.map (fun (x : TrimNonEmptyString) -> x.Value)]
        |> List.concat
        |> String.concat " "
    override __.Equals(yobj) = 
        match yobj with
        |  :? NameAndAffixes as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse ((salutations : string list), (simpleName : string), (suffixes : string list), tags) =
        match SimpleName.TryParse (simpleName, tags) with
        | Some x -> 
            Some <| NameAndAffixes ((TrimNonEmptyString.Parse salutations), x, (TrimNonEmptyString.Parse suffixes))
        | None -> None

    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? NameAndAffixes as y -> 
                if __.SimpleName > y.SimpleName then 1
                elif __.SimpleName < y.SimpleName then -1
                elif __.Salutations > y.Salutations then 1
                elif __.Salutations < y.Salutations then -1
                elif __.Suffixes > y.Suffixes then 1
                elif __.Suffixes < y.Suffixes then -1
                else 0
            | _ -> invalidArg "NameAndAffixes" "cannot compare values of different types"

type ContactName =
    | SimpleName of SimpleName
    | FullName of FullName
    | NameAndAffixes of NameAndAffixes

type ZipCode5 internal (zip) =
    member __.Value = zip
    override __.ToString() = zip
    override __.Equals(yobj) = 
        match yobj with
        |  :? ZipCode5 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse zip = verifyStringInt zip 5 ZipCode5
    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? ZipCode5 as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "ZipCode5" "cannot compare values of different types"

type ZipCode5Plus4 internal (zip : string) = 
    member __.Value = zip
    member __.ValueFormatted = 
        sprintf "%s-%s" (zip.Substring(0,5)) (zip.Substring(5))
        
    override __.ToString() = zip
    override __.Equals(yobj) = 
        match yobj with
        |  :? ZipCode5Plus4 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (zip : string) =
        if String.IsNullOrWhiteSpace zip then
            None
        else
            let parseFormatted (splitter : char []) =
                let zipParts = zip.Split(splitter, StringSplitOptions.RemoveEmptyEntries)

                if zipParts.Length = 2 then
                    match ZipCode5.TryParse zipParts.[0] with
                    | Some zip5 -> 
                        match  Digits4.TryParse zipParts.[1] with
                        | Some x -> 
                            Some <| ZipCode5Plus4 (sprintf "%s%s" zip5.Value x.Value)
                        | None -> None
                    | None -> None
                else
                    None
            match seq {yield verifyStringInt zip 9 ZipCode5Plus4; yield parseFormatted [|'-'|]; yield parseFormatted [|' '|];}
                    |> Seq.tryFind (fun x -> x.IsSome) with
            | Some x -> x
            | None -> None

    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? ZipCode5Plus4 as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "ZipCode5Plus4" "cannot compare values of different types"

type NonUsPostalCode internal (postalCode) =
    member __.Value = TrimNonEmptyString postalCode
    override __.ToString() = postalCode
    override __.Equals(yobj) = 
        match yobj with
        |  :? NonUsPostalCode as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse postalCode = verifyTrimNonEmptyString postalCode NonUsPostalCode
    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? NonUsPostalCode as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "OtherPostalCode" "cannot compare values of different types"

[<CustomEquality;CustomComparison>]
type ZipCode =
    | ZipCode5 of ZipCode5
    | ZipCode5Plus4 of ZipCode5Plus4
    static member TryParse zipcode =
        match ZipCode5Plus4.TryParse zipcode with
        | Some x -> Some <| ZipCode.ZipCode5Plus4 x
        | None ->
            match ZipCode5.TryParse zipcode with
            | Some x -> Some <| ZipCode.ZipCode5 x
            | None -> None
    override __.ToString() = 
        match __ with
        | ZipCode5 x -> x.ToString()
        | ZipCode5Plus4 x ->  x.ToString()
    override __.Equals(yobj) = 
        yobj.GetType() = __.GetType() && yobj.ToString() = __.ToString()
    override __.GetHashCode() = hash __
    interface IComparable with
        member __.CompareTo yobj =       
            let yString = (unbox yobj).ToString()
            let xString = __.ToString()

            if xString > yString then 1
            elif xString < yString then -1
            else 0

[<CustomEquality;CustomComparison>]
type PostalCode =
    | ZipCode of ZipCode
    | NonUsPostalCode of NonUsPostalCode
    static member TryParse postalCode =
        match ZipCode.TryParse postalCode with
        | Some x -> Some <| PostalCode.ZipCode  x
        | None -> 
            match NonUsPostalCode.TryParse postalCode with
            | Some x -> Some (PostalCode.NonUsPostalCode  x)
            | None -> None
    override __.ToString() = 
        match __ with
        | ZipCode x -> x.ToString()
        | NonUsPostalCode x ->  x.ToString()
    override __.Equals(yobj) = 
        yobj.GetType() = __.GetType() && yobj.ToString() = __.ToString()
    override __.GetHashCode() = hash __
    interface IComparable with
        member __.CompareTo yobj =
            let yString = (unbox yobj).ToString()
            let xString = __.ToString()

            if xString > yString then 1
            elif xString <yString then -1
            else 0

type PhysicalAddress internal (streetAddress, city, state, postalCode, country, tags) =
    member __.StreetAddress : TrimNonEmptyString list = streetAddress 
    member __.City : TrimNonEmptyString option = city 
    member __.State : TrimNonEmptyString option = state 
    member __.PostalCode : PostalCode option = postalCode
    member __.Country : TrimNonEmptyString option = country
    member __.Tags : Tag Set = tags
    override __.Equals(yobj) = 
        match yobj with
        |  :? PhysicalAddress as y -> 
            __.Country = y.Country
            && __.State = y.State
            && __.City = y.City
            && __.PostalCode = y.PostalCode
            && __.StreetAddress = y.StreetAddress
        | _ -> false
    override __.GetHashCode() = hash __
    override __.ToString() = 
        sprintf "%A %A %A %A %A" __.StreetAddress __.City __.State __.PostalCode __.Country
    static member TryParse ((streetAddress : string list), (city : string option), (state : string option), (postalCode : string option), (country : string option), tags) =
        let sa = TrimNonEmptyString.Parse streetAddress
        let cy = TrimNonEmptyString.TryParse city
        let s = TrimNonEmptyString.TryParse state
        let p = 
            match postalCode with
            | Some x ->  PostalCode.TryParse x
            | None -> None 
        let c = TrimNonEmptyString.TryParse country
        match sa, cy, s, p, c with
        | x, _, _, _, _ when x.Length > 0 -> PhysicalAddress (sa, cy, s, p, c, tags) |> Some 
        | _, Some _, _, _, _ -> PhysicalAddress (sa, cy, s, p, c, tags) |> Some
        | _, _, Some _, _, _ -> PhysicalAddress (sa, cy, s, p, c, tags) |> Some
        | _, _, _, Some _, _ -> PhysicalAddress (sa, cy, s, p, c, tags) |> Some
        | _, _, _, _, Some _ -> PhysicalAddress (sa, cy, s, p, c, tags) |> Some
        | _ -> None
    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? PhysicalAddress as y -> 
                if __.Country > y.Country then 1
                elif __.Country < y.Country then -1
                elif __.State > y.State then 1
                elif __.State < y.State then -1
                elif __.City > y.City then 1
                elif __.City < y.City then -1
                elif __.PostalCode > y.PostalCode then 1
                elif __.PostalCode < y.PostalCode then -1
                elif __.StreetAddress > y.StreetAddress then 1
                elif __.StreetAddress < y.StreetAddress then -1
                else 0
            | _ -> invalidArg "PhysicalAddress" "cannot compare values of different types"

type EmailAddress internal (email : string, tags : Tag Set) =
    member __.Value = email
    member __.Tags = tags
    override __.ToString() = email
    override __.Equals(yobj) = 
        match yobj with
        |  :? EmailAddress as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (email, tags) = 
        match verifyTrimNonEmptyString email id with
        | Some x ->
            if x.StartsWith("@") || x.EndsWith("@") || x.EndsWith(".") then
                None
            else
                if x.Contains("@") then
                    Some <| EmailAddress (x, tags)
                else
                    None
        | None ->
            None

    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? EmailAddress as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "EmailAddress" "cannot compare values of different types"

type UpperLatin2 internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? UpperLatin2 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse value = 
        verifyUpperLatinString value 2 UpperLatin2
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? UpperLatin2 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "UpperLatin2" "cannot compare values of different types"

type UpperLatin3 internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? UpperLatin2 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse value = 
        verifyUpperLatinString value 3 UpperLatin3
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? UpperLatin3 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "UpperLatin3" "cannot compare values of different types"

type Country =
    {
    Name        : string
    ISO         : UpperLatin2
    UnAlpha     : UpperLatin3
    UnNum       : UInt16
    CallingCodes  : Set<UInt16>
    }
    static member private Create (name, iso, unAlpha, unNum, callingCodes) =
        {
        Name        = name
        ISO         = iso
        UnAlpha     = unAlpha
        UnNum       = unNum
        CallingCodes  = callingCodes
        }

type State =
    {
    Name : string
    Abbreviation : string
    }

module CallingCodes =
    [<Literal>]
    let NorthAmerica = 1us

module Countries =
    //to do: load raw from resource file and build set with Create
    // also Wikipedia calling code page has a few "country" calling codes not covered here
    let countries = 
        [
        {Name = "Afghanistan"; ISO = UpperLatin2.TryParse("AF").Value; UnAlpha = UpperLatin3.TryParse("AFG").Value; UnNum = 4us; CallingCodes = [93us] |> Set.ofList}
        {Name = "Albania"; ISO = UpperLatin2.TryParse("AL").Value; UnAlpha = UpperLatin3.TryParse("ALB").Value; UnNum = 8us; CallingCodes = [355us] |> Set.ofList}
        {Name = "Algeria"; ISO = UpperLatin2.TryParse("DZ").Value; UnAlpha = UpperLatin3.TryParse("DZA").Value; UnNum = 12us; CallingCodes = [213us] |> Set.ofList}
        {Name = "American Samoa"; ISO = UpperLatin2.TryParse("AS").Value; UnAlpha = UpperLatin3.TryParse("ASM").Value; UnNum = 16us; CallingCodes = [1684us] |> Set.ofList}
        {Name = "Andorra"; ISO = UpperLatin2.TryParse("AD").Value; UnAlpha = UpperLatin3.TryParse("AND").Value; UnNum = 20us; CallingCodes = [376us] |> Set.ofList}
        {Name = "Angola"; ISO = UpperLatin2.TryParse("AO").Value; UnAlpha = UpperLatin3.TryParse("AGO").Value; UnNum = 24us; CallingCodes = [244us] |> Set.ofList}
        {Name = "Anguilla"; ISO = UpperLatin2.TryParse("AI").Value; UnAlpha = UpperLatin3.TryParse("AIA").Value; UnNum = 660us; CallingCodes = [1264us] |> Set.ofList}
        {Name = "Antarctica"; ISO = UpperLatin2.TryParse("AQ").Value; UnAlpha = UpperLatin3.TryParse("ATA").Value; UnNum = 10us; CallingCodes = [672us] |> Set.ofList}
        {Name = "Antigua and Barbuda"; ISO = UpperLatin2.TryParse("AG").Value; UnAlpha = UpperLatin3.TryParse("ATG").Value; UnNum = 28us; CallingCodes = [1268us] |> Set.ofList}
        {Name = "Argentina"; ISO = UpperLatin2.TryParse("AR").Value; UnAlpha = UpperLatin3.TryParse("ARG").Value; UnNum = 32us; CallingCodes = [54us] |> Set.ofList}
        {Name = "Armenia"; ISO = UpperLatin2.TryParse("AM").Value; UnAlpha = UpperLatin3.TryParse("ARM").Value; UnNum = 51us; CallingCodes = [374us] |> Set.ofList}
        {Name = "Aruba"; ISO = UpperLatin2.TryParse("AW").Value; UnAlpha = UpperLatin3.TryParse("ABW").Value; UnNum = 533us; CallingCodes = [297us] |> Set.ofList}
        {Name = "Australia"; ISO = UpperLatin2.TryParse("AU").Value; UnAlpha = UpperLatin3.TryParse("AUS").Value; UnNum = 36us; CallingCodes = [61us] |> Set.ofList}
        {Name = "Austria"; ISO = UpperLatin2.TryParse("AT").Value; UnAlpha = UpperLatin3.TryParse("AUT").Value; UnNum = 40us; CallingCodes = [43us] |> Set.ofList}
        {Name = "Azerbaijan"; ISO = UpperLatin2.TryParse("AZ").Value; UnAlpha = UpperLatin3.TryParse("AZE").Value; UnNum = 31us; CallingCodes = [994us] |> Set.ofList}
        {Name = "Bahamas"; ISO = UpperLatin2.TryParse("BS").Value; UnAlpha = UpperLatin3.TryParse("BHS").Value; UnNum = 44us; CallingCodes = [1242us] |> Set.ofList}
        {Name = "Bahrain"; ISO = UpperLatin2.TryParse("BH").Value; UnAlpha = UpperLatin3.TryParse("BHR").Value; UnNum = 48us; CallingCodes = [973us] |> Set.ofList}
        {Name = "Bangladesh"; ISO = UpperLatin2.TryParse("BD").Value; UnAlpha = UpperLatin3.TryParse("BGD").Value; UnNum = 50us; CallingCodes = [880us] |> Set.ofList}
        {Name = "Barbados"; ISO = UpperLatin2.TryParse("BB").Value; UnAlpha = UpperLatin3.TryParse("BRB").Value; UnNum = 52us; CallingCodes = [1246us] |> Set.ofList}
        {Name = "Belarus"; ISO = UpperLatin2.TryParse("BY").Value; UnAlpha = UpperLatin3.TryParse("BLR").Value; UnNum = 112us; CallingCodes = [375us] |> Set.ofList}
        {Name = "Belgium"; ISO = UpperLatin2.TryParse("BE").Value; UnAlpha = UpperLatin3.TryParse("BEL").Value; UnNum = 56us; CallingCodes = [32us] |> Set.ofList}
        {Name = "Belize"; ISO = UpperLatin2.TryParse("BZ").Value; UnAlpha = UpperLatin3.TryParse("BLZ").Value; UnNum = 84us; CallingCodes = [501us] |> Set.ofList}
        {Name = "Benin"; ISO = UpperLatin2.TryParse("BJ").Value; UnAlpha = UpperLatin3.TryParse("BEN").Value; UnNum = 204us; CallingCodes = [229us] |> Set.ofList}
        {Name = "Bermuda"; ISO = UpperLatin2.TryParse("BM").Value; UnAlpha = UpperLatin3.TryParse("BMU").Value; UnNum = 60us; CallingCodes = [1441us] |> Set.ofList}
        {Name = "Bhutan"; ISO = UpperLatin2.TryParse("BT").Value; UnAlpha = UpperLatin3.TryParse("BTN").Value; UnNum = 64us; CallingCodes = [975us] |> Set.ofList}
        {Name = "Bolivia"; ISO = UpperLatin2.TryParse("BO").Value; UnAlpha = UpperLatin3.TryParse("BOL").Value; UnNum = 68us; CallingCodes = [591us] |> Set.ofList}
        {Name = "Bonaire"; ISO = UpperLatin2.TryParse("BQ").Value; UnAlpha = UpperLatin3.TryParse("BES").Value; UnNum = 535us; CallingCodes = [599us] |> Set.ofList}
        {Name = "Bosnia and Herzegovina"; ISO = UpperLatin2.TryParse("BA").Value; UnAlpha = UpperLatin3.TryParse("BIH").Value; UnNum = 70us; CallingCodes = [387us] |> Set.ofList}
        {Name = "Botswana"; ISO = UpperLatin2.TryParse("BW").Value; UnAlpha = UpperLatin3.TryParse("BWA").Value; UnNum = 72us; CallingCodes = [267us] |> Set.ofList}
        {Name = "Bouvet Island"; ISO = UpperLatin2.TryParse("BV").Value; UnAlpha = UpperLatin3.TryParse("BVT").Value; UnNum = 74us; CallingCodes = [47us] |> Set.ofList}
        {Name = "Brazil"; ISO = UpperLatin2.TryParse("BR").Value; UnAlpha = UpperLatin3.TryParse("BRA").Value; UnNum = 76us; CallingCodes = [55us] |> Set.ofList}
        {Name = "British Indian Ocean Territory"; ISO = UpperLatin2.TryParse("IO").Value; UnAlpha = UpperLatin3.TryParse("IOT").Value; UnNum = 86us; CallingCodes = [246us] |> Set.ofList}
        {Name = "Brunei Darussalam"; ISO = UpperLatin2.TryParse("BN").Value; UnAlpha = UpperLatin3.TryParse("BRN").Value; UnNum = 96us; CallingCodes = [673us] |> Set.ofList}
        {Name = "Bulgaria"; ISO = UpperLatin2.TryParse("BG").Value; UnAlpha = UpperLatin3.TryParse("BGR").Value; UnNum = 100us; CallingCodes = [359us] |> Set.ofList}
        {Name = "Burkina Faso"; ISO = UpperLatin2.TryParse("BF").Value; UnAlpha = UpperLatin3.TryParse("BFA").Value; UnNum = 854us; CallingCodes = [226us] |> Set.ofList}
        {Name = "Burundi"; ISO = UpperLatin2.TryParse("BI").Value; UnAlpha = UpperLatin3.TryParse("BDI").Value; UnNum = 108us; CallingCodes = [257us] |> Set.ofList}
        {Name = "Cambodia"; ISO = UpperLatin2.TryParse("KH").Value; UnAlpha = UpperLatin3.TryParse("KHM").Value; UnNum = 116us; CallingCodes = [855us] |> Set.ofList}
        {Name = "Cameroon"; ISO = UpperLatin2.TryParse("CM").Value; UnAlpha = UpperLatin3.TryParse("CMR").Value; UnNum = 120us; CallingCodes = [237us] |> Set.ofList}
        {Name = "Canada"; ISO = UpperLatin2.TryParse("CA").Value; UnAlpha = UpperLatin3.TryParse("CAN").Value; UnNum = 124us; CallingCodes = [1us] |> Set.ofList}
        {Name = "Cape Verde"; ISO = UpperLatin2.TryParse("CV").Value; UnAlpha = UpperLatin3.TryParse("CPV").Value; UnNum = 132us; CallingCodes = [238us] |> Set.ofList}
        {Name = "Cayman Islands"; ISO = UpperLatin2.TryParse("KY").Value; UnAlpha = UpperLatin3.TryParse("CYM").Value; UnNum = 136us; CallingCodes = [1345us] |> Set.ofList}
        {Name = "Central African Republic"; ISO = UpperLatin2.TryParse("CF").Value; UnAlpha = UpperLatin3.TryParse("CAF").Value; UnNum = 140us; CallingCodes = [236us] |> Set.ofList}
        {Name = "Chad"; ISO = UpperLatin2.TryParse("TD").Value; UnAlpha = UpperLatin3.TryParse("TCD").Value; UnNum = 148us; CallingCodes = [235us] |> Set.ofList}
        {Name = "Chile"; ISO = UpperLatin2.TryParse("CL").Value; UnAlpha = UpperLatin3.TryParse("CHL").Value; UnNum = 152us; CallingCodes = [56us] |> Set.ofList}
        {Name = "China"; ISO = UpperLatin2.TryParse("CN").Value; UnAlpha = UpperLatin3.TryParse("CHN").Value; UnNum = 156us; CallingCodes = [86us] |> Set.ofList}
        {Name = "Christmas Island"; ISO = UpperLatin2.TryParse("CX").Value; UnAlpha = UpperLatin3.TryParse("CXR").Value; UnNum = 162us; CallingCodes = [61us] |> Set.ofList}
        {Name = "Cocos (Keeling) Islands"; ISO = UpperLatin2.TryParse("CC").Value; UnAlpha = UpperLatin3.TryParse("CCK").Value; UnNum = 166us; CallingCodes = [61us] |> Set.ofList}
        {Name = "Colombia"; ISO = UpperLatin2.TryParse("CO").Value; UnAlpha = UpperLatin3.TryParse("COL").Value; UnNum = 170us; CallingCodes = [57us] |> Set.ofList}
        {Name = "Comoros"; ISO = UpperLatin2.TryParse("KM").Value; UnAlpha = UpperLatin3.TryParse("COM").Value; UnNum = 174us; CallingCodes = [269us] |> Set.ofList}
        {Name = "Congo"; ISO = UpperLatin2.TryParse("CG").Value; UnAlpha = UpperLatin3.TryParse("COG").Value; UnNum = 178us; CallingCodes = [242us] |> Set.ofList}
        {Name = "Democratic Republic of the Congo"; ISO = UpperLatin2.TryParse("CD").Value; UnAlpha = UpperLatin3.TryParse("COD").Value; UnNum = 180us; CallingCodes = [243us] |> Set.ofList}
        {Name = "Cook Islands"; ISO = UpperLatin2.TryParse("CK").Value; UnAlpha = UpperLatin3.TryParse("COK").Value; UnNum = 184us; CallingCodes = [682us] |> Set.ofList}
        {Name = "Costa Rica"; ISO = UpperLatin2.TryParse("CR").Value; UnAlpha = UpperLatin3.TryParse("CRI").Value; UnNum = 188us; CallingCodes = [506us] |> Set.ofList}
        {Name = "Croatia"; ISO = UpperLatin2.TryParse("HR").Value; UnAlpha = UpperLatin3.TryParse("HRV").Value; UnNum = 191us; CallingCodes = [385us] |> Set.ofList}
        {Name = "Cuba"; ISO = UpperLatin2.TryParse("CU").Value; UnAlpha = UpperLatin3.TryParse("CUB").Value; UnNum = 192us; CallingCodes = [53us] |> Set.ofList}
        {Name = "CuraÃ§ao"; ISO = UpperLatin2.TryParse("CW").Value; UnAlpha = UpperLatin3.TryParse("CUW").Value; UnNum = 531us; CallingCodes = [599us] |> Set.ofList}
        {Name = "Cyprus"; ISO = UpperLatin2.TryParse("CY").Value; UnAlpha = UpperLatin3.TryParse("CYP").Value; UnNum = 196us; CallingCodes = [357us] |> Set.ofList}
        {Name = "Czech Republic"; ISO = UpperLatin2.TryParse("CZ").Value; UnAlpha = UpperLatin3.TryParse("CZE").Value; UnNum = 203us; CallingCodes = [420us] |> Set.ofList}
        {Name = "CÃ´te d'Ivoire"; ISO = UpperLatin2.TryParse("CI").Value; UnAlpha = UpperLatin3.TryParse("CIV").Value; UnNum = 384us; CallingCodes = [225us] |> Set.ofList}
        {Name = "Denmark"; ISO = UpperLatin2.TryParse("DK").Value; UnAlpha = UpperLatin3.TryParse("DNK").Value; UnNum = 208us; CallingCodes = [45us] |> Set.ofList}
        {Name = "Djibouti"; ISO = UpperLatin2.TryParse("DJ").Value; UnAlpha = UpperLatin3.TryParse("DJI").Value; UnNum = 262us; CallingCodes = [253us] |> Set.ofList}
        {Name = "Dominica"; ISO = UpperLatin2.TryParse("DM").Value; UnAlpha = UpperLatin3.TryParse("DMA").Value; UnNum = 212us; CallingCodes = [1767us] |> Set.ofList}
        {Name = "Dominican Republic"; ISO = UpperLatin2.TryParse("DO").Value; UnAlpha = UpperLatin3.TryParse("DOM").Value; UnNum = 214us; CallingCodes = [1809us; 1829us; 1849us] |> Set.ofList}
        {Name = "Ecuador"; ISO = UpperLatin2.TryParse("EC").Value; UnAlpha = UpperLatin3.TryParse("ECU").Value; UnNum = 218us; CallingCodes = [593us] |> Set.ofList}
        {Name = "Egypt"; ISO = UpperLatin2.TryParse("EG").Value; UnAlpha = UpperLatin3.TryParse("EGY").Value; UnNum = 818us; CallingCodes = [20us] |> Set.ofList}
        {Name = "El Salvador"; ISO = UpperLatin2.TryParse("SV").Value; UnAlpha = UpperLatin3.TryParse("SLV").Value; UnNum = 222us; CallingCodes = [503us] |> Set.ofList}
        {Name = "Equatorial Guinea"; ISO = UpperLatin2.TryParse("GQ").Value; UnAlpha = UpperLatin3.TryParse("GNQ").Value; UnNum = 226us; CallingCodes = [240us] |> Set.ofList}
        {Name = "Eritrea"; ISO = UpperLatin2.TryParse("ER").Value; UnAlpha = UpperLatin3.TryParse("ERI").Value; UnNum = 232us; CallingCodes = [291us] |> Set.ofList}
        {Name = "Estonia"; ISO = UpperLatin2.TryParse("EE").Value; UnAlpha = UpperLatin3.TryParse("EST").Value; UnNum = 233us; CallingCodes = [372us] |> Set.ofList}
        {Name = "Ethiopia"; ISO = UpperLatin2.TryParse("ET").Value; UnAlpha = UpperLatin3.TryParse("ETH").Value; UnNum = 231us; CallingCodes = [251us] |> Set.ofList}
        {Name = "Falkland Islands (Malvinas)"; ISO = UpperLatin2.TryParse("FK").Value; UnAlpha = UpperLatin3.TryParse("FLK").Value; UnNum = 238us; CallingCodes = [500us] |> Set.ofList}
        {Name = "Faroe Islands"; ISO = UpperLatin2.TryParse("FO").Value; UnAlpha = UpperLatin3.TryParse("FRO").Value; UnNum = 234us; CallingCodes = [298us] |> Set.ofList}
        {Name = "Fiji"; ISO = UpperLatin2.TryParse("FJ").Value; UnAlpha = UpperLatin3.TryParse("FJI").Value; UnNum = 242us; CallingCodes = [679us] |> Set.ofList}
        {Name = "Finland"; ISO = UpperLatin2.TryParse("FI").Value; UnAlpha = UpperLatin3.TryParse("FIN").Value; UnNum = 246us; CallingCodes = [358us] |> Set.ofList}
        {Name = "France"; ISO = UpperLatin2.TryParse("FR").Value; UnAlpha = UpperLatin3.TryParse("FRA").Value; UnNum = 250us; CallingCodes = [33us] |> Set.ofList}
        {Name = "French Guiana"; ISO = UpperLatin2.TryParse("GF").Value; UnAlpha = UpperLatin3.TryParse("GUF").Value; UnNum = 254us; CallingCodes = [594us] |> Set.ofList}
        {Name = "French Polynesia"; ISO = UpperLatin2.TryParse("PF").Value; UnAlpha = UpperLatin3.TryParse("PYF").Value; UnNum = 258us; CallingCodes = [689us] |> Set.ofList}
        {Name = "French Southern Territories"; ISO = UpperLatin2.TryParse("TF").Value; UnAlpha = UpperLatin3.TryParse("ATF").Value; UnNum = 260us; CallingCodes = [262us] |> Set.ofList}
        {Name = "Gabon"; ISO = UpperLatin2.TryParse("GA").Value; UnAlpha = UpperLatin3.TryParse("GAB").Value; UnNum = 266us; CallingCodes = [241us] |> Set.ofList}
        {Name = "Gambia"; ISO = UpperLatin2.TryParse("GM").Value; UnAlpha = UpperLatin3.TryParse("GMB").Value; UnNum = 270us; CallingCodes = [220us] |> Set.ofList}
        {Name = "Georgia"; ISO = UpperLatin2.TryParse("GE").Value; UnAlpha = UpperLatin3.TryParse("GEO").Value; UnNum = 268us; CallingCodes = [995us] |> Set.ofList}
        {Name = "Germany"; ISO = UpperLatin2.TryParse("DE").Value; UnAlpha = UpperLatin3.TryParse("DEU").Value; UnNum = 276us; CallingCodes = [49us] |> Set.ofList}
        {Name = "Ghana"; ISO = UpperLatin2.TryParse("GH").Value; UnAlpha = UpperLatin3.TryParse("GHA").Value; UnNum = 288us; CallingCodes = [233us] |> Set.ofList}
        {Name = "Gibraltar"; ISO = UpperLatin2.TryParse("GI").Value; UnAlpha = UpperLatin3.TryParse("GIB").Value; UnNum = 292us; CallingCodes = [350us] |> Set.ofList}
        {Name = "Greece"; ISO = UpperLatin2.TryParse("GR").Value; UnAlpha = UpperLatin3.TryParse("GRC").Value; UnNum = 300us; CallingCodes = [30us] |> Set.ofList}
        {Name = "Greenland"; ISO = UpperLatin2.TryParse("GL").Value; UnAlpha = UpperLatin3.TryParse("GRL").Value; UnNum = 304us; CallingCodes = [299us] |> Set.ofList}
        {Name = "Grenada"; ISO = UpperLatin2.TryParse("GD").Value; UnAlpha = UpperLatin3.TryParse("GRD").Value; UnNum = 308us; CallingCodes = [1473us] |> Set.ofList}
        {Name = "Guadeloupe"; ISO = UpperLatin2.TryParse("GP").Value; UnAlpha = UpperLatin3.TryParse("GLP").Value; UnNum = 312us; CallingCodes = [590us] |> Set.ofList}
        {Name = "Guam"; ISO = UpperLatin2.TryParse("GU").Value; UnAlpha = UpperLatin3.TryParse("GUM").Value; UnNum = 316us; CallingCodes = [1671us] |> Set.ofList}
        {Name = "Guatemala"; ISO = UpperLatin2.TryParse("GT").Value; UnAlpha = UpperLatin3.TryParse("GTM").Value; UnNum = 320us; CallingCodes = [502us] |> Set.ofList}
        {Name = "Guernsey"; ISO = UpperLatin2.TryParse("GG").Value; UnAlpha = UpperLatin3.TryParse("GGY").Value; UnNum = 831us; CallingCodes = [44us] |> Set.ofList}
        {Name = "Guinea"; ISO = UpperLatin2.TryParse("GN").Value; UnAlpha = UpperLatin3.TryParse("GIN").Value; UnNum = 324us; CallingCodes = [224us] |> Set.ofList}
        {Name = "Guinea-Bissau"; ISO = UpperLatin2.TryParse("GW").Value; UnAlpha = UpperLatin3.TryParse("GNB").Value; UnNum = 624us; CallingCodes = [245us] |> Set.ofList}
        {Name = "Guyana"; ISO = UpperLatin2.TryParse("GY").Value; UnAlpha = UpperLatin3.TryParse("GUY").Value; UnNum = 328us; CallingCodes = [592us] |> Set.ofList}
        {Name = "Haiti"; ISO = UpperLatin2.TryParse("HT").Value; UnAlpha = UpperLatin3.TryParse("HTI").Value; UnNum = 332us; CallingCodes = [509us] |> Set.ofList}
        {Name = "Heard Island and McDonald Mcdonald Islands"; ISO = UpperLatin2.TryParse("HM").Value; UnAlpha = UpperLatin3.TryParse("HMD").Value; UnNum = 334us; CallingCodes = [672us] |> Set.ofList}
        {Name = "Holy See (Vatican City State)"; ISO = UpperLatin2.TryParse("VA").Value; UnAlpha = UpperLatin3.TryParse("VAT").Value; UnNum = 336us; CallingCodes = [379us] |> Set.ofList}
        {Name = "Honduras"; ISO = UpperLatin2.TryParse("HN").Value; UnAlpha = UpperLatin3.TryParse("HND").Value; UnNum = 340us; CallingCodes = [504us] |> Set.ofList}
        {Name = "Hong Kong"; ISO = UpperLatin2.TryParse("HK").Value; UnAlpha = UpperLatin3.TryParse("HKG").Value; UnNum = 344us; CallingCodes = [852us] |> Set.ofList}
        {Name = "Hungary"; ISO = UpperLatin2.TryParse("HU").Value; UnAlpha = UpperLatin3.TryParse("HUN").Value; UnNum = 348us; CallingCodes = [36us] |> Set.ofList}
        {Name = "Iceland"; ISO = UpperLatin2.TryParse("IS").Value; UnAlpha = UpperLatin3.TryParse("ISL").Value; UnNum = 352us; CallingCodes = [354us] |> Set.ofList}
        {Name = "India"; ISO = UpperLatin2.TryParse("IN").Value; UnAlpha = UpperLatin3.TryParse("IND").Value; UnNum = 356us; CallingCodes = [91us] |> Set.ofList}
        {Name = "Indonesia"; ISO = UpperLatin2.TryParse("ID").Value; UnAlpha = UpperLatin3.TryParse("IDN").Value; UnNum = 360us; CallingCodes = [62us] |> Set.ofList}
        {Name = "Iran, Islamic Republic of"; ISO = UpperLatin2.TryParse("IR").Value; UnAlpha = UpperLatin3.TryParse("IRN").Value; UnNum = 364us; CallingCodes = [98us] |> Set.ofList}
        {Name = "Iraq"; ISO = UpperLatin2.TryParse("IQ").Value; UnAlpha = UpperLatin3.TryParse("IRQ").Value; UnNum = 368us; CallingCodes = [964us] |> Set.ofList}
        {Name = "Ireland"; ISO = UpperLatin2.TryParse("IE").Value; UnAlpha = UpperLatin3.TryParse("IRL").Value; UnNum = 372us; CallingCodes = [353us] |> Set.ofList}
        {Name = "Isle of Man"; ISO = UpperLatin2.TryParse("IM").Value; UnAlpha = UpperLatin3.TryParse("IMN").Value; UnNum = 833us; CallingCodes = [44us] |> Set.ofList}
        {Name = "Israel"; ISO = UpperLatin2.TryParse("IL").Value; UnAlpha = UpperLatin3.TryParse("ISR").Value; UnNum = 376us; CallingCodes = [972us] |> Set.ofList}
        {Name = "Italy"; ISO = UpperLatin2.TryParse("IT").Value; UnAlpha = UpperLatin3.TryParse("ITA").Value; UnNum = 380us; CallingCodes = [39us] |> Set.ofList}
        {Name = "Jamaica"; ISO = UpperLatin2.TryParse("JM").Value; UnAlpha = UpperLatin3.TryParse("JAM").Value; UnNum = 388us; CallingCodes = [1876us] |> Set.ofList}
        {Name = "Japan"; ISO = UpperLatin2.TryParse("JP").Value; UnAlpha = UpperLatin3.TryParse("JPN").Value; UnNum = 392us; CallingCodes = [81us] |> Set.ofList}
        {Name = "Jersey"; ISO = UpperLatin2.TryParse("JE").Value; UnAlpha = UpperLatin3.TryParse("JEY").Value; UnNum = 832us; CallingCodes = [44us] |> Set.ofList}
        {Name = "Jordan"; ISO = UpperLatin2.TryParse("JO").Value; UnAlpha = UpperLatin3.TryParse("JOR").Value; UnNum = 400us; CallingCodes = [962us] |> Set.ofList}
        {Name = "Kazakhstan"; ISO = UpperLatin2.TryParse("KZ").Value; UnAlpha = UpperLatin3.TryParse("KAZ").Value; UnNum = 398us; CallingCodes = [7us] |> Set.ofList}
        {Name = "Kenya"; ISO = UpperLatin2.TryParse("KE").Value; UnAlpha = UpperLatin3.TryParse("KEN").Value; UnNum = 404us; CallingCodes = [254us] |> Set.ofList}
        {Name = "Kiribati"; ISO = UpperLatin2.TryParse("KI").Value; UnAlpha = UpperLatin3.TryParse("KIR").Value; UnNum = 296us; CallingCodes = [686us] |> Set.ofList}
        {Name = "Korea, Democratic People's Republic of"; ISO = UpperLatin2.TryParse("KP").Value; UnAlpha = UpperLatin3.TryParse("PRK").Value; UnNum = 408us; CallingCodes = [850us] |> Set.ofList}
        {Name = "Korea, Republic of"; ISO = UpperLatin2.TryParse("KR").Value; UnAlpha = UpperLatin3.TryParse("KOR").Value; UnNum = 410us; CallingCodes = [82us] |> Set.ofList}
        {Name = "Kuwait"; ISO = UpperLatin2.TryParse("KW").Value; UnAlpha = UpperLatin3.TryParse("KWT").Value; UnNum = 414us; CallingCodes = [965us] |> Set.ofList}
        {Name = "Kyrgyzstan"; ISO = UpperLatin2.TryParse("KG").Value; UnAlpha = UpperLatin3.TryParse("KGZ").Value; UnNum = 417us; CallingCodes = [996us] |> Set.ofList}
        {Name = "Lao People's Democratic Republic"; ISO = UpperLatin2.TryParse("LA").Value; UnAlpha = UpperLatin3.TryParse("LAO").Value; UnNum = 418us; CallingCodes = [856us] |> Set.ofList}
        {Name = "Latvia"; ISO = UpperLatin2.TryParse("LV").Value; UnAlpha = UpperLatin3.TryParse("LVA").Value; UnNum = 428us; CallingCodes = [371us] |> Set.ofList}
        {Name = "Lebanon"; ISO = UpperLatin2.TryParse("LB").Value; UnAlpha = UpperLatin3.TryParse("LBN").Value; UnNum = 422us; CallingCodes = [961us] |> Set.ofList}
        {Name = "Lesotho"; ISO = UpperLatin2.TryParse("LS").Value; UnAlpha = UpperLatin3.TryParse("LSO").Value; UnNum = 426us; CallingCodes = [266us] |> Set.ofList}
        {Name = "Liberia"; ISO = UpperLatin2.TryParse("LR").Value; UnAlpha = UpperLatin3.TryParse("LBR").Value; UnNum = 430us; CallingCodes = [231us] |> Set.ofList}
        {Name = "Libya"; ISO = UpperLatin2.TryParse("LY").Value; UnAlpha = UpperLatin3.TryParse("LBY").Value; UnNum = 434us; CallingCodes = [218us] |> Set.ofList}
        {Name = "Liechtenstein"; ISO = UpperLatin2.TryParse("LI").Value; UnAlpha = UpperLatin3.TryParse("LIE").Value; UnNum = 438us; CallingCodes = [423us] |> Set.ofList}
        {Name = "Lithuania"; ISO = UpperLatin2.TryParse("LT").Value; UnAlpha = UpperLatin3.TryParse("LTU").Value; UnNum = 440us; CallingCodes = [370us] |> Set.ofList}
        {Name = "Luxembourg"; ISO = UpperLatin2.TryParse("LU").Value; UnAlpha = UpperLatin3.TryParse("LUX").Value; UnNum = 442us; CallingCodes = [352us] |> Set.ofList}
        {Name = "Macao"; ISO = UpperLatin2.TryParse("MO").Value; UnAlpha = UpperLatin3.TryParse("MAC").Value; UnNum = 446us; CallingCodes = [853us] |> Set.ofList}
        {Name = "Macedonia, the Former Yugoslav Republic of"; ISO = UpperLatin2.TryParse("MK").Value; UnAlpha = UpperLatin3.TryParse("MKD").Value; UnNum = 807us; CallingCodes = [389us] |> Set.ofList}
        {Name = "Madagascar"; ISO = UpperLatin2.TryParse("MG").Value; UnAlpha = UpperLatin3.TryParse("MDG").Value; UnNum = 450us; CallingCodes = [261us] |> Set.ofList}
        {Name = "Malawi"; ISO = UpperLatin2.TryParse("MW").Value; UnAlpha = UpperLatin3.TryParse("MWI").Value; UnNum = 454us; CallingCodes = [265us] |> Set.ofList}
        {Name = "Malaysia"; ISO = UpperLatin2.TryParse("MY").Value; UnAlpha = UpperLatin3.TryParse("MYS").Value; UnNum = 458us; CallingCodes = [60us] |> Set.ofList}
        {Name = "Maldives"; ISO = UpperLatin2.TryParse("MV").Value; UnAlpha = UpperLatin3.TryParse("MDV").Value; UnNum = 462us; CallingCodes = [960us] |> Set.ofList}
        {Name = "Mali"; ISO = UpperLatin2.TryParse("ML").Value; UnAlpha = UpperLatin3.TryParse("MLI").Value; UnNum = 466us; CallingCodes = [223us] |> Set.ofList}
        {Name = "Malta"; ISO = UpperLatin2.TryParse("MT").Value; UnAlpha = UpperLatin3.TryParse("MLT").Value; UnNum = 470us; CallingCodes = [356us] |> Set.ofList}
        {Name = "Marshall Islands"; ISO = UpperLatin2.TryParse("MH").Value; UnAlpha = UpperLatin3.TryParse("MHL").Value; UnNum = 584us; CallingCodes = [692us] |> Set.ofList}
        {Name = "Martinique"; ISO = UpperLatin2.TryParse("MQ").Value; UnAlpha = UpperLatin3.TryParse("MTQ").Value; UnNum = 474us; CallingCodes = [596us] |> Set.ofList}
        {Name = "Mauritania"; ISO = UpperLatin2.TryParse("MR").Value; UnAlpha = UpperLatin3.TryParse("MRT").Value; UnNum = 478us; CallingCodes = [222us] |> Set.ofList}
        {Name = "Mauritius"; ISO = UpperLatin2.TryParse("MU").Value; UnAlpha = UpperLatin3.TryParse("MUS").Value; UnNum = 480us; CallingCodes = [230us] |> Set.ofList}
        {Name = "Mayotte"; ISO = UpperLatin2.TryParse("YT").Value; UnAlpha = UpperLatin3.TryParse("MYT").Value; UnNum = 175us; CallingCodes = [262us] |> Set.ofList}
        {Name = "Mexico"; ISO = UpperLatin2.TryParse("MX").Value; UnAlpha = UpperLatin3.TryParse("MEX").Value; UnNum = 484us; CallingCodes = [52us] |> Set.ofList}
        {Name = "Micronesia, Federated States of"; ISO = UpperLatin2.TryParse("FM").Value; UnAlpha = UpperLatin3.TryParse("FSM").Value; UnNum = 583us; CallingCodes = [691us] |> Set.ofList}
        {Name = "Moldova, Republic of"; ISO = UpperLatin2.TryParse("MD").Value; UnAlpha = UpperLatin3.TryParse("MDA").Value; UnNum = 498us; CallingCodes = [373us] |> Set.ofList}
        {Name = "Monaco"; ISO = UpperLatin2.TryParse("MC").Value; UnAlpha = UpperLatin3.TryParse("MCO").Value; UnNum = 492us; CallingCodes = [377us] |> Set.ofList}
        {Name = "Mongolia"; ISO = UpperLatin2.TryParse("MN").Value; UnAlpha = UpperLatin3.TryParse("MNG").Value; UnNum = 496us; CallingCodes = [976us] |> Set.ofList}
        {Name = "Montenegro"; ISO = UpperLatin2.TryParse("ME").Value; UnAlpha = UpperLatin3.TryParse("MNE").Value; UnNum = 499us; CallingCodes = [382us] |> Set.ofList}
        {Name = "Montserrat"; ISO = UpperLatin2.TryParse("MS").Value; UnAlpha = UpperLatin3.TryParse("MSR").Value; UnNum = 500us; CallingCodes = [1664us] |> Set.ofList}
        {Name = "Morocco"; ISO = UpperLatin2.TryParse("MA").Value; UnAlpha = UpperLatin3.TryParse("MAR").Value; UnNum = 504us; CallingCodes = [212us] |> Set.ofList}
        {Name = "Mozambique"; ISO = UpperLatin2.TryParse("MZ").Value; UnAlpha = UpperLatin3.TryParse("MOZ").Value; UnNum = 508us; CallingCodes = [258us] |> Set.ofList}
        {Name = "Myanmar"; ISO = UpperLatin2.TryParse("MM").Value; UnAlpha = UpperLatin3.TryParse("MMR").Value; UnNum = 104us; CallingCodes = [95us] |> Set.ofList}
        {Name = "Namibia"; ISO = UpperLatin2.TryParse("NA").Value; UnAlpha = UpperLatin3.TryParse("NAM").Value; UnNum = 516us; CallingCodes = [264us] |> Set.ofList}
        {Name = "Nauru"; ISO = UpperLatin2.TryParse("NR").Value; UnAlpha = UpperLatin3.TryParse("NRU").Value; UnNum = 520us; CallingCodes = [674us] |> Set.ofList}
        {Name = "Nepal"; ISO = UpperLatin2.TryParse("NP").Value; UnAlpha = UpperLatin3.TryParse("NPL").Value; UnNum = 524us; CallingCodes = [977us] |> Set.ofList}
        {Name = "Netherlands"; ISO = UpperLatin2.TryParse("NL").Value; UnAlpha = UpperLatin3.TryParse("NLD").Value; UnNum = 528us; CallingCodes = [31us] |> Set.ofList}
        {Name = "New Caledonia"; ISO = UpperLatin2.TryParse("NC").Value; UnAlpha = UpperLatin3.TryParse("NCL").Value; UnNum = 540us; CallingCodes = [687us] |> Set.ofList}
        {Name = "New Zealand"; ISO = UpperLatin2.TryParse("NZ").Value; UnAlpha = UpperLatin3.TryParse("NZL").Value; UnNum = 554us; CallingCodes = [64us] |> Set.ofList}
        {Name = "Nicaragua"; ISO = UpperLatin2.TryParse("NI").Value; UnAlpha = UpperLatin3.TryParse("NIC").Value; UnNum = 558us; CallingCodes = [505us] |> Set.ofList}
        {Name = "Niger"; ISO = UpperLatin2.TryParse("NE").Value; UnAlpha = UpperLatin3.TryParse("NER").Value; UnNum = 562us; CallingCodes = [227us] |> Set.ofList}
        {Name = "Nigeria"; ISO = UpperLatin2.TryParse("NG").Value; UnAlpha = UpperLatin3.TryParse("NGA").Value; UnNum = 566us; CallingCodes = [234us] |> Set.ofList}
        {Name = "Niue"; ISO = UpperLatin2.TryParse("NU").Value; UnAlpha = UpperLatin3.TryParse("NIU").Value; UnNum = 570us; CallingCodes = [683us] |> Set.ofList}
        {Name = "Norfolk Island"; ISO = UpperLatin2.TryParse("NF").Value; UnAlpha = UpperLatin3.TryParse("NFK").Value; UnNum = 574us; CallingCodes = [672us] |> Set.ofList}
        {Name = "Northern Mariana Islands"; ISO = UpperLatin2.TryParse("MP").Value; UnAlpha = UpperLatin3.TryParse("MNP").Value; UnNum = 580us; CallingCodes = [1670us] |> Set.ofList}
        {Name = "Norway"; ISO = UpperLatin2.TryParse("NO").Value; UnAlpha = UpperLatin3.TryParse("NOR").Value; UnNum = 578us; CallingCodes = [47us] |> Set.ofList}
        {Name = "Oman"; ISO = UpperLatin2.TryParse("OM").Value; UnAlpha = UpperLatin3.TryParse("OMN").Value; UnNum = 512us; CallingCodes = [968us] |> Set.ofList}
        {Name = "Pakistan"; ISO = UpperLatin2.TryParse("PK").Value; UnAlpha = UpperLatin3.TryParse("PAK").Value; UnNum = 586us; CallingCodes = [92us] |> Set.ofList}
        {Name = "Palau"; ISO = UpperLatin2.TryParse("PW").Value; UnAlpha = UpperLatin3.TryParse("PLW").Value; UnNum = 585us; CallingCodes = [680us] |> Set.ofList}
        {Name = "Palestine, State of"; ISO = UpperLatin2.TryParse("PS").Value; UnAlpha = UpperLatin3.TryParse("PSE").Value; UnNum = 275us; CallingCodes = [970us] |> Set.ofList}
        {Name = "Panama"; ISO = UpperLatin2.TryParse("PA").Value; UnAlpha = UpperLatin3.TryParse("PAN").Value; UnNum = 591us; CallingCodes = [507us] |> Set.ofList}
        {Name = "Papua New Guinea"; ISO = UpperLatin2.TryParse("PG").Value; UnAlpha = UpperLatin3.TryParse("PNG").Value; UnNum = 598us; CallingCodes = [675us] |> Set.ofList}
        {Name = "Paraguay"; ISO = UpperLatin2.TryParse("PY").Value; UnAlpha = UpperLatin3.TryParse("PRY").Value; UnNum = 600us; CallingCodes = [595us] |> Set.ofList}
        {Name = "Peru"; ISO = UpperLatin2.TryParse("PE").Value; UnAlpha = UpperLatin3.TryParse("PER").Value; UnNum = 604us; CallingCodes = [51us] |> Set.ofList}
        {Name = "Philippines"; ISO = UpperLatin2.TryParse("PH").Value; UnAlpha = UpperLatin3.TryParse("PHL").Value; UnNum = 608us; CallingCodes = [63us] |> Set.ofList}
        {Name = "Pitcairn"; ISO = UpperLatin2.TryParse("PN").Value; UnAlpha = UpperLatin3.TryParse("PCN").Value; UnNum = 612us; CallingCodes = [870us] |> Set.ofList}
        {Name = "Poland"; ISO = UpperLatin2.TryParse("PL").Value; UnAlpha = UpperLatin3.TryParse("POL").Value; UnNum = 616us; CallingCodes = [48us] |> Set.ofList}
        {Name = "Portugal"; ISO = UpperLatin2.TryParse("PT").Value; UnAlpha = UpperLatin3.TryParse("PRT").Value; UnNum = 620us; CallingCodes = [351us] |> Set.ofList}
        {Name = "Puerto Rico"; ISO = UpperLatin2.TryParse("PR").Value; UnAlpha = UpperLatin3.TryParse("PRI").Value; UnNum = 630us; CallingCodes = [1us] |> Set.ofList}
        {Name = "Qatar"; ISO = UpperLatin2.TryParse("QA").Value; UnAlpha = UpperLatin3.TryParse("QAT").Value; UnNum = 634us; CallingCodes = [974us] |> Set.ofList}
        {Name = "Romania"; ISO = UpperLatin2.TryParse("RO").Value; UnAlpha = UpperLatin3.TryParse("ROU").Value; UnNum = 642us; CallingCodes = [40us] |> Set.ofList}
        {Name = "Russian Federation"; ISO = UpperLatin2.TryParse("RU").Value; UnAlpha = UpperLatin3.TryParse("RUS").Value; UnNum = 643us; CallingCodes = [7us] |> Set.ofList}
        {Name = "Rwanda"; ISO = UpperLatin2.TryParse("RW").Value; UnAlpha = UpperLatin3.TryParse("RWA").Value; UnNum = 646us; CallingCodes = [250us] |> Set.ofList}
        {Name = "Reunion"; ISO = UpperLatin2.TryParse("RE").Value; UnAlpha = UpperLatin3.TryParse("REU").Value; UnNum = 638us; CallingCodes = [262us] |> Set.ofList}
        {Name = "Saint Barthelemy"; ISO = UpperLatin2.TryParse("BL").Value; UnAlpha = UpperLatin3.TryParse("BLM").Value; UnNum = 652us; CallingCodes = [590us] |> Set.ofList}
        {Name = "Saint Helena"; ISO = UpperLatin2.TryParse("SH").Value; UnAlpha = UpperLatin3.TryParse("SHN").Value; UnNum = 654us; CallingCodes = [290us] |> Set.ofList}
        {Name = "Saint Kitts and Nevis"; ISO = UpperLatin2.TryParse("KN").Value; UnAlpha = UpperLatin3.TryParse("KNA").Value; UnNum = 659us; CallingCodes = [1869us] |> Set.ofList}
        {Name = "Saint Lucia"; ISO = UpperLatin2.TryParse("LC").Value; UnAlpha = UpperLatin3.TryParse("LCA").Value; UnNum = 662us; CallingCodes = [1758us] |> Set.ofList}
        {Name = "Saint Martin (French part)"; ISO = UpperLatin2.TryParse("MF").Value; UnAlpha = UpperLatin3.TryParse("MAF").Value; UnNum = 663us; CallingCodes = [590us] |> Set.ofList}
        {Name = "Saint Pierre and Miquelon"; ISO = UpperLatin2.TryParse("PM").Value; UnAlpha = UpperLatin3.TryParse("SPM").Value; UnNum = 666us; CallingCodes = [508us] |> Set.ofList}
        {Name = "Saint Vincent and the Grenadines"; ISO = UpperLatin2.TryParse("VC").Value; UnAlpha = UpperLatin3.TryParse("VCT").Value; UnNum = 670us; CallingCodes = [1784us] |> Set.ofList}
        {Name = "Samoa"; ISO = UpperLatin2.TryParse("WS").Value; UnAlpha = UpperLatin3.TryParse("WSM").Value; UnNum = 882us; CallingCodes = [685us] |> Set.ofList}
        {Name = "San Marino"; ISO = UpperLatin2.TryParse("SM").Value; UnAlpha = UpperLatin3.TryParse("SMR").Value; UnNum = 674us; CallingCodes = [378us] |> Set.ofList}
        {Name = "Sao Tome and Principe"; ISO = UpperLatin2.TryParse("ST").Value; UnAlpha = UpperLatin3.TryParse("STP").Value; UnNum = 678us; CallingCodes = [239us] |> Set.ofList}
        {Name = "Saudi Arabia"; ISO = UpperLatin2.TryParse("SA").Value; UnAlpha = UpperLatin3.TryParse("SAU").Value; UnNum = 682us; CallingCodes = [966us] |> Set.ofList}
        {Name = "Senegal"; ISO = UpperLatin2.TryParse("SN").Value; UnAlpha = UpperLatin3.TryParse("SEN").Value; UnNum = 686us; CallingCodes = [221us] |> Set.ofList}
        {Name = "Serbia"; ISO = UpperLatin2.TryParse("RS").Value; UnAlpha = UpperLatin3.TryParse("SRB").Value; UnNum = 688us; CallingCodes = [381us] |> Set.ofList}
        {Name = "Seychelles"; ISO = UpperLatin2.TryParse("SC").Value; UnAlpha = UpperLatin3.TryParse("SYC").Value; UnNum = 690us; CallingCodes = [248us] |> Set.ofList}
        {Name = "Sierra Leone"; ISO = UpperLatin2.TryParse("SL").Value; UnAlpha = UpperLatin3.TryParse("SLE").Value; UnNum = 694us; CallingCodes = [232us] |> Set.ofList}
        {Name = "Singapore"; ISO = UpperLatin2.TryParse("SG").Value; UnAlpha = UpperLatin3.TryParse("SGP").Value; UnNum = 702us; CallingCodes = [65us] |> Set.ofList}
        {Name = "Sint Maarten (Dutch part)"; ISO = UpperLatin2.TryParse("SX").Value; UnAlpha = UpperLatin3.TryParse("SXM").Value; UnNum = 534us; CallingCodes = [1721us] |> Set.ofList}
        {Name = "Slovakia"; ISO = UpperLatin2.TryParse("SK").Value; UnAlpha = UpperLatin3.TryParse("SVK").Value; UnNum = 703us; CallingCodes = [421us] |> Set.ofList}
        {Name = "Slovenia"; ISO = UpperLatin2.TryParse("SI").Value; UnAlpha = UpperLatin3.TryParse("SVN").Value; UnNum = 705us; CallingCodes = [386us] |> Set.ofList}
        {Name = "Solomon Islands"; ISO = UpperLatin2.TryParse("SB").Value; UnAlpha = UpperLatin3.TryParse("SLB").Value; UnNum = 90us; CallingCodes = [677us] |> Set.ofList}
        {Name = "Somalia"; ISO = UpperLatin2.TryParse("SO").Value; UnAlpha = UpperLatin3.TryParse("SOM").Value; UnNum = 706us; CallingCodes = [252us] |> Set.ofList}
        {Name = "South Africa"; ISO = UpperLatin2.TryParse("ZA").Value; UnAlpha = UpperLatin3.TryParse("ZAF").Value; UnNum = 710us; CallingCodes = [27us] |> Set.ofList}
        {Name = "South Georgia and the South Sandwich Islands"; ISO = UpperLatin2.TryParse("GS").Value; UnAlpha = UpperLatin3.TryParse("SGS").Value; UnNum = 239us; CallingCodes = [500us] |> Set.ofList}
        {Name = "South Sudan"; ISO = UpperLatin2.TryParse("SS").Value; UnAlpha = UpperLatin3.TryParse("SSD").Value; UnNum = 728us; CallingCodes = [211us] |> Set.ofList}
        {Name = "Spain"; ISO = UpperLatin2.TryParse("ES").Value; UnAlpha = UpperLatin3.TryParse("ESP").Value; UnNum = 724us; CallingCodes = [34us] |> Set.ofList}
        {Name = "Sri Lanka"; ISO = UpperLatin2.TryParse("LK").Value; UnAlpha = UpperLatin3.TryParse("LKA").Value; UnNum = 144us; CallingCodes = [94us] |> Set.ofList}
        {Name = "Sudan"; ISO = UpperLatin2.TryParse("SD").Value; UnAlpha = UpperLatin3.TryParse("SDN").Value; UnNum = 729us; CallingCodes = [249us] |> Set.ofList}
        {Name = "Suriname"; ISO = UpperLatin2.TryParse("SR").Value; UnAlpha = UpperLatin3.TryParse("SUR").Value; UnNum = 740us; CallingCodes = [597us] |> Set.ofList}
        {Name = "Svalbard and Jan Mayen"; ISO = UpperLatin2.TryParse("SJ").Value; UnAlpha = UpperLatin3.TryParse("SJM").Value; UnNum = 744us; CallingCodes = [47us] |> Set.ofList}
        {Name = "Swaziland"; ISO = UpperLatin2.TryParse("SZ").Value; UnAlpha = UpperLatin3.TryParse("SWZ").Value; UnNum = 748us; CallingCodes = [268us] |> Set.ofList}
        {Name = "Sweden"; ISO = UpperLatin2.TryParse("SE").Value; UnAlpha = UpperLatin3.TryParse("SWE").Value; UnNum = 752us; CallingCodes = [46us] |> Set.ofList}
        {Name = "Switzerland"; ISO = UpperLatin2.TryParse("CH").Value; UnAlpha = UpperLatin3.TryParse("CHE").Value; UnNum = 756us; CallingCodes = [41us] |> Set.ofList}
        {Name = "Syrian Arab Republic"; ISO = UpperLatin2.TryParse("SY").Value; UnAlpha = UpperLatin3.TryParse("SYR").Value; UnNum = 760us; CallingCodes = [963us] |> Set.ofList}
        {Name = "Taiwan, Province of China"; ISO = UpperLatin2.TryParse("TW").Value; UnAlpha = UpperLatin3.TryParse("TWN").Value; UnNum = 158us; CallingCodes = [886us] |> Set.ofList}
        {Name = "Tajikistan"; ISO = UpperLatin2.TryParse("TJ").Value; UnAlpha = UpperLatin3.TryParse("TJK").Value; UnNum = 762us; CallingCodes = [992us] |> Set.ofList}
        {Name = "United Republic of Tanzania"; ISO = UpperLatin2.TryParse("TZ").Value; UnAlpha = UpperLatin3.TryParse("TZA").Value; UnNum = 834us; CallingCodes = [255us] |> Set.ofList}
        {Name = "Thailand"; ISO = UpperLatin2.TryParse("TH").Value; UnAlpha = UpperLatin3.TryParse("THA").Value; UnNum = 764us; CallingCodes = [66us] |> Set.ofList}
        {Name = "Timor-Leste"; ISO = UpperLatin2.TryParse("TL").Value; UnAlpha = UpperLatin3.TryParse("TLS").Value; UnNum = 626us; CallingCodes = [670us] |> Set.ofList}
        {Name = "Togo"; ISO = UpperLatin2.TryParse("TG").Value; UnAlpha = UpperLatin3.TryParse("TGO").Value; UnNum = 768us; CallingCodes = [228us] |> Set.ofList}
        {Name = "Tokelau"; ISO = UpperLatin2.TryParse("TK").Value; UnAlpha = UpperLatin3.TryParse("TKL").Value; UnNum = 772us; CallingCodes = [690us] |> Set.ofList}
        {Name = "Tonga"; ISO = UpperLatin2.TryParse("TO").Value; UnAlpha = UpperLatin3.TryParse("TON").Value; UnNum = 776us; CallingCodes = [676us] |> Set.ofList}
        {Name = "Trinidad and Tobago"; ISO = UpperLatin2.TryParse("TT").Value; UnAlpha = UpperLatin3.TryParse("TTO").Value; UnNum = 780us; CallingCodes = [1868us] |> Set.ofList}
        {Name = "Tunisia"; ISO = UpperLatin2.TryParse("TN").Value; UnAlpha = UpperLatin3.TryParse("TUN").Value; UnNum = 788us; CallingCodes = [216us] |> Set.ofList}
        {Name = "Turkey"; ISO = UpperLatin2.TryParse("TR").Value; UnAlpha = UpperLatin3.TryParse("TUR").Value; UnNum = 792us; CallingCodes = [90us] |> Set.ofList}
        {Name = "Turkmenistan"; ISO = UpperLatin2.TryParse("TM").Value; UnAlpha = UpperLatin3.TryParse("TKM").Value; UnNum = 795us; CallingCodes = [993us] |> Set.ofList}
        {Name = "Turks and Caicos Islands"; ISO = UpperLatin2.TryParse("TC").Value; UnAlpha = UpperLatin3.TryParse("TCA").Value; UnNum = 796us; CallingCodes = [1649us] |> Set.ofList}
        {Name = "Tuvalu"; ISO = UpperLatin2.TryParse("TV").Value; UnAlpha = UpperLatin3.TryParse("TUV").Value; UnNum = 798us; CallingCodes = [688us] |> Set.ofList}
        {Name = "Uganda"; ISO = UpperLatin2.TryParse("UG").Value; UnAlpha = UpperLatin3.TryParse("UGA").Value; UnNum = 800us; CallingCodes = [256us] |> Set.ofList}
        {Name = "Ukraine"; ISO = UpperLatin2.TryParse("UA").Value; UnAlpha = UpperLatin3.TryParse("UKR").Value; UnNum = 804us; CallingCodes = [380us] |> Set.ofList}
        {Name = "United Arab Emirates"; ISO = UpperLatin2.TryParse("AE").Value; UnAlpha = UpperLatin3.TryParse("ARE").Value; UnNum = 784us; CallingCodes = [971us] |> Set.ofList}
        {Name = "United Kingdom"; ISO = UpperLatin2.TryParse("GB").Value; UnAlpha = UpperLatin3.TryParse("GBR").Value; UnNum = 826us; CallingCodes = [44us] |> Set.ofList}
        {Name = "United States"; ISO = UpperLatin2.TryParse("US").Value; UnAlpha = UpperLatin3.TryParse("USA").Value; UnNum = 840us; CallingCodes = [1us] |> Set.ofList}
        {Name = "United States Minor Outlying Islands"; ISO = UpperLatin2.TryParse("UM").Value; UnAlpha = UpperLatin3.TryParse("UMI").Value; UnNum = 581us; CallingCodes = [1us] |> Set.ofList}
        {Name = "Uruguay"; ISO = UpperLatin2.TryParse("UY").Value; UnAlpha = UpperLatin3.TryParse("URY").Value; UnNum = 858us; CallingCodes = [598us] |> Set.ofList}
        {Name = "Uzbekistan"; ISO = UpperLatin2.TryParse("UZ").Value; UnAlpha = UpperLatin3.TryParse("UZB").Value; UnNum = 860us; CallingCodes = [998us] |> Set.ofList}
        {Name = "Vanuatu"; ISO = UpperLatin2.TryParse("VU").Value; UnAlpha = UpperLatin3.TryParse("VUT").Value; UnNum = 548us; CallingCodes = [678us] |> Set.ofList}
        {Name = "Venezuela"; ISO = UpperLatin2.TryParse("VE").Value; UnAlpha = UpperLatin3.TryParse("VEN").Value; UnNum = 862us; CallingCodes = [58us] |> Set.ofList}
        {Name = "Viet Nam"; ISO = UpperLatin2.TryParse("VN").Value; UnAlpha = UpperLatin3.TryParse("VNM").Value; UnNum = 704us; CallingCodes = [84us] |> Set.ofList}
        {Name = "British Virgin Islands"; ISO = UpperLatin2.TryParse("VG").Value; UnAlpha = UpperLatin3.TryParse("VGB").Value; UnNum = 92us; CallingCodes = [1284us] |> Set.ofList}
        {Name = "US Virgin Islands"; ISO = UpperLatin2.TryParse("VI").Value; UnAlpha = UpperLatin3.TryParse("VIR").Value; UnNum = 850us; CallingCodes = [1340us] |> Set.ofList}
        {Name = "Wallis and Futuna"; ISO = UpperLatin2.TryParse("WF").Value; UnAlpha = UpperLatin3.TryParse("WLF").Value; UnNum = 876us; CallingCodes = [681us] |> Set.ofList}
        {Name = "Western Sahara"; ISO = UpperLatin2.TryParse("EH").Value; UnAlpha = UpperLatin3.TryParse("ESH").Value; UnNum = 732us; CallingCodes = [212us] |> Set.ofList}
        {Name = "Yemen"; ISO = UpperLatin2.TryParse("YE").Value; UnAlpha = UpperLatin3.TryParse("YEM").Value; UnNum = 887us; CallingCodes = [967us] |> Set.ofList}
        {Name = "Zambia"; ISO = UpperLatin2.TryParse("ZM").Value; UnAlpha = UpperLatin3.TryParse("ZMB").Value; UnNum = 894us; CallingCodes = [260us] |> Set.ofList}
        {Name = "Zimbabwe"; ISO = UpperLatin2.TryParse("ZW").Value; UnAlpha = UpperLatin3.TryParse("ZWE").Value; UnNum = 716us; CallingCodes = [263us] |> Set.ofList}
        {Name = "Aland Islands"; ISO = UpperLatin2.TryParse("AX").Value; UnAlpha = UpperLatin3.TryParse("ALA").Value; UnNum = 248us; CallingCodes = [358us] |> Set.ofList}
        ]
        |> Set.ofList

    let byCallingCodes =
        countries
        |> Set.fold (fun s country ->
            let codes1country =
                country.CallingCodes
                |> Set.fold (fun s' callingCode -> 
                    (callingCode, country)::s'
                    ) []
            codes1country @ s) []
        |> List.groupBy (fun (callingCode, _) -> callingCode)
        |> List.map (fun (callingCode, xs) -> 
                        let xs' =
                            xs
                            |> List.map (fun (_, country) -> country)
                            |> Set.ofList
                        callingCode, xs')
        |> dict
    let byName =
        countries
        |> Set.toList
        |> List.map (fun x -> x.Name, x)
        |> dict

    let states = 
        [
        {Name = "Alabama"; Abbreviation = "AL"}
        {Name = "Alaska"; Abbreviation = "AK"}
        {Name = "Arizona"; Abbreviation = "AZ"}
        {Name = "Arkansas"; Abbreviation = "AR"}
        {Name = "California"; Abbreviation = "CA"}
        {Name = "Colorado"; Abbreviation = "CO"}
        {Name = "Connecticut"; Abbreviation = "CT"}
        {Name = "Delaware"; Abbreviation = "DE"}
        {Name = "Florida"; Abbreviation = "FL"}
        {Name = "Georgia"; Abbreviation = "GA"}
        {Name = "Hawaii"; Abbreviation = "HI"}
        {Name = "Idaho"; Abbreviation = "ID"}
        {Name = "Illinois"; Abbreviation = "IL"}
        {Name = "Indiana"; Abbreviation = "IN"}
        {Name = "Iowa"; Abbreviation = "IA"}
        {Name = "Kansas"; Abbreviation = "KS"}
        {Name = "Kentucky"; Abbreviation = "KY"}
        {Name = "Louisiana"; Abbreviation = "LA"}
        {Name = "Maine"; Abbreviation = "ME"}
        {Name = "Maryland"; Abbreviation = "MD"}
        {Name = "Massachusetts"; Abbreviation = "MA"}
        {Name = "Michigan"; Abbreviation = "MI"}
        {Name = "Minnesota"; Abbreviation = "MN"}
        {Name = "Mississippi"; Abbreviation = "MS"}
        {Name = "Missouri"; Abbreviation = "MO"}
        {Name = "Montana"; Abbreviation = "MT"}
        {Name = "Nebraska"; Abbreviation = "NE"}
        {Name = "Nevada"; Abbreviation = "NV"}
        {Name = "New Hampshire"; Abbreviation = "NH"}
        {Name = "New Jersey"; Abbreviation = "NJ"}
        {Name = "New Mexico"; Abbreviation = "NM"}
        {Name = "New York"; Abbreviation = "NY"}
        {Name = "North Carolina"; Abbreviation = "NC"}
        {Name = "North Dakota"; Abbreviation = "ND"}
        {Name = "Ohio"; Abbreviation = "OH"}
        {Name = "Oklahoma"; Abbreviation = "OK"}
        {Name = "Oregon"; Abbreviation = "OR"}
        {Name = "Pennsylvania"; Abbreviation = "PA"}
        {Name = "Rhode Island"; Abbreviation = "RI"}
        {Name = "South Carolina"; Abbreviation = "SC"}
        {Name = "South Dakota"; Abbreviation = "SD"}
        {Name = "Tennessee"; Abbreviation = "TN"}
        {Name = "Texas"; Abbreviation = "TX"}
        {Name = "Utah"; Abbreviation = "UT"}
        {Name = "Vermont"; Abbreviation = "VT"}
        {Name = "Virginia"; Abbreviation = "VA"}
        {Name = "Washington"; Abbreviation = "WA"}
        {Name = "West Virginia"; Abbreviation = "WV"}
        {Name = "Wisconsin"; Abbreviation = "WI"}
        {Name = "Wyoming"; Abbreviation = "WY"}
        {Name = "American Samoa"; Abbreviation = "AS"}
        {Name = "District of Columbia"; Abbreviation = "DC"}
        {Name = "Federated States of Micronesia"; Abbreviation = "FM"}
        {Name = "Guam"; Abbreviation = "GU"}
        {Name = "Marshall Islands"; Abbreviation = "MH"}
        {Name = "Northern Mariana Islands"; Abbreviation = "MP"}
        {Name = "Palau"; Abbreviation = "PW"}
        {Name = "Puerto Rico"; Abbreviation = "PR"}
        {Name = "Virgin Islands"; Abbreviation = "VI"}
        ]

    let stateByAbbreviation =
        states
        |> List.map (fun x -> x.Abbreviation, x)
        |> dict

    let stateByName =
        states
        |> List.map (fun x -> x.Name, x)
        |> dict

type UsPhone internal (areaCode, exchange, suffix) =
    member __.AreaCode : Digits3 option = areaCode
    member __.Exchange : Digits3 =  exchange
    member __.Suffix : Digits4 = suffix
    member __.Value =
        match areaCode with
        | Some x -> Digits <| sprintf "%s%s%s" x.Value exchange.Value suffix.Value
        | None -> Digits <| sprintf "%s%s" exchange.Value suffix.Value
    member __.Formatted =
        match areaCode with
        | Some x ->
            sprintf "(%s) %s-%s" x.Value exchange.Value suffix.Value
        | None ->
            sprintf "%s-%s" exchange.Value suffix.Value
    override __.ToString() = __.Value.ToString()
    override __.Equals(yobj) = 
        match yobj with
        |  :? UsPhone as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (areaCode, exchange, suffix) = 
        //to do: fix to reject international prefixes
        let a = 
            match areaCode with
            | Some x ->
                Digits3.TryParse x
            | None -> None
        let e = Digits3.TryParse exchange
        let s = Digits4.TryParse suffix
        match e, s with
        | None, _ -> None
        | _, None -> None
        | Some ee, Some ss -> Some <| UsPhone (a, ee, ss)
    static member TryParse (phone : string) =
        //to do: fix to reject international prefixes
        if String.IsNullOrWhiteSpace phone then
            None
        else
            let digits = digitsFromString <| phone.Replace("(", "").Replace(")", "").Replace("-", "").Replace(" ", "")

            match digits.Length with
            | 10 ->
                let a = Digits3 <| new string(Array.sub digits 0 3)
                let e = Digits3 <| new string(Array.sub digits 3 3)
                let s = Digits4 <| new string(Array.sub digits 6 4)
                Some <| UsPhone (Some a, e, s)
            | 7 ->
                let e = Digits3 <| new string(Array.sub digits 0 3)
                let s = Digits4 <| new string(Array.sub digits 3 4)
                Some <| UsPhone (None, e, s)
            | _ ->
                None
    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? UsPhone as y -> 
                if __.AreaCode.IsSome && y.AreaCode.IsNone then -1
                elif __.AreaCode.IsNone && y.AreaCode.IsSome then 1
                elif __.AreaCode > y.AreaCode then 1
                elif __.AreaCode < y.AreaCode then -1
                elif __.Exchange > y.Exchange then 1
                elif __.Exchange < y.Exchange then -1
                elif __.Suffix > y.Suffix then 1
                elif __.Suffix < y.Suffix then -1
                else 0
            | _ -> invalidArg "UsPhone" "cannot compare values of different types"

type OtherPhone internal (phone) =
    member __.Value : Digits =  phone
    member __.Formatted = phone.ToString()
    override __.ToString() = __.Value.ToString()
    override __.Equals(yobj) = 
        match yobj with
        |  :? OtherPhone as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (phone : string) = 
    //other phone accespts any digit at least 4 long
        //to do: fix to reject international prefixes
        if String.IsNullOrWhiteSpace phone then
            None
        else
            let digits = digitsFromString phone

            if digits.Length < 5 || digits.Length > 14 then
                None
            else 
                new string(digits)
                |> Digits
                |> OtherPhone
                |> Some
    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? OtherPhone as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "OtherPhone" "cannot compare values of different types"

[<CustomEquality;CustomComparison>] 
type Phone =
    | UsPhone of UsPhone
    | OtherPhone of OtherPhone
    member __.Value = 
        match __ with
        | UsPhone x -> x.Value
        | OtherPhone x -> x.Value
    member __.Formatted = 
        match __ with
        | UsPhone x -> x.Formatted
        | OtherPhone x -> x.Formatted
    static member TryParse phone =
        match UsPhone.TryParse phone with
        | Some x -> Some <| Phone.UsPhone x
        | None ->
            match OtherPhone.TryParse phone with
            | Some x -> Some <| Phone.OtherPhone x
            | None -> None
    override __.ToString() = 
        match __ with
        | UsPhone x -> x.ToString()
        | OtherPhone x ->  x.ToString()
    override __.Equals(yobj) = 
        yobj.GetType() = __.GetType() && yobj.ToString() = __.ToString()
    override __.GetHashCode() = hash __
    interface IComparable with
        member __.CompareTo yobj =
            let yString = (unbox yobj).ToString()
            let xString = __.ToString()

            if xString > yString then 1
            elif xString < yString then -1
            else 0
       
type PhoneNumber internal (callingCode : UInt16 option, phone : Phone, extension : Digits option, tags : Tag Set) = 
    member __.CallingCode = callingCode    
    member __.Phone = phone
    member __.Extension = extension
    member __.Value =
        let cc =
            match callingCode with
            | Some x ->
                x.ToString()
            | None -> ""
        sprintf "%s%s%s" cc phone.Value.Value
            (match extension with
                 | Some x -> x.ToString()
                 | None -> "")
        |> Digits

    member __.Tags = tags

    member __.Formatted =
        sprintf "%s%s%s"
            (match callingCode with
            | Some x -> sprintf "+%s " <| x.ToString()
            | None -> "")

            phone.Formatted

            (match extension with
            | Some x -> " x" + x.ToString()
            | None -> "")
    override __.ToString() = __.Value.Value
    override __.Equals(yobj) = 
        match yobj with
        |  :? PhoneNumber as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (callingCode, phone, extension, tags) = 
        //max 15 digits including callingCode, excluding extension
        //https://github.com/googlei18n/libphonenumber
        //https://en.wikipedia.org/wiki/E.164

        let code =
            match callingCode with
            | Some cc ->
                match Digits.TryParse cc with
                | Some x -> 
                    match UInt16.TryParse x.Value |> toOption with
                    | Some y ->
                        if Countries.byCallingCodes.ContainsKey y then
                            if y = CallingCodes.NorthAmerica then
                                if phone.ToString().Length = 10 then
                                    Some y
                                else
                                    None
                            else
                                Some y
                        else None
                    | None ->
                        None
                | None -> 
                    None
            | None -> 
                None
        
        PhoneNumber (code, phone, (Option.map Digits extension), tags) |> Some 
    static member TryParse (phone : string, tags) : PhoneNumber option = 
        if String.IsNullOrWhiteSpace phone then
            None
        else
            let txt = phone.ToUpper().Split([|'X'|], 2)

            let extension =
                if txt.Length = 2 then
                    Digits.TryParse txt.[1]
                else
                    None

            let number = txt.[0].Trim()

            let tryRawCallingCode rawCC =
                match UInt16.TryParse rawCC |> toOption with
                | Some x ->
                    match Countries.byCallingCodes.TryGetValue x with
                    | true, _ -> Some (x, rawCC)
                    | _ -> None
                | None ->
                    None

            let digitsRaw = digitsFromString number

            if digitsRaw.Length > 4 then
                let digits = new String(digitsRaw)
                let callingCode, numberDigits =
                    if number.Length > 5 && number.StartsWith("+") then
                        if number.StartsWith("+") then
                            match seq {yield tryRawCallingCode <| digits.Substring(0, 4); 
                                        yield tryRawCallingCode <| digits.Substring(0, 3);
                                        yield tryRawCallingCode <| digits.Substring(0, 2);
                                        yield tryRawCallingCode <| digits.Substring(0, 1);
                                        }
                                        |> Seq.tryFind (fun x -> x.IsSome) with
                            | Some x -> 
                                let uint16, rawCC = x.Value
                                match Countries.byCallingCodes.TryGetValue uint16 with
                                | true, _ -> 
                                    if uint16 = CallingCodes.NorthAmerica then
                                        if digits.Length = 11 then
                                            (Some uint16), digits.Substring rawCC.Length
                                        else
                                            None, digits.Substring rawCC.Length
                                    else
                                        (Some uint16), digits.Substring rawCC.Length
                                | _ -> None, digits
                            | None -> None, digits
                        else
                            None, digits
                    else
                        None, digits

                match Phone.TryParse numberDigits with
                | Some phone ->
                    PhoneNumber (callingCode, phone, extension, tags) |> Some 
                | None ->
                    None
            else
                None

    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? PhoneNumber as y -> 
                if __.CallingCode.IsSome > y.CallingCode.IsNone then -1
                elif __.CallingCode.IsNone < y.CallingCode.IsSome then 1
                elif __.CallingCode > y.CallingCode then 1
                elif __.CallingCode < y.CallingCode then -1
                elif __.Phone > y.Phone then 1
                elif __.Phone < y.Phone then -1
                elif __.Extension > y.Extension then 1
                elif __.Extension < y.Extension then -1
                else 0
            | _ -> invalidArg "PhoneNumber" "cannot compare values of different types"

type Handle = 
    {
    Address : TrimNonEmptyString
    Tags : Tag Set
    }

type UriTagged internal (uri, tags) = 
    member __.Uri : Uri = uri
    member __.Tags : Set<Tag> = tags
    override __.ToString() = __.Uri.ToString()
    override __.Equals(yobj) = 
        match yobj with
        |  :? UriTagged as y -> (__.Uri.AbsolutePath = y.Uri.AbsolutePath)
        | _ -> false
    override __.GetHashCode() = __.Uri.GetHashCode()
    static member Create (uri, tags) = 
        UriTagged (uri, tags)
    static member TryParse ((uri : string), tags) = 
        if String.IsNullOrWhiteSpace uri then
            None
        else
            let x = Uri.EscapeUriString <| uri.Trim()
            match Uri.IsWellFormedUriString(x, UriKind.Absolute) with
            | true -> UriTagged ((Uri(x, UriKind.Absolute)), tags) |> Some
            | false -> None
    static member TryParse ((uri : string), uriKind, tags) =
        let x = Uri.EscapeUriString <| uri.Trim()
        match Uri.IsWellFormedUriString(x, uriKind) with
        | true -> UriTagged ((Uri(x, uriKind)), tags) |> Some
        | false -> None
    with
        interface IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? UriTagged as y -> 
                    if (__.Uri.IsAbsoluteUri && y.Uri.IsAbsoluteUri) && (__.Uri.AbsolutePath > y.Uri.AbsolutePath) then 1
                    elif (__.Uri.IsAbsoluteUri && y.Uri.IsAbsoluteUri) && (__.Uri.AbsolutePath < y.Uri.AbsolutePath) then -1
                    elif __.Uri.OriginalString > y.Uri.OriginalString then 1
                    elif __.Uri.OriginalString < y.Uri.OriginalString then -1
                    else 0
                | _ -> invalidArg "UriTagged" "cannot compare values of different types"

type Address =
    | PhysicalAddress of PhysicalAddress
    | EmailAddress of EmailAddress
    | PhoneNumber of PhoneNumber
    | Url of UriTagged
    | Handle of Handle

type Contact =
    {
    Names : ContactName Set
    Addresses : Address Set
    Tags : Tag Set
    }

type Agent =
    | Person of Contact
    | Uri of UriTagged

type Port (portNumber : int) =
    member __.Value = portNumber
    override __.ToString() = portNumber.ToString()
    static member TryParse portNumber = 
        match port portNumber with
        | Success _ -> Port portNumber |> Some
        | _ -> None

type SecurityProtocol =
    | SslTls
    | StartTls
    | Nothing

type Authentication =
    | Password
    | EncryptedPassword
    | OAuth2
    | TlsCert

type IMAP =
    {
    ServerName : string
    Port : Port
    Security : SecurityProtocol
    Authentication : Authentication
    }

type EmailAccountName =
    | NameIsEmailAddress
    | Other of string

type EmailAccount =
    {
    Name : EmailAccountName
    EmailAddress : EmailAddress
    ReplyToAddress : EmailAddress option
    SimpleName : SimpleName
    Signature : string
    SignatureRule : string
    SMTP : string
    Imap : IMAP
    }
