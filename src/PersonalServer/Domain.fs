namespace Jackfoxy.PersonalServer

open FSharpx.Choice
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
        interface System.IComparable with
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
        |> List.map TrimNonEmptyString.TryParse 
        |> List.choose id
    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? TrimNonEmptyString as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "TrimNonEmptyString" "cannot compare values of different types"

type DigitString internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? DigitString as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse (value : string) = 
        let s' = value.Trim()
        if String.length(s') = 0 then 
            None
        else
            let regex = new Regex("^[0-9]+$")

            if regex.IsMatch s' then 
                Some <| DigitString s'
            else 
                None
    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? DigitString as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "DigitString" "cannot compare values of different types"

type DigitString2 internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? DigitString2 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse value = verifyStringInt value 2 DigitString2
    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? DigitString2 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "DigitString2" "cannot compare values of different types"

type DigitString3 internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? DigitString3 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse value = verifyStringInt value 3 DigitString3
    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? DigitString3 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "DigitString3" "cannot compare values of different types"

type DigitString4 internal (value) =
    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? DigitString4 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash value
    static member TryParse value = verifyStringInt value 4 DigitString4
    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? DigitString4 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "DigitString4" "cannot compare values of different types"

type FullName internal (first, middle, family, nameOrder, tags) =
    member __.First : TrimNonEmptyString option = first 
    member __.Middle : TrimNonEmptyString list = middle 
    member __.Family : TrimNonEmptyString option = family 
    member __.NameOrder : NameOrder = nameOrder
    member __.Tags : Tag Set = tags
    member __.PersonName =
        let combineName nameParts =
            let name =
                nameParts
                |> List.concat
                |> String.concat " "
            PersonName(name, __.Tags)

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
        |  :? FullName as y -> (__.PersonName = y.PersonName)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse ((first : string option), middle, (family : string option), nameOrder, tags) =
        let fi = TrimNonEmptyString.TryParse first
        let m = TrimNonEmptyString.Parse middle
        let fa = TrimNonEmptyString.TryParse family
        match fi, m, fa with
        | Some _, _, _ -> FullName (fi, m, fa, nameOrder, tags) |> Some
        | _, x, _ when x.Length > 0 -> FullName (fi, m, fa, nameOrder, tags) |> Some 
        | _, _, Some _ -> FullName (fi, m, fa, nameOrder, tags) |> Some 
        | _ -> None
    interface System.IComparable with
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
    | Custom of (FullName -> PersonName)
and PersonName internal (name: string, tags : Tag Set) = 
    member __.Value = TrimNonEmptyString name
    member __.Tags = tags
    override __.ToString() = name
    override __.Equals(yobj) = 
        match yobj with
        |  :? PersonName as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (name, tags) =
        if String.IsNullOrWhiteSpace name then
            None        
        else 
            Some <| PersonName ((name.Trim()), tags)
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? PersonName as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "PersonName" "cannot compare values of different types"
and NameAndAffixes (salutations, personName, suffixes) =
    member __.Salutations = salutations 
    member __.PersonName : PersonName = personName
    member __.Suffixes = suffixes
    member __.Value =
        TrimNonEmptyString <| __.ToString()
    override __.ToString() = 
        [salutations |> List.map (fun (x : TrimNonEmptyString) -> x.Value);
        [personName.ToString()];
        suffixes |> List.map (fun (x : TrimNonEmptyString) -> x.Value)]
        |> List.concat
        |> String.concat " "
    override __.Equals(yobj) = 
        match yobj with
        |  :? NameAndAffixes as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse ((salutations : string list), (personName : string), (suffixes : string list), tags) =
        match PersonName.TryParse (personName, tags) with
        | Some x -> 
            Some <| NameAndAffixes ((TrimNonEmptyString.Parse salutations), x, (TrimNonEmptyString.Parse suffixes))
        | None -> None

    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? NameAndAffixes as y -> 
                if __.PersonName > y.PersonName then 1
                elif __.PersonName < y.PersonName then -1
                elif __.Salutations > y.Salutations then 1
                elif __.Salutations < y.Salutations then -1
                elif __.Suffixes > y.Suffixes then 1
                elif __.Suffixes < y.Suffixes then -1
                else 0
            | _ -> invalidArg "NameAndAffixes" "cannot compare values of different types"

type NameOfPerson =
    | Name of PersonName
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
    interface System.IComparable with
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
                        match  DigitString4.TryParse zipParts.[1] with
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

    interface System.IComparable with
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
    interface System.IComparable with
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
    interface System.IComparable with
        member __.CompareTo yobj =
            let yString = (unbox yobj).ToString()
            let xString = __.ToString()

            if xString > yString then 1
            elif xString <yString then -1
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
    interface System.IComparable with
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
    interface System.IComparable with
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
        //to do: @ is present, not start or end
        //      no illegal characters
        // https://tools.ietf.org/html/rfc2822
        // https://tools.ietf.org/html/rfc2822#section-3.2.4
        // https://tools.ietf.org/html/rfc2822#section-3.4.1
        // http://www.ex-parrot.com/pdw/Mail-RFC822-Address.html
        //
        // http://emailregex.com/
        // ( see also RFC 5322 )
        //  [a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?

        // http://stackoverflow.com/questions/297420/list-of-email-addresses-that-can-be-used-to-test-a-javascript-validation-script
        // http://code.iamcal.com/php/rfc822/tests/
        if String.IsNullOrWhiteSpace email then
            None
        else
            let s = email.Split '@'
            if s.Length = 2 && s.[0].Length > 0 && s.[1].Length > 0 then
                Some <| EmailAddress (email, tags)
            else
                None

    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? EmailAddress as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "EmailAddress" "cannot compare values of different types"

type UsPhone internal (areaCode, exchange, suffix) =
    member __.AreaCode : DigitString3 option = areaCode
    member __.Exchange : DigitString3 =  exchange
    member __.Suffix : DigitString4 = suffix
    member __.Value =
        match areaCode with
        | Some x -> DigitString <| sprintf "%s%s%s" x.Value exchange.Value suffix.Value
        | None -> DigitString <| sprintf "%s%s" exchange.Value suffix.Value
    member __.Formatted =
        match areaCode with
        | Some x ->
            sprintf "(%s) %s-%s" x.Value exchange.Value suffix.Value
        | None ->
            sprintf "%s-%s" exchange.Value suffix.Value
    override __.ToString() = __.Formatted
    override __.Equals(yobj) = 
        match yobj with
        |  :? UsPhone as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (areaCode, exchange, suffix) = 
        let a = 
            match areaCode with
            | Some x ->
                DigitString3.TryParse x
            | None -> None
        let e = DigitString3.TryParse exchange
        let s = DigitString4.TryParse suffix
        match e, s with
        | None, _ -> None
        | _, None -> None
        | Some ee, Some ss -> Some <| UsPhone (a, ee, ss)
    static member TryParse (phone : string) =
        let digits = digitsFromString phone

        match digits.Length with
        | 10 ->
            let a = DigitString3 <| new string(Array.sub digits 0 3)
            let e = DigitString3 <| new string(Array.sub digits 4 3)
            let s = DigitString4 <| new string(Array.sub digits 7 4)
            Some <| UsPhone (Some a, e, s)
        | 7 ->
            let e = DigitString3 <| new string(Array.sub digits 0 3)
            let s = DigitString4 <| new string(Array.sub digits 3 4)
            Some <| UsPhone (None, e, s)
        | _ ->
            None
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? UsPhone as y -> 
                if __.AreaCode > y.AreaCode then 1
                elif __.AreaCode < y.AreaCode then -1
                elif __.Exchange > y.Exchange then 1
                elif __.Exchange < y.Exchange then -1
                elif __.Suffix > y.Suffix then 1
                elif __.Suffix < y.Suffix then -1
                else 0
            | _ -> invalidArg "UsPhone" "cannot compare values of different types"
and OtherPhone internal (phone) =
    member __.Value : DigitString =  phone
    member __.Formatted = phone.ToString()
    override __.ToString() = __.Formatted
    override __.Equals(yobj) = 
        match yobj with
        |  :? OtherPhone as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (phone : string) = 
        let digits = digitsFromString phone

        if digits.Length < 4 then
            None
        else 
            new string(digits)
            |> DigitString
            |> OtherPhone
            |> Some
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? OtherPhone as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "OtherPhone" "cannot compare values of different types"
and Phone =
    | UsPhone of UsPhone
    | OtherPhone of OtherPhone
    member __.Value = 
        match __ with
        | UsPhone x -> x.Value
        | OtherPhone x -> x.Value
    override __.ToString() = __.Formatted  
    member __.Formatted = 
        match __ with
        | UsPhone x -> x.Formatted
        | OtherPhone x -> x.Formatted
       
type PhoneNumber internal (countryCode : string option, phone : Phone, extension : int option, tags : Tag Set) = 
    member __.CountryCode = Option.map DigitString2 <| countryCode       
    member __.Phone = phone
    member __.Extension = extension
    member __.Value =
        let cc =
            match countryCode with
            | Some x ->
                new string(digitsFromString x)
            | None -> ""
        sprintf "%s%s%s" cc phone.Value.Value
            (match extension with
                 | Some x -> x.ToString()
                 | None -> "")
        |> DigitString

     member __.Tags = tags

    member __.Formatted =
        sprintf "%s%s%s"
            (match countryCode with
            | Some x -> x + " "
            | None -> "")

            phone.Formatted

            (match extension with
            | Some x -> " x" + x.ToString()
            | None -> "")
    override __.ToString() = __.Formatted
    override __.Equals(yobj) = 
        match yobj with
        |  :? PhoneNumber as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse countryCode phone extension = 
         PhoneNumber (countryCode, phone, extension, Set.empty) |> Some    //to do
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? PhoneNumber as y -> 
                if __.CountryCode > y.CountryCode then 1
                elif __.CountryCode < y.CountryCode then -1
                elif __.Phone > y.Phone then 1
                elif __.Phone < y.Phone then -1
                elif __.Extension > y.Extension then 1
                elif __.Extension < y.Extension then -1
                else 0
            | _ -> invalidArg "PhoneNumber" "cannot compare values of different types"

type Handle = 
    {
    Address : string
    Tags : Tag Set
    }

type Uri =    
    val Uri : System.Uri
    new (uri) =
        { Uri = new System.Uri(uri);}
    new (uri, (uriKind : System.UriKind)) =
        { Uri = new System.Uri(uri, uriKind);}

    override __.ToString() = __.Uri.ToString()
    override __.Equals(yobj) = 
        match yobj with
        |  :? Uri as y -> (__.Uri.AbsolutePath = y.Uri.AbsolutePath)
        | _ -> false
    override __.GetHashCode() = __.Uri.GetHashCode()

    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? Uri as y -> 
                    if __.Uri.AbsolutePath > y.Uri.AbsolutePath then 1
                    elif __.Uri.AbsolutePath < y.Uri.AbsolutePath then -1
                    else 0
                | _ -> invalidArg "Uri" "cannot compare values of different types"

type Address =
    | PhysicalAddress of PhysicalAddress
    | EmailAddress of EmailAddress
    | PhoneNumber of PhoneNumber
    | Url of Uri
    | OtherHandle of Handle

type Person =
    {
    Names : NameOfPerson Set
    Addresses : Address Set
    Tags : Tag Set
    }

type Agent =
    | Person of Person
    | Uri of Uri

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
    PersonName : PersonName
    Signature : string
    SignatureRule : string
    SMTP : string
    Imap : IMAP
    }
