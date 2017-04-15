namespace Jackfoxy.PersonalServer

open DomainVerifications
open FSharpx.Choice
open System
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
    static member TryParse value = verifyStringInt value value.Length DigitString
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
    member __.First = first 
    member __.Middle = middle 
    member __.Family = family 
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
    static member TryParse (first, middle, family, nameOrder, tags) =
        match fullName first middle family with
        | Some (fi, m, fa) -> FullName (TrimNonEmptyString.TryParse fi, TrimNonEmptyString.Parse m, TrimNonEmptyString.TryParse fa, nameOrder, tags) |> Some 
        | None -> None
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
            | _ -> invalidArg "PersonFullName" "cannot compare values of different types"
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
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
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
        let zipParts = zip.Split '-'

        if zipParts.Length = 2 then
            match choose {
                            do! 
                                match verifyStringInt (zipParts.[0].Trim()) 5 id with
                                | Some _ -> Success ()
                                | _ -> Failure ""
                            do! 
                                match  verifyStringInt (zipParts.[1].Trim()) 4 id with
                                | Some _ -> Success ()
                                | _ -> Failure ""
                            return ()
                            } with
            | Success _ -> ZipCode5Plus4 (sprintf "%s%s" (zipParts.[0].Trim()) (zipParts.[1].Trim())) |> Some
            | _ -> None
        else
            match verifyStringInt (zip.Trim()) 9 id with
            | Some x -> ZipCode5Plus4 x |> Some
            | _ -> None

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

type ZipCode =
    | ZipCode5 of ZipCode5
    | ZipCode5Plus4 of ZipCode5Plus4

type PostalCode =
    | ZipCode of ZipCode
    | NonUsPostalCode of NonUsPostalCode

type PhysicalAddress =
    {
    StreetAddress : TrimNonEmptyString list
    City : TrimNonEmptyString option
    State : TrimNonEmptyString option
    PostalCode : PostalCode option
    Country : TrimNonEmptyString option
    Tags : Tag Set
    }

type EmailAddress internal (email : string, tags : Tag Set) =
    member __.Value = email
    member __.Tags = tags
    override __.ToString() = email
    override __.Equals(yobj) = 
        match yobj with
        |  :? EmailAddress as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse email = 
        match emailAddress email with
        | Success _ -> EmailAddress (email, Set.empty) |> Some
        | _ -> None
    static member TryParse (email, tags) = 
        match emailAddress email with
        | Success _ -> EmailAddress (email,tags) |> Some
        | _ -> None
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? EmailAddress as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "EmailAddress" "cannot compare values of different types"

type UsPhone internal (areaCode, exchange, suffix) =
    member __.AreaCode = Option.map DigitString3 <| areaCode
    member __.Exchange = DigitString3 exchange
    member __.Suffix = DigitString4 suffix
    member __.Value =
        [Option.toList areaCode;
        [exchange];
        [suffix];]
        |> combineNumber
        |> DigitString
    member __.Formatted =
        match areaCode with
        | Some x ->
            sprintf "(%s) %s-%s" x exchange suffix
        | None ->
            sprintf "%s-%s" exchange suffix
    override __.ToString() = __.Formatted
    override __.Equals(yobj) = 
        match yobj with
        |  :? UsPhone as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse areaCode exchange suffix = 
        match usPhone areaCode exchange suffix with
        | Some (a, e, s) -> UsPhone (a, e, s) |> Some
        | _ -> None
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
    member __.Value = DigitString <| numbersFromString phone
    member __.Formatted = phone
    override __.ToString() = __.Formatted
    override __.Equals(yobj) = 
        match yobj with
        |  :? OtherPhone as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse phone = 
        match otherPhone phone with
        | Success _ -> OtherPhone phone |> Some
        | _ -> None
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
                numbersFromString x
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
        match phoneNumber countryCode phone extension with
        | Success _ -> PhoneNumber (countryCode, phone, extension, Set.empty) |> Some
        | _ -> None
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
