namespace Jackfoxy.PersonalServer

open DomainVerifications
open Utilities
open System

type Tag internal (tag: string) =
    do
        verifyNonEmptyString "Tag" tag
        |> verifyConstructor

    member __.Value = tag.Trim()
    override __.ToString() = __.Value
    override __.Equals(yobj) = 
        match yobj with
        |  :? Tag as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __.Value
    static member TryParse (tag : string) =
        match verifyNonEmptyString "NonEmptyString" tag with
        | Success () -> Tag tag |> Some
        | _ -> None

    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? Tag as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "Tag" "cannot compare values of different types"

type NonEmptyString internal (value : string) =
    do
        verifyNonEmptyString "NonEmptyString" value
        |> verifyConstructor

    member __.Value = value.Trim()
    override __.ToString() =  __.Value
    override __.Equals(yobj) = 
        match yobj with
        |  :? NonEmptyString as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __.Value
    static member TryParse (value : string) =
        match verifyNonEmptyString "NonEmptyString" value with
        | Success () -> NonEmptyString value |> Some
        | _ -> None
    static member TryParse (value : string option) =
        match value with
        | Some x ->
            match verifyNonEmptyString "NonEmptyString" x with
            | Success () -> NonEmptyString x |> Some
            | _ -> None
        | None -> None
    static member TryParse (xs : string list) =
        xs
        |> List.map NonEmptyString.TryParse 
        |> List.choose id

    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? NonEmptyString as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "NonEmptyString" "cannot compare values of different types"

type DigitString internal (value) =
    do
        verifyStringInt "DigitString" "DigitString" value value.Length
        |> verifyConstructor

    member __.Value = value
    override __.ToString() = value
    override __.Equals(yobj) = 
        match yobj with
        |  :? DigitString as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse value =
        match verifyStringInt "" "" value value.Length with
        | Success () -> DigitString value |> Some
        | _ -> None

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
    let digitString2 = ref String.Empty
    do
        verifyDigitString digitString2 2 value

    member __.Value = !digitString2
    override __.ToString() = !digitString2
    override __.Equals(yobj) = 
        match yobj with
        |  :? DigitString2 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse value =
        match verifyStringInt "" "" value 2 with
        | Success () -> DigitString2 value |> Some
        | _ -> None

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
    let digitString3 = ref String.Empty
    do
        verifyDigitString digitString3 3 value

    member __.Value = !digitString3
    override __.ToString() = !digitString3
    override __.Equals(yobj) = 
        match yobj with
        |  :? DigitString3 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse value =
        match verifyStringInt "" "" value 3 with
        | Success () -> DigitString3 value |> Some
        | _ -> None

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
    let digitString4 = ref String.Empty
    do
        verifyDigitString digitString4 4 value

    member __.Value = !digitString4
    override __.ToString() = !digitString4
    override __.Equals(yobj) = 
        match yobj with
        |  :? DigitString4 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse value =
        match verifyStringInt "" "" value 4 with
        | Success () -> DigitString4 value |> Some
        | _ -> None

    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? DigitString4 as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "DigitString4" "cannot compare values of different types"

type FullName internal (salutation : string list, first, middle, family, suffix : string list, nameOrder, tags) =
    do
        match fullName first middle family with
        | Success () -> ()
        | Failure _ -> fullName first middle family |> verifyConstructor

    member __.Salutation = NonEmptyString.TryParse salutation
    member __.First = NonEmptyString.TryParse first
    member __.Middle = NonEmptyString.TryParse middle
    member __.Family = NonEmptyString.TryParse family
    member __.Suffix = NonEmptyString.TryParse suffix
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
            [__.Salutation |> List.map (fun x -> x.Value);
            Option.toList <| Option.map (fun x -> x.ToString()) __.First;
            __.Middle |> List.map (fun x -> x.Value);
            Option.toList <| Option.map (fun x -> x.ToString()) __.Family;
            __.Suffix |> List.map (fun x -> x.Value)]
            |> combineName
        | FamilyFirst ->
            [__.Salutation |> List.map (fun x -> x.Value);
            Option.toList <| Option.map (fun x -> x.ToString()) __.Family;
            Option.toList <| Option.map (fun x -> x.ToString()) __.First;
            __.Middle |> List.map (fun x -> x.Value);
            __.Suffix |> List.map (fun x -> x.Value)]
            |> combineName
        | Custom f -> f __
    override __.Equals(yobj) = 
        match yobj with
        |  :? FullName as y -> (__.PersonName = y.PersonName)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (salutation, first, middle, family, suffix, nameOrder, tags) =
        match fullName first middle family with
        | Success () -> FullName (salutation, first, middle, family, suffix, nameOrder, tags) |> Some 
        | Failure _ -> None
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
    do
        verifyNonEmptyString "PersonName" name
        |> verifyConstructor

    member __.Value = NonEmptyString name
    member __.Tags = tags
    override __.ToString() = name
    override __.Equals(yobj) = 
        match yobj with
        |  :? PersonName as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse (name, tags) =
        match verifyNonEmptyString "PersonName" name with
        | Success () -> PersonName (name, tags) |> Some
        | _ -> None
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? PersonName as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "PersonName" "cannot compare values of different types"

type NameOfPerson =
    | Name of PersonName
    | FullName of FullName

type ZipCode5(zip) =
    do
        zipCode5 zip
        |> verifyConstructor

    member __.Value = zip
    override __.ToString() = zip
    override __.Equals(yobj) = 
        match yobj with
        |  :? ZipCode5 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse zip = 
        match zipCode5 zip with
        | Success _ -> ZipCode5 zip |> Some
        | _ -> None
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? ZipCode5 as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "ZipCode5" "cannot compare values of different types"

type ZipCode5Plus4(zip : string) =
    do
        zipCode5Plus4 zip
        |> verifyConstructor
        
    member __.Value = zip
    member __.ValueFormatted = zip
    override __.ToString() = zip
    override __.Equals(yobj) = 
        match yobj with
        |  :? ZipCode5Plus4 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse zip = 
        match zipCode5Plus4 zip with
        | Success _ -> ZipCode5Plus4 zip |> Some
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
    do
        verifyNonEmptyString "NonUsPostalCode" postalCode
        |> verifyConstructor

    member __.Value = NonEmptyString postalCode
    override __.ToString() = postalCode
    override __.Equals(yobj) = 
        match yobj with
        |  :? NonUsPostalCode as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member TryParse postalCode =
        match verifyNonEmptyString "NonUsPostalCode" postalCode with
        | Success () -> NonUsPostalCode postalCode |> Some
        | _ -> None
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
    StreetAddress : NonEmptyString list
    City : NonEmptyString option
    State : NonEmptyString option
    PostalCode : PostalCode option
    Country : NonEmptyString option
    Tags : Tag Set
    }

type EmailAddress(email : string, tags : Tag Set) =
    do
        emailAddress email
        |> verifyConstructor
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

type UsPhone (areaCode, exchange, suffix) =
    do
        usPhone areaCode exchange suffix
        |> verifyConstructor

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
        | Success _ -> UsPhone (areaCode, exchange, suffix) |> Some
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
and OtherPhone (phone) =
    do
        otherPhone phone
        |> verifyConstructor
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
       
type PhoneNumber (countryCode : string option, phone : Phone, extension : int option, tags : Tag Set) = 
    do 
        phoneNumber countryCode phone extension
        |> verifyConstructor
        
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
    do
        port portNumber
        |> verifyConstructor

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
