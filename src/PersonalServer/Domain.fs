namespace Jackfoxy.PersonalServer

open DomainVerifications
open Utilities
open System

//type Tag = Tag of string with
//    member __.Get = match __ with Tag  r -> r
//    override __.ToString() = __.Get
type Tag(tag) =
    do
        if String.IsNullOrEmpty tag then Failure ("Tag", "tag is null or empty")
        else Success ()
        |> verifyConstructor
    member __.Value = tag
    override __.ToString() = __.Value
    override __.Equals(yobj) = 
        match yobj with
        |  :? Tag as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = __.Value.GetHashCode()

    with
        interface System.IComparable with
            member __.CompareTo yobj =
                match yobj with
                | :? Tag as y -> 
                    if __.Value > y.Value then 1
                    elif __.Value < y.Value then -1
                    else 0
                | _ -> invalidArg "Tag" "cannot compare values of different types"

[<CustomEquality; CustomComparison>]
type PersonFullName =
    {
    Salutation : string option
    First : string option
    Middle : string list
    Family : string option
    Suffix : string option
    NameOrder : NameOrder
    Tags : Tag Set
    }
    member __.PersonName =
        let combineName foo =
            let name =
                foo
                |> List.concat
                |> String.concat " "
            PersonName(name, __.Tags)

        match __.NameOrder with
        | Western -> 
            [Option.toList __.Salutation;
            Option.toList __.First;
            __.Middle;
            Option.toList __.Family;
            Option.toList __.Suffix]
            |> combineName
        | FamilyFirst ->
            [Option.toList __.Salutation;
            Option.toList __.Family;
            Option.toList __.First;
            __.Middle;
            Option.toList __.Suffix]
            |> combineName
        | Custom f -> f __
    override __.Equals(yobj) = 
        match yobj with
        |  :? PersonFullName as y -> (__.PersonName = y.PersonName)
        | _ -> false
    override __.GetHashCode() = hash __
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? PersonFullName as y -> 
                if __.Family > y.Family then 1
                elif __.Family < y.Family then -1
                elif __.First > y.First then 1
                elif __.First < y.First then -1
                elif __.Middle > y.Middle then 1
                elif __.Middle < y.Middle then -1
                else 0
            | _ -> invalidArg "PersonFullName" "cannot compare values of different types"
and NameOrder =
    /// Salutation, First, Middle, Family, Suffix
    | Western
    /// Salutation Family, First, Middle, Suffix
    | FamilyFirst
    | Custom of (PersonFullName -> PersonName)
and PersonName (name: string, tags : Tag Set) = 
    member __.Value = name
    member __.Tags = tags
    override __.ToString() = name
    override __.Equals(yobj) = 
        match yobj with
        |  :? PersonName as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
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
    | FullName of PersonFullName

type ZipCode =
    | ZipCode5 of ZipCode5
    | ZipCode5Plus4 of ZipCode5Plus4
and ZipCode5(zip) =
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
    static member Parse zip = 
        match zipCode5 zip with
        | Success _ -> ZipCode5 zip |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? ZipCode5 as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "ZipCode5" "cannot compare values of different types"
and ZipCode5Plus4(zip : string) =
    do
        zipCode5Plus4 zip
        |> verifyConstructor
        
    member __.Value = zip
    override __.ToString() = zip
    override __.Equals(yobj) = 
        match yobj with
        |  :? ZipCode5Plus4 as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member Parse zip = 
        match zipCode5Plus4 zip with
        | Success _ -> ZipCode5Plus4 zip |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? ZipCode5Plus4 as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "ZipCode5Plus4" "cannot compare values of different types"
and OtherPostalCode(other : string) =
    member __.Value = other
    override __.ToString() = other
    override __.Equals(yobj) = 
        match yobj with
        |  :? OtherPostalCode as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member Parse other = 
        match otherPostalCode other with
        | Success _ -> OtherPostalCode other |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
    interface System.IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? OtherPostalCode as y -> 
                if __.Value > y.Value then 1
                elif __.Value < y.Value then -1
                else 0
            | _ -> invalidArg "OtherPostalCode" "cannot compare values of different types"

type PostalCode =
    | ZipCode
    | Other of string  

type PhysicalAddress =
    {
    StreetAddress : string list option
    City : string option
    State : string option
    PostalCode : PostalCode option
    Country : string option
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
    static member Parse email = 
        match emailAddress email with
        | Success _ -> EmailAddress (email, Set.empty) |> Success
        | Failure (caller, msg) -> Failure (caller,msg)
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

    member __.AreaCode = areaCode
    member __.Exchange = exchange
    member __.Suffix = suffix
    member __.Value =
        [Option.toList areaCode;
        [exchange];
        [suffix];]
        |> combineNumber
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
    static member Parse areaCode exchange suffix = 
        match usPhone areaCode exchange suffix with
        | Success _ -> UsPhone (areaCode, exchange, suffix) |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
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
    member __.Value = numbersFromString phone
    member __.Formatted = phone
    override __.ToString() = __.Formatted
    override __.Equals(yobj) = 
        match yobj with
        |  :? OtherPhone as y -> (__.Value = y.Value)
        | _ -> false
    override __.GetHashCode() = hash __
    static member Parse phone = 
        match otherPhone phone with
        | Success _ -> OtherPhone phone |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
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
        
    member __.CountryCode = countryCode
    member __.Phone = phone
    member __.Extension = extension
    member __.Value =
        match countryCode with
        | Some x ->
            numbersFromString x
        | None -> ""
        +
        phone.Value
        +
        match extension with
        | Some x -> x.ToString()
        | None -> ""

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
    static member Parse countryCode phone extension = 
        match phoneNumber countryCode phone extension with
        | Success _ -> PhoneNumber (countryCode, phone, extension, Set.empty) |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
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
    | Person of string
    | Uri of Uri

type Port (portNumber : int) =
    do
        port portNumber
        |> verifyConstructor

    member __.Value = portNumber
    override __.ToString() = portNumber.ToString()
    static member Parse portNumber = 
        match port portNumber with
        | Success _ -> Port portNumber |> Success
        | Failure (caller,msg) -> Failure (caller,msg)

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
