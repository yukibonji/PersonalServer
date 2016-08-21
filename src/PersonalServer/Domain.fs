namespace Jackfoxy.PersonalServer

open DomainVerifications
open Utilities

type Tag = string

type PersonFullName =
    {
    Salutation : string option
    First : string option
    Middle : string list
    Family : string option
    Suffix : string option
    NameOrder : NameOrder
    Tags : Set<Tag>
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
and NameOrder =
    /// Salutation, First, Middle, Family, Suffix
    | Western
    /// Salutation Family, First, Middle, Suffix
    | FamilyFirst
    | Custom of (PersonFullName -> PersonName)
and PersonName (name: string, tags : Set<Tag>) = 
    member __.Value = name
    member __.Tags = tags
    override __.ToString() = name
    
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
    static member Parse zip = 
        match zipCode5 zip with
        | Success _ -> ZipCode5 zip |> Success
        | Failure (caller,msg) -> Failure (caller,msg)

and ZipCode5Plus4(zip : string) =
    do
        zipCode5Plus4 zip
        |> verifyConstructor
        
    member __.Value = zip
    override __.ToString() = zip
    static member Parse zip = 
        match zipCode5Plus4 zip with
        | Success _ -> ZipCode5Plus4 zip |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
and OtherPostalCode(other : string) =
    member __.Value = other
    override __.ToString() = other
    static member Parse other = 
        match otherPostalCode other with
        | Success _ -> OtherPostalCode other |> Success
        | Failure (caller,msg) -> Failure (caller,msg)

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
    Tags : Set<Tag>
    }

type EmailAddress(email : string, tags : Set<Tag>) =
    do
        emailAddress email
        |> verifyConstructor
    member __.Value = email
    member __.Tags = tags
    override __.ToString() = email
    static member Parse email = 
        match emailAddress email with
        | Success _ -> EmailAddress (email, Set.empty) |> Success
        | Failure (caller, msg) -> Failure (caller,msg)

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
    static member Parse areaCode exchange suffix = 
        match usPhone areaCode exchange suffix with
        | Success _ -> UsPhone (areaCode, exchange, suffix) |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
and OtherPhone (phone) =
    do
        otherPhone phone
        |> verifyConstructor
    member __.Value = numbersFromString phone
    member __.Formatted = phone
    override __.ToString() = __.Formatted
    static member Parse phone = 
        match otherPhone phone with
        | Success _ -> OtherPhone phone |> Success
        | Failure (caller,msg) -> Failure (caller,msg)
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
          
type PhoneNumber (countryCode : string option, phone : Phone, extension : int option, tags : Set<Tag>) = 
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
    static member Parse countryCode phone extension = 
        match phoneNumber countryCode phone extension with
        | Success _ -> PhoneNumber (countryCode, phone, extension, Set.empty) |> Success
        | Failure (caller,msg) -> Failure (caller,msg)

type Handle = 
    {
    Address : string
    Tags : Set<Tag>
    }

type Address =
    | PhysicalAddress of PhysicalAddress
    | EmailAddress of EmailAddress
    | PhoneNumber of PhoneNumber
    | OtherHandle of Handle

type Person =
    {
    Names : NameOfPerson list
    Addresses : Address list
    Tags : Set<Tag>
    }

type Agent =
    | Person of string
    | Uri of string

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
