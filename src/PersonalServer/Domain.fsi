namespace Jackfoxy.PersonalServer

open System 

[<Class>]
type Tag =  //to do: equals performance testing -- http://stackoverflow.com/questions/28142655/iequatable-in-f-operator-performance-and-structural-equality
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : tag:string -> Tag option

[<Class>]
type TrimNonEmptyString =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string option -> TrimNonEmptyString option
    static member TryParse : value:string -> TrimNonEmptyString option
    static member Parse : value:string list -> TrimNonEmptyString list

[<Class>]
type DigitString =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> DigitString option

[<Class>]
type DigitString2 =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> DigitString2 option

[<Class>]
type DigitString3 =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> DigitString3 option

[<Class>]
type DigitString4 =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> DigitString4 option


//to do: name with affixes
//type NameAndAffixes
[<Class>]  
type FullName =
    interface System.IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    member First: TrimNonEmptyString option
    member Middle: TrimNonEmptyString list
    member Family: TrimNonEmptyString option
    member NameOrder: NameOrder
    member Tags: Set<Tag>
    member PersonName : PersonName
    static member TryParse : first: string option * middle: string list * family: string option * nameOrder: NameOrder * tags:Set<Tag> -> FullName option
and NameOrder =
    /// Salutation, First, Middle, Family, Suffix
    | Western
    /// Salutation Family, First, Middle, Suffix
    | FamilyFirst
    | Custom of (FullName -> PersonName)
and [<Class>] PersonName =
    interface System.IComparable
    override ToString : unit -> string
    member Tags : Set<Tag>
    member Value : TrimNonEmptyString
    static member TryParse : name:string * tags:Set<Tag> -> PersonName option
and NameAndAffixes =
    new : salutation: TrimNonEmptyString list * personName : PersonName * suffix: TrimNonEmptyString list -> NameAndAffixes
    interface System.IComparable
    override ToString : unit -> string
    member Salutations: TrimNonEmptyString list
    member PersonName : PersonName
    member Suffixes: TrimNonEmptyString list
    member Value : TrimNonEmptyString
    static member TryParse : salutations: string list * personName : string * suffixes: string list * tags:Set<Tag> -> NameAndAffixes option

type NameOfPerson =
    | Name of PersonName
    | FullName of FullName
    | NameAndAffixes of NameAndAffixes

[<Class>]
 type ZipCode5 =
      interface IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : string
      static member TryParse : zip:string -> ZipCode5 option

[<Class>]
 type ZipCode5Plus4 =
      interface IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : string
      member ValueFormatted : string
      static member TryParse : zip:string -> ZipCode5Plus4 option

[<Class>]
 type NonUsPostalCode =
      interface IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : TrimNonEmptyString
      static member TryParse : postalCode: string -> NonUsPostalCode option

 type ZipCode =
    | ZipCode5 of ZipCode5
    | ZipCode5Plus4 of ZipCode5Plus4

type PostalCode =
    | ZipCode of ZipCode
    | NonUsPostalCode of NonUsPostalCode

type PhysicalAddress =
    {StreetAddress: TrimNonEmptyString list
     City: TrimNonEmptyString option
     State: TrimNonEmptyString option
     PostalCode: PostalCode option
     Country: TrimNonEmptyString option
     Tags: Set<Tag>}

[<Class>]
type EmailAddress =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Tags : Set<Tag>
      member Value : string
      static member TryParse : email:string -> EmailAddress option
      static member TryParse : email:string * tags:Tag Set -> EmailAddress option

[<Class>]
type UsPhone =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member AreaCode : DigitString3 option
      member Exchange : DigitString3
      member Formatted : string
      member Suffix : DigitString4
      member Value : DigitString
      static member TryParse : areaCode:string option -> exchange:string -> suffix:string -> UsPhone option
and [<Class>] OtherPhone =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Formatted : string
      member Value : DigitString
      static member TryParse : phone:string -> OtherPhone option
and Phone =
    | UsPhone of UsPhone
    | OtherPhone of OtherPhone
    with
      override ToString : unit -> string
      member Formatted : string
      member Value : DigitString

[<Class>]
type PhoneNumber =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member CountryCode : DigitString2 option
      member Extension : int option
      member Formatted : string
      member Phone : Phone
      member Tags : Set<Tag>
      member Value : DigitString
      static member TryParse : countryCode:string option -> phone:Phone -> extension:int option -> PhoneNumber option

type Handle =
    {Address: string
     Tags: Set<Tag>}

type Uri =
      interface System.IComparable
      new : uri:string -> Uri
      new : uri:string * uriKind:System.UriKind -> Uri
      val Uri: System.Uri
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string

type Address =
    | PhysicalAddress of PhysicalAddress
    | EmailAddress of EmailAddress
    | PhoneNumber of PhoneNumber
    | Url of Uri
    | OtherHandle of Handle

type Person =
    {Names: Set<NameOfPerson>
     Addresses: Set<Address>
     Tags: Set<Tag>}

type Agent =
    | Person of Person
    | Uri of Uri

type Port =
      new : portNumber:int -> Port
      override ToString : unit -> string
      member Value : int
      static member TryParse : portNumber:int -> Port option

type SecurityProtocol =
    | SslTls
    | StartTls
    | Nothing

type Authentication =
    | Password
    | EncryptedPassword
    | OAuth2
    | TlsCert

/// IMAP commands defined in RFC 3501. https://tools.ietf.org/html/rfc3501
/// 
/// https://godoc.org/github.com/emersion/go-imap/commands (https://github.com/emersion/go-imap)
/// https://github.com/alienscience/imapsrv
type IMAP =
    {ServerName: string
     Port: Port
     Security: SecurityProtocol
     Authentication: Authentication}

type EmailAccountName =
    | NameIsEmailAddress
    | Other of string

type EmailAccount =
    {Name: EmailAccountName
     EmailAddress: EmailAddress
     ReplyToAddress: EmailAddress option
     PersonName: PersonName
     Signature: string
     SignatureRule: string
     SMTP: string
     Imap: IMAP}