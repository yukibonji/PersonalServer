namespace Jackfoxy.PersonalServer

open System 

type Tag =
    interface IComparable
    new : tag:string -> Tag
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : tag:string -> Tag option

type NonEmptyString =
    interface IComparable
    new : value:string -> NonEmptyString
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> NonEmptyString option

type NonEmptyStringOption =
    interface IComparable
    new : value:string -> NonEmptyStringOption
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    member Value : string option

type DigitString =
    interface IComparable
    new : value:string -> DigitString
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> DigitString option

type DigitString2 =
    interface IComparable
    new : value:string -> DigitString2
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> DigitString2 option

type DigitString3 =
    interface IComparable
    new : value:string -> DigitString3
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> DigitString3 option

type DigitString4 =
    interface IComparable
    new : value:string -> DigitString4
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> DigitString4 option

[<CustomEquality; CustomComparison>]
type PersonFullName =
  {Salutation: NonEmptyString list
   First: NonEmptyStringOption
   Middle: NonEmptyString list
   Family: NonEmptyStringOption
   Suffix: NonEmptyString list
   NameOrder: NameOrder
   Tags: Set<Tag>}
  with
    interface System.IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    member PersonName : PersonName
and NameOrder =
  | Western
  | FamilyFirst
  | Custom of (PersonFullName -> PersonName)
and PersonName =
    interface System.IComparable
    new : name:NonEmptyString * tags:Set<Tag> -> PersonName
    override ToString : unit -> string
    member Tags : Set<Tag>
    member Value : string

type NameOfPerson =
    | Name of PersonName
    | FullName of PersonFullName

 type ZipCode5 =
      interface IComparable
      new : zip:string -> ZipCode5
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : string
      static member TryParse : zip:string -> ZipCode5 option

 type ZipCode5Plus4 =
      interface IComparable
      new : zip:string -> ZipCode5Plus4
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : string
      static member TryParse : zip:string -> ZipCode5Plus4 option

 type NonUsPostalCode =
      interface IComparable
      new : postalCode:NonEmptyString -> NonUsPostalCode
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : string

 type ZipCode =
    | ZipCode5 of ZipCode5
    | ZipCode5Plus4 of ZipCode5Plus4

type PostalCode =
    | ZipCode of ZipCode
    | NonUsPostalCode of NonUsPostalCode

type PhysicalAddress =
    {StreetAddress: NonEmptyString list
     City: NonEmptyStringOption
     State: NonEmptyStringOption
     PostalCode: PostalCode option
     Country: NonEmptyStringOption
     Tags: Set<Tag>}

type EmailAddress =
      interface System.IComparable
      new : email:string * tags:Set<Tag> -> EmailAddress
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Tags : Set<Tag>
      member Value : string
      static member TryParse : email:string -> EmailAddress option
      static member TryParse : email:string * tags:Tag Set -> EmailAddress option

type UsPhone =
      interface System.IComparable
      new : areaCode: string option * exchange:string * suffix:string -> UsPhone
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member AreaCode : DigitString3 option
      member Exchange : DigitString3
      member Formatted : string
      member Suffix : DigitString4
      member Value : DigitString
      static member TryParse : areaCode:string option -> exchange:string -> suffix:string -> UsPhone option
and OtherPhone =
      interface System.IComparable
      new : phone:string -> OtherPhone
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

type PhoneNumber =
      interface System.IComparable
      new : countryCode:string option * phone:Phone * extension:int option *  tags:Set<Tag> -> PhoneNumber
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