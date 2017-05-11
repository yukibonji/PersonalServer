namespace Jackfoxy.PersonalServer

open System 
open System.Collections.Generic

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
type Digits =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> Digits option

[<Class>]
type Digits2 =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> Digits2 option

[<Class>]
type Digits3 =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> Digits3 option

[<Class>]
type Digits4 =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> Digits4 option

[<Class>]  
type FullName =
    interface System.IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
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

[<CustomEquality;CustomComparison>]
 type ZipCode =
    | ZipCode5 of ZipCode5
    | ZipCode5Plus4 of ZipCode5Plus4
    interface System.IComparable
    static member TryParse : postalCode: string -> ZipCode option

[<CustomEquality;CustomComparison>]
type PostalCode =
    | ZipCode of ZipCode
    | NonUsPostalCode of NonUsPostalCode
    interface System.IComparable
    static member TryParse : postalCode: string -> PostalCode option

[<Class>]
type PhysicalAddress =
    interface System.IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member StreetAddress: TrimNonEmptyString list
    member City: TrimNonEmptyString option
    member State: TrimNonEmptyString option
    member PostalCode: PostalCode option
    member Country: TrimNonEmptyString option
    member Tags: Set<Tag>
    static member TryParse : streetAddress: string list * city: string option * state: string option * postalCode: string option * country: string option * tags:Set<Tag> -> PhysicalAddress option

[<Class>]
type EmailAddress =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Tags : Set<Tag>
      member Value : string
      static member TryParse : email:string * tags:Tag Set -> EmailAddress option

[<Class>]
type UsPhone =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member AreaCode : Digits3 option
      member Exchange : Digits3
      member Formatted : string
      member Suffix : Digits4
      member Value : Digits
      static member TryParse : areaCode:string option * exchange:string * suffix:string -> UsPhone option
      static member TryParse : phone:string -> UsPhone option

[<Class>] 
type OtherPhone =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Formatted : string
      member Value : Digits
      static member TryParse : phone:string -> OtherPhone option

[<CustomEquality;CustomComparison>] 
type Phone =
    | UsPhone of UsPhone
    | OtherPhone of OtherPhone
    with
      override ToString : unit -> string
      member Formatted : string
      member Value : Digits
      interface System.IComparable
      static member TryParse : phone:string -> Phone option

[<Class>]
type UpperLatin2 =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> UpperLatin2 option

[<Class>]
type UpperLatin3 =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : value:string -> UpperLatin3 option

type Country =
    {
    Name        : string
    ISO         : UpperLatin2
    UnAlpha     : UpperLatin3
    UnNum       : System.UInt16
    CallingCodes  : Set<UInt16>
    }

[<Class>]
type PhoneNumber =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member CallingCode : UInt16 option
      member Extension : Digits option
      member Formatted : string
      member Phone : Phone
      member Tags : Set<Tag>
      member Value : Digits
      static member TryParse : callingCode : string option * phone:Phone * extension:string option * tags:Set<Tag>-> PhoneNumber option
      static member TryParse : phone:string * tags:Set<Tag> -> PhoneNumber option

type Handle =
    {Address: TrimNonEmptyString
     Tags: Set<Tag>}

[<Class>]
type Uri =
      interface System.IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Uri: System.Uri
      member Tags : Set<Tag>
      static member Create : uri : System.Uri * tags:Set<Tag>-> Uri
      static member TryParse : uri : string * tags:Set<Tag>-> Uri option
      static member TryParse : uri : string * uriKind:System.UriKind * tags:Set<Tag>-> Uri option

type Address =
    | PhysicalAddress of PhysicalAddress
    | EmailAddress of EmailAddress
    | PhoneNumber of PhoneNumber
    | Url of Uri
    | Handle of Handle

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

module Countries =
    val countries : Set<Country>
    val byCallingCodes : IDictionary<uint16, Country Set>