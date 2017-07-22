namespace Jackfoxy.PersonalServer

open System 
open System.Collections.Generic

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
type UtcDateTime =
    interface IComparable
    new : dateTime : DateTime -> UtcDateTime
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : DateTime

[<Class>]
type NonEmptySet<'T when 'T : comparison> =
    interface IComparable
    member Value : Set<'T>
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    static member TryParse : set : Set<'T> -> NonEmptySet<'T> option
    static member TryParse : values : seq<'T> -> NonEmptySet<'T> option

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
type Source =
    interface IComparable
    new : primary : TrimNonEmptyString * secondary : TrimNonEmptyString option * earliestTimeStamp : UtcDateTime * latestTimeStamp : UtcDateTime -> Source
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Primary : TrimNonEmptyString
    member Secondary : TrimNonEmptyString option
    member EarliestTimeStamp : UtcDateTime
    member LatestTimeStamp : UtcDateTime
    static member TryParse : primary : string * secondary : string option * earliestTimeStamp : DateTime * latestTimeStamp : DateTime -> Source option
    static member Parse : primary : TrimNonEmptyString * secondary : string option * earliestTimeStamp : DateTime * latestTimeStamp : DateTime -> Source

[<Class>]
type Tag =  //to do: equals performance testing -- http://stackoverflow.com/questions/28142655/iequatable-in-f-operator-performance-and-structural-equality
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string
    static member TryParse : tag:string -> Tag option

[<Class>]  
type FullName =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member First: TrimNonEmptyString option
    member Middle: TrimNonEmptyString list
    member Family: TrimNonEmptyString option
    member NameOrder: NameOrder
    member Tags: Set<Tag>
    member Sources : NonEmptySet<Source>
    member SimpleName : SimpleName
    static member TryParse : first: string option * middle: string list * family: string option * nameOrder: NameOrder * tags:Set<Tag> * sources:NonEmptySet<Source> -> FullName option
and NameOrder =
    /// Salutation, First, Middle, Family, Suffix
    | Western
    /// Salutation Family, First, Middle, Suffix
    | FamilyFirst
    | Custom of (FullName -> SimpleName)
and [<Class>] SimpleName =
    interface IComparable
    override ToString : unit -> string
    member Tags : Set<Tag>
    member Sources : NonEmptySet<Source>
    member Value : TrimNonEmptyString
    static member TryParse : name:string * tags:Set<Tag> * sources:NonEmptySet<Source> -> SimpleName option
and NameAndAffixes =
    new : salutation: TrimNonEmptyString list * simpleName : SimpleName * suffix: TrimNonEmptyString list * sources:NonEmptySet<Source> -> NameAndAffixes
    interface IComparable
    override ToString : unit -> string
    member Salutations: TrimNonEmptyString list
    member SimpleName : SimpleName
    member Suffixes: TrimNonEmptyString list
    member Value : TrimNonEmptyString
    member Sources : NonEmptySet<Source>
    static member TryParse : salutations: string list * simpleName : string * suffixes: string list * tags:Set<Tag> * sources:NonEmptySet<Source> -> NameAndAffixes option

type ContactName =
    | SimpleName of SimpleName
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
    interface IComparable
    static member TryParse : postalCode: string -> ZipCode option

[<CustomEquality;CustomComparison>]
type PostalCode =
    | ZipCode of ZipCode
    | NonUsPostalCode of NonUsPostalCode
    interface IComparable
    static member TryParse : postalCode: string -> PostalCode option

[<Class>]
type PhysicalAddress =
    interface IComparable
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member StreetAddress: TrimNonEmptyString list
    member City: TrimNonEmptyString option
    member State: TrimNonEmptyString option
    member PostalCode: PostalCode option
    member Country: TrimNonEmptyString option
    member Tags: Set<Tag>
    member Sources : NonEmptySet<Source>
    static member TryParse : streetAddress: string list * city: string option * state: string option * postalCode: string option * country: string option * tags:Set<Tag> * sources:NonEmptySet<Source> -> PhysicalAddress option

[<Class>]
type EmailAddress =
      interface IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Tags : Set<Tag>
      member Sources : NonEmptySet<Source>
      member Value : string
      static member TryParse : email:string * tags:Tag Set  * sources:NonEmptySet<Source> -> EmailAddress option

[<Class>]
type UsPhone =
      interface IComparable
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
      interface IComparable
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
      interface IComparable
      static member TryParse : phone:string -> Phone option

type Country =
    {
    Name        : string
    ISO         : UpperLatin2
    UnAlpha     : UpperLatin3
    UnNum       : UInt16
    CallingCodes  : Set<UInt16>
    }

type State =
    {
    Name : string
    Abbreviation : string
    }

[<Class>]
type PhoneNumber =
      interface IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member CallingCode : UInt16 option
      member Extension : Digits option
      member Formatted : string
      member Phone : Phone
      member Tags : Set<Tag>
      member Sources : NonEmptySet<Source>
      member Value : Digits
      static member TryParse : callingCode : string option * phone:Phone * extension:string option * tags:Set<Tag> * sources:NonEmptySet<Source> -> PhoneNumber option
      static member TryParse : phone:string * tags:Set<Tag> * sources:NonEmptySet<Source> -> PhoneNumber option

type Handle =
    {Address: TrimNonEmptyString
     Tags: Set<Tag>}

[<Class>]
type UriTagged =
      interface IComparable
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Uri : Uri
      member Tags : Set<Tag>
      member Sources : NonEmptySet<Source>
      static member Create : uri : Uri * tags:Set<Tag> * sources:NonEmptySet<Source> -> UriTagged
      static member TryParse : uri : string * tags:Set<Tag> * sources:NonEmptySet<Source> -> UriTagged option
      static member TryParse : uri : string * uriKind:UriKind * tags:Set<Tag> * sources:NonEmptySet<Source> -> UriTagged option

type Address =
    | PhysicalAddress of PhysicalAddress
    | EmailAddress of EmailAddress
    | PhoneNumber of PhoneNumber
    | Url of UriTagged
    | Handle of Handle

type Contact =
    {Names: Set<ContactName>
     Addresses: Set<Address>
     Tags: Set<Tag>}

type Agent =
    | Person of Contact
    | Uri of UriTagged

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
     SimpleName: SimpleName
     Signature: string
     SignatureRule: string
     SMTP: string
     Imap: IMAP}

module Countries =
    val countries : Set<Country>
    val byCallingCodes : IDictionary<uint16, Country Set>
    val byName : IDictionary<string, Country>
    val stateByAbbreviation : IDictionary<string, State>
    val stateByName : IDictionary<string, State>

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module SimpleName =
    val tryElimination : simpleName1 : SimpleName -> simpleName2 : SimpleName -> SimpleName option

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module FullName =
    val tryElimination : fullName1 : FullName -> fullName2 : FullName -> FullName option
    val tryEliminateSimpleName : fullName : FullName -> simpleName : SimpleName -> FullName option

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module NameAndAffixes =
    val tryElimination : nameAndAffixes1 : NameAndAffixes -> nameAndAffixes2 : NameAndAffixes -> NameAndAffixes option
    val tryEliminateSimpleName : nameAndAffixes : NameAndAffixes -> simpleName : SimpleName -> NameAndAffixes option

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ContactName =
    val elimination : contactNames : ContactName list -> ContactName list

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PhysicalAddress =
    val tryElimination : physicalAddress1 : PhysicalAddress -> physicalAddress2 : PhysicalAddress -> PhysicalAddress option

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EmailAddress =
    val tryElimination : emailAddress1 : EmailAddress -> emailAddress2 : EmailAddress -> EmailAddress option

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module PhoneNumber =
    val tryElimination : phoneNumber1 : PhoneNumber -> phoneNumber2 : PhoneNumber -> PhoneNumber option

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module UriTagged =
    val tryElimination : uriTagged1 : UriTagged -> uriTagged2 : UriTagged -> UriTagged option

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Handle =
    val tryElimination : handle1 : Handle -> handle2 : Handle -> Handle option

[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Address =
    val elimination : addresses : Address list -> Address list
