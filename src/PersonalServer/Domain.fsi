namespace Jackfoxy.PersonalServer

open System 

//type Tag =
//  | Tag of string
//  with
//    override ToString : unit -> string
//    member Get : string
type Tag =
    interface IComparable
    new : tag:string -> Tag
    override Equals : yobj:obj -> bool
    override GetHashCode : unit -> int
    override ToString : unit -> string
    member Value : string

[<CustomEquality; CustomComparison>]
type PersonFullName =
  {Salutation: string option;
   First: string option;
   Middle: string list;
   Family: string option;
   Suffix: string option;
   NameOrder: NameOrder;
   Tags: Set<Tag>;}
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
    new : name:string * tags:Set<Tag> -> PersonName
    override ToString : unit -> string
    member Tags : Set<Tag>
    member Value : string

type NameOfPerson =
    | Name of PersonName
    | FullName of PersonFullName

type ZipCode =
    | ZipCode5 of ZipCode5
    | ZipCode5Plus4 of ZipCode5Plus4
  and ZipCode5 =
      interface IComparable
      new : zip:string -> ZipCode5
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : string
      static member Parse : zip:string -> Choice<ZipCode5,(string * string)>
  and ZipCode5Plus4 =
      interface IComparable
      new : zip:string -> ZipCode5Plus4
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : string
      static member Parse : zip:string -> Choice<ZipCode5Plus4,(string * string)>
  and OtherPostalCode =
      interface IComparable
      new : other:string -> OtherPostalCode
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Value : string
      static member Parse : other:string -> Choice<OtherPostalCode,(string * string)>

type PostalCode =
    | ZipCode
    | Other of string

type PhysicalAddress =
    {StreetAddress: string list option;
     City: string option;
     State: string option;
     PostalCode: PostalCode option;
     Country: string option;
     Tags: Set<Tag>;}

type EmailAddress =
      interface System.IComparable
      new : email:string * tags:Set<Tag> -> EmailAddress
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Tags : Set<Tag>
      member Value : string
      static member Parse : email:string -> Choice<EmailAddress,(string * string)>

type UsPhone =
      interface System.IComparable
      new : areaCode:string option * exchange:string * suffix:string -> UsPhone
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member AreaCode : string option
      member Exchange : string
      member Formatted : string
      member Suffix : string
      member Value : string
      static member Parse : areaCode:string option -> exchange:string -> suffix:string -> Choice<UsPhone,(string * string)>
and OtherPhone =
      interface System.IComparable
      new : phone:string -> OtherPhone
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member Formatted : string
      member Value : string
      static member Parse : phone:string -> Choice<OtherPhone,(string * string)>
and Phone =
    | UsPhone of UsPhone
    | OtherPhone of OtherPhone
    with
      override ToString : unit -> string
      member Formatted : string
      member Value : string

type PhoneNumber =
      interface System.IComparable
      new : countryCode:string option * phone:Phone * extension:int option *  tags:Set<Tag> -> PhoneNumber
      override Equals : yobj:obj -> bool
      override GetHashCode : unit -> int
      override ToString : unit -> string
      member CountryCode : string option
      member Extension : int option
      member Formatted : string
      member Phone : Phone
      member Tags : Set<Tag>
      member Value : string
      static member Parse : countryCode:string option -> phone:Phone -> extension:int option -> Choice<PhoneNumber,(string * string)>

type Handle =
    {Address: string;
     Tags: Set<Tag>;}

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
    {Names: Set<NameOfPerson>;
     Addresses: Set<Address>;
     Tags: Set<Tag>;}

type Agent =
    | Person of string
    | Uri of Uri

type Port =
      new : portNumber:int -> Port
      override ToString : unit -> string
      member Value : int
      static member Parse : portNumber:int -> Choice<Port,(string * string)>

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
    {ServerName: string;
     Port: Port;
     Security: SecurityProtocol;
     Authentication: Authentication;}

type EmailAccountName =
    | NameIsEmailAddress
    | Other of string

type EmailAccount =
    {Name: EmailAccountName;
     EmailAddress: EmailAddress;
     ReplyToAddress: EmailAddress option;
     PersonName: PersonName;
     Signature: string;
     SignatureRule: string;
     SMTP: string;
     Imap: IMAP;}