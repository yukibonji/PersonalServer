namespace Jackfoxy.PersonalServer

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
    member PersonName : PersonName
and NameOrder =
    /// Salutation, First, Middle, Family, Suffix
    | Western
    /// Salutation Family, First, Middle, Suffix
    | FamilyFirst
    | Custom of (PersonFullName -> PersonName)
and PersonName = 
    new : string * Set<Tag> -> PersonName
    member Value : string
    member Tags : Set<Tag>
    
type NameOfPerson =
    | Name of PersonName
    | FullName of PersonFullName

type ZipCode =
  | ZipCode5 of ZipCode5
  | ZipCode5Plus4 of ZipCode5Plus4
and ZipCode5 =
    new : zip : string -> ZipCode5
    member Value : string
    static member Parse : zip : string -> Choice<ZipCode5,(string * string)>
and ZipCode5Plus4 =
    new : zip : string -> ZipCode5Plus4
    member Value : string
    static member Parse : zip : string -> Choice<ZipCode5Plus4,(string * string)>
and OtherPostalCode =
    new : other : string -> OtherPostalCode
    member Value : string
    static member Parse : other : string -> Choice<OtherPostalCode,(string * string)>

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
    
type EmailAddress =
    new : email : string * tags : Set<Tag> -> EmailAddress
    member Value : string
    member Tags : Set<Tag>
    static member Parse : email : string -> Choice<EmailAddress,(string * string)>

type UsPhone =
    new : areaCode : string option * exchange : string * suffix : string -> UsPhone
    member AreaCode : string option
    member Exchange : string
    member Formatted : string
    member Suffix : string
    member Value : string
    static member Parse : areaCode : string option -> exchange : string -> suffix : string -> Choice<UsPhone,(string * string)>
and OtherPhone =
    new : phone : string -> OtherPhone
    member Formatted : string
    member Value : string
    static member Parse : phone : string -> Choice<OtherPhone,(string * string)>
and Phone =
  | UsPhone of UsPhone
  | OtherPhone of OtherPhone

type PhoneNumber =
    new : countryCode : string option * phone : Phone * extension : int option * tags : Set<Tag> -> PhoneNumber
    member CountryCode : string option
    member Extension : int option
    member Formatted : string
    member Phone : Phone
    member Value : string
    member Tags : Set<Tag>
    static member Parse : countryCode:  string option -> phone : Phone -> extension : int option -> Choice<PhoneNumber, (string * string)>

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

type Port =
    new : port : int -> Port
    member Value : int
    static member Parse : portNumber:  int -> Choice<Port,(string * string)>

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