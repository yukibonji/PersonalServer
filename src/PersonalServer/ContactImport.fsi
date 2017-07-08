namespace Jackfoxy.PersonalServer

open System 

module ContactImport =
    
    type ImportSourceMeta =
        {
        PrimaryName : TrimNonEmptyString
        TimeStamp : DateTime
        }

    val simpleEntityBuilder : source : string -> headers : string [] -> tryParse : (string * Set<Tag> -> 'a option) -> displ : int -> columns : string [] -> 
        'a option * Set<Tag>

    val entityBuilders : source : string -> headers : string [] -> tryParse : (string * Set<Tag> -> 'a option) ->  coveredHeaderColumns : int [] ->  entityCstr : ('a -> 'b) -> 
        (string [] -> 'b option * Set<Tag>) list * int []

    val commonBuilders : source : string -> headers : string [] -> 
        (string [] -> ContactName option * Set<Tag>) list * (string [] -> Address option * Set<Tag>) list * int []

    val contactImport : sources : seq<'a> -> sourceBuilder : ('a -> 'b) ->  nameBuilders : seq<'b -> ContactName option * Set<Tag>> -> addressBuilders : seq<'b -> Address option * Set<Tag>> -> 
        seq<Contact>
