namespace Jackfoxy.PersonalServer

open System 

module ContactImport =
    
    type ImportSourceMeta =
        {
        PrimaryName : TrimNonEmptyString
        TimeStamp : DateTime
        Headers : string []
        }

    val simpleEntityBuilder : sourceMeta : ImportSourceMeta -> tryParse : (string * Set<Tag> * Set<Source> -> 'a option) -> displ : int -> columns : string [] -> 
        'a option * Set<Tag>

    val entityBuilders : sourceMeta : ImportSourceMeta -> tryParse : (string * Set<Tag> * Set<Source> -> 'a option) ->  coveredHeaderColumns : int [] ->  entityCstr : ('a -> 'b) -> 
        (string [] -> 'b option * Set<Tag>) list * int []

    val commonBuilders : sourceMeta : ImportSourceMeta -> (string [] -> ContactName option * Set<Tag>) list * (string [] -> Address option * Set<Tag>) list * int []

    val contactImport : sources : seq<'a> -> sourceBuilder : ('a -> 'b) ->  nameBuilders : seq<'b -> ContactName option * Set<Tag>> -> addressBuilders : seq<'b -> Address option * Set<Tag>> -> 
        seq<Contact>
