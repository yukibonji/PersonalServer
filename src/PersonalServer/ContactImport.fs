namespace Jackfoxy.PersonalServer

open System

module ContactImport =

    type ImportSourceMeta =
        {
        PrimaryName : TrimNonEmptyString
        TimeStamp : DateTime
        Headers : string []
        }

    type ContactSynonyms =
        {
        PhoneNumber: string []
        Email : string []
        Uri : string []
        SimpleName : string []
        FullNameFirstName : string []
        FullNameLastName : string []
        PhysicalAddress : string []
        PhysicalCity : string []
        PhysicalState : string []
        PhysicalPostalCode : string []
        PhysicalCountry : string []
        }

    let valueFromIndexOpt (columns : string []) indexOpt = 
        match indexOpt with
        | Some i -> 
            if String.IsNullOrWhiteSpace columns.[i] then
                None
            else
                Some columns.[i]
        | None ->
            None

    let headerSources primarySource sourceTimeStamp (headers : string []) (contentIndices : int seq) =
        contentIndices
        |> Seq.map (fun index -> 
            Source.Parse (primarySource, Some headers.[index], sourceTimeStamp, sourceTimeStamp) )
        |> Set.ofSeq
        |> NonEmptySet.TryParse

    let sourceTags (source : ImportSourceMeta) (headers : string []) (columns : string []) (contentIndices : int seq) =
        contentIndices
        |> Seq.choose (fun index -> 
            match TrimNonEmptyString.TryParse columns.[index] with
            | Some x ->
                let sources = 
                    Set.singleton <| Source.Parse(source.PrimaryName, Some headers.[index], source.TimeStamp, source.TimeStamp)
                    |> NonEmptySet.TryParse
                Tag.TryParse ((sprintf "%s::%s::%s" source.PrimaryName.Value headers.[index] columns.[index]), sources.Value)
            | None ->
                None )
        |> Set.ofSeq

    type PhysicalAddressBuilderParms =
        {
        AddressIndex : int list
        CityIndex : int option
        StateIndex : int option
        PostalCodeIndex : int option
        CountryIndex : int option
        }

    let physicalAddressBuilder (physicalAddressBuilderParms : PhysicalAddressBuilderParms) (sourceMeta : ImportSourceMeta) =
        fun (columns : string []) ->
            let contentIndices =
                physicalAddressBuilderParms.AddressIndex @
                   ([physicalAddressBuilderParms.CityIndex;
                    physicalAddressBuilderParms.StateIndex;
                    physicalAddressBuilderParms.PostalCodeIndex;
                    physicalAddressBuilderParms.CountryIndex]
                    |> List.choose id)

            let streetAddress: string list =
                physicalAddressBuilderParms.AddressIndex
                |> List.choose (fun i -> valueFromIndexOpt columns <| Some i)
                
            let city = valueFromIndexOpt columns physicalAddressBuilderParms.CityIndex
            let state = valueFromIndexOpt columns physicalAddressBuilderParms.StateIndex
            let postalCode = valueFromIndexOpt columns physicalAddressBuilderParms.PostalCodeIndex
            let country = valueFromIndexOpt columns physicalAddressBuilderParms.CountryIndex

            match  headerSources sourceMeta.PrimaryName sourceMeta.TimeStamp sourceMeta.Headers contentIndices with
            | Some sources ->
                match PhysicalAddress.TryParse (streetAddress, city, state, postalCode, country, Set.empty, sources) with
                | Some physicalAddress ->
                    (Some physicalAddress, Set.empty)
                | None ->
                    (None, (sourceTags sourceMeta sourceMeta.Headers columns contentIndices) )
            | None ->
                (None, (sourceTags sourceMeta sourceMeta.Headers columns contentIndices) )

    type FullNameBuilderParms =
        { 
        FirstIndex : int option
        MiddleIndex : int list
        FamilyIndex : int option
        NameOrder : NameOrder
        }

    let fullNameBuilder (fullNameBuilderParms : FullNameBuilderParms) (sourceMeta : ImportSourceMeta) =
        fun (columns : string []) ->
            let contentIndices =
                fullNameBuilderParms.MiddleIndex @
                   ([fullNameBuilderParms.FirstIndex;
                    fullNameBuilderParms.FamilyIndex]
                    |> List.choose id)

            let middle: string list =
                fullNameBuilderParms.MiddleIndex
                |> List.choose (fun i -> valueFromIndexOpt columns <| Some i)
                
            let first = valueFromIndexOpt columns fullNameBuilderParms.FirstIndex
            let family = valueFromIndexOpt columns fullNameBuilderParms.FamilyIndex
            let nameOrder = fullNameBuilderParms.NameOrder

            match headerSources sourceMeta.PrimaryName sourceMeta.TimeStamp sourceMeta.Headers contentIndices with
            | Some sources ->
                match FullName.TryParse (first, middle, family, nameOrder, Set.empty, sources) with
                | Some fullName ->
                    (Some fullName, Set.empty)
                | None ->
                    (None, (sourceTags sourceMeta sourceMeta.Headers columns contentIndices) )
            | None ->
                (None, (sourceTags sourceMeta sourceMeta.Headers columns contentIndices) )

    let headerOffsets (headers : string []) (targets : string []) =
        headers
        |> Array.mapi (fun i header ->
            if  targets
                |> Array.exists (fun x -> header.ToLower().Contains(x.ToLower()) )
            then
                Some i
            else
                None
            )
        |> Array.choose id
        
    let rawToFinalResult transform (rawResult : 'a option * Set<Tag>) =
        match rawResult with
        | (Some x), tags -> (Some <| transform x), tags
        | None, tags -> None, tags

    let fullNameBuilderParms headers (synonyms : ContactSynonyms) =
        { 
        FirstIndex =
            match headerOffsets headers synonyms.FullNameFirstName |> Array.toList with
            | hd::_ -> Some hd
            | [] -> None
        MiddleIndex = []
        FamilyIndex =
            match headerOffsets headers synonyms.FullNameLastName |> Array.toList with
            | hd::_ -> Some hd
            | [] -> None
        NameOrder = NameOrder.Western
        }

    let fullNameBuilders fullNameBuilderParms sourceMeta =
        [fullNameBuilder fullNameBuilderParms sourceMeta >> rawToFinalResult ContactName.FullName]

    let headersNotExluded synonyms excludes headers =
        headers
        |> List.filter (fun n -> (List.exists (fun x -> x = n) excludes) |> not)

    let physicalAddressBuilderParms synonyms headers  =
        let cityList = headerOffsets headers synonyms.PhysicalCity |> Array.toList
        let stateList = headerOffsets headers synonyms.PhysicalState |> Array.toList
        let postalCodeList = headerOffsets headers synonyms.PhysicalPostalCode |> Array.toList
        let countryList = headerOffsets headers synonyms.PhysicalCountry |> Array.toList

        let optIndex intList =
            match intList with
            | hd::_ -> Some hd
            | [] -> None

        let excludes =
            (headerOffsets headers synonyms.Email)
            |> Array.toList
            |> List.append ([cityList; stateList; postalCodeList; countryList] |> List.concat )
            |> List.distinct

        let addressIndex = 
            headerOffsets headers synonyms.PhysicalAddress
            |> Array.filter (fun x ->
                List.exists (fun x' -> x' = x) excludes
                |> not
                )
            |> Array.toList
            |> headersNotExluded synonyms excludes

        {
        AddressIndex = addressIndex
        CityIndex = optIndex cityList 
        StateIndex = optIndex stateList
        PostalCodeIndex = optIndex postalCodeList
        CountryIndex =  optIndex countryList
        }

    let physicalAddressBuilders physicalAddressBuilderParms sourceMeta =
        [physicalAddressBuilder physicalAddressBuilderParms sourceMeta >> rawToFinalResult Address.PhysicalAddress]

    //email, phone, uri, contact name
    let simpleEntityBuilder (sourceMeta : ImportSourceMeta) (tryParse : string * Set<Tag> * NonEmptySet<Source> -> 'a option) displ =
        fun (columns : string []) ->
            if String.IsNullOrWhiteSpace columns.[displ] then
                (None, Set.empty)
            else
                match headerSources sourceMeta.PrimaryName sourceMeta.TimeStamp sourceMeta.Headers [displ] with
                | Some sources ->
                    match tryParse (columns.[displ], Set.empty, sources) with
                    | Some entity ->
                        (Some entity, Set.empty)
                    | None ->
                        (None, (sourceTags sourceMeta sourceMeta.Headers columns [displ]) )
                | None ->
                    (None, (sourceTags sourceMeta sourceMeta.Headers columns [displ]) )
                     
    let entityBuilders sourceMeta tryParse coveredHeaderColumns entityCstr =
        let builders =
            coveredHeaderColumns
            |> Array.fold (fun s i -> 
                (simpleEntityBuilder sourceMeta tryParse i >> rawToFinalResult entityCstr)::s ) []

        builders, coveredHeaderColumns

    let getAddressBuilders sourceMeta synonyms =
        let physicalAddressBuilderParms = physicalAddressBuilderParms synonyms sourceMeta.Headers

        let usedPhysicalAddressHeaderColumns = 
            [physicalAddressBuilderParms.CityIndex;
                physicalAddressBuilderParms.StateIndex;
                physicalAddressBuilderParms.PostalCodeIndex;
                physicalAddressBuilderParms.CountryIndex]
            |> List.choose id
            |> List.append physicalAddressBuilderParms.AddressIndex
            |> List.toArray

        let builders, usedHeaderColumns = 
            [|entityBuilders sourceMeta PhoneNumber.TryParse (headerOffsets sourceMeta.Headers synonyms.PhoneNumber) Address.PhoneNumber;
            entityBuilders sourceMeta EmailAddress.TryParse (headerOffsets sourceMeta.Headers synonyms.Email) Address.EmailAddress; 
            entityBuilders sourceMeta UriTagged.TryParse (headerOffsets sourceMeta.Headers synonyms.Uri) Address.Url; 
            (physicalAddressBuilders physicalAddressBuilderParms sourceMeta), usedPhysicalAddressHeaderColumns;|]
            |> Array.unzip

        (builders |> List.concat), (usedHeaderColumns |> Array.concat)
        
    let getNameBuilders sourceMeta synonyms =
        let fullNameBuilderParms = fullNameBuilderParms sourceMeta.Headers synonyms

        let usedNameHeaderColumns = 
            [fullNameBuilderParms.FirstIndex;
                fullNameBuilderParms.FamilyIndex;]
            |> List.choose id
            |> List.append fullNameBuilderParms.MiddleIndex
            |> List.toArray

        let namesOtherThanFullName = 
            (headerOffsets sourceMeta.Headers synonyms.SimpleName)
            |> List.ofArray
            |> headersNotExluded synonyms (usedNameHeaderColumns |> List.ofArray)
            |> Array.ofList

        let builders, usedHeaderColumns = 
            [|entityBuilders sourceMeta SimpleName.TryParse namesOtherThanFullName ContactName.SimpleName; 
            (fullNameBuilders fullNameBuilderParms sourceMeta), usedNameHeaderColumns|]
            |> Array.unzip

        (builders |> List.concat), (usedHeaderColumns |> Array.concat)

    let commonBuilders sourceMeta =
        let synonyms =
            {
            PhoneNumber = [|"phone"; "fax"; "pager"; "mobile"; "cell"; "telephone";|]
            Email = [|"email"; "e-mail"; "emailaddress"|]
            Uri = [|"web";|]
            SimpleName = [|"display name"; "nickname"; "screen name"; "name";|]
            FullNameFirstName = [|"first name"; "given name"; "firstname"|]
            FullNameLastName = [|"last name"; "family name"; "lastname"|]
            PhysicalAddress = [|"address"|]
            PhysicalCity = [|"city"|]
            PhysicalState = [|"state"|]
            PhysicalPostalCode = [|"zip"; "zipcode"; "postal code"|]
            PhysicalCountry = [|"country"|]
            }

        let addressBuilders, usedAddressColumns = getAddressBuilders sourceMeta synonyms
        let nameBuilders, usedNameColumns = getNameBuilders sourceMeta synonyms

        let usedColumns =
            Array.concat [|usedAddressColumns; usedNameColumns|]
            |> Array.distinct
            
        let unUsedColumns = 
            sourceMeta.Headers
            |> Array.mapi (fun i _ ->
                if Array.exists (fun t -> i = t) usedColumns then
                    None
                else Some i)
            |> Array.choose id

        nameBuilders, addressBuilders, unUsedColumns

    let contactImport sources sourceBuilder nameBuilders addressBuilders =
        sources
        |> Seq.choose (fun source ->
            let columns = sourceBuilder source

            let objectsAndTags builders =
                ([], builders)
                ||> Seq.fold (fun s f -> (f columns)::s )
                |> List.unzip

            let contactNames, nameTags =
                objectsAndTags nameBuilders

            let addressOpts, addressTags =
                objectsAndTags addressBuilders

            let tagSet = 
                [nameTags; addressTags]
                |> List.concat
                |> List.collect Set.toList 
                |> Set.ofList
   
            let names = contactNames |> List.choose id
            let addresses = addressOpts |> List.choose id

            match names, addresses, tagSet with
            | [], [], s when s.IsEmpty -> None
            | _ ->
                {Names = names |> ContactName.elimination |> Set.ofList
                 Addresses = addresses |> Address.elimination |> Set.ofList
                 Tags = tagSet
                 } |> Some
            )
