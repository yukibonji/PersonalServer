namespace Jackfoxy.PersonalServer

open System

module ContactImport =

    type ImportSourceMeta =
        {
        PrimaryName : TrimNonEmptyString
        TimeStamp : DateTime
        Headers : string []
        }

    //to do: eventually resource file
    let phoneNumberSynonyms = [|"phone"; "fax"; "pager"; "mobile"; "cell"; "telephone";|]

    let emailSynonyms = [|"email"; "e-mail"; "emailaddress"|]

    let uriSynonyms = [|"web";|]

    let simpleNameSynonyms = [|"display name"; "nickname"; "screen name"; "name";|]

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

    let sourceTags source (headers : string []) (columns : string []) (contentIndices : int seq) =
        contentIndices
        |> Seq.choose (fun index -> 
            match Tag.TryParse columns.[index] with
            | Some _ ->
                Tag.TryParse <| sprintf "%s::%s::%s" source headers.[index] columns.[index]
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
                    (None, (sourceTags sourceMeta.PrimaryName.Value sourceMeta.Headers columns contentIndices) )
            | None ->
                (None, (sourceTags sourceMeta.PrimaryName.Value sourceMeta.Headers columns contentIndices) )

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
                    (None, (sourceTags sourceMeta.PrimaryName.Value sourceMeta.Headers columns contentIndices) )
            | None ->
                (None, (sourceTags sourceMeta.PrimaryName.Value sourceMeta.Headers columns contentIndices) )

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

        //to do: complete first/last name coverage (i.e. potential multiples)
        //to do: eventually resource file
    let fullNameFirstNameSynonyms = [|"first name"; "given name"; "firstname"|]
    let fullNameLastNameSynonyms = [|"last name"; "family name"; "lastname"|]

    let fullNameBuilderParms headers =
        { 
        FirstIndex =
            match headerOffsets headers fullNameFirstNameSynonyms |> Array.toList with
            | hd::_ -> Some hd
            | [] -> None
        MiddleIndex = []
        FamilyIndex =
            match headerOffsets headers fullNameLastNameSynonyms |> Array.toList with
            | hd::_ -> Some hd
            | [] -> None
        NameOrder = NameOrder.Western
        }

    let fullNameBuilders fullNameBuilderParms sourceMeta =
        [fullNameBuilder fullNameBuilderParms sourceMeta >> rawToFinalResult ContactName.FullName]

    //to do: eventually resource file
    let physicalAddressAddressSynonyms = [|"address"|]
    let physicalAddressCitySynonyms = [|"city"|]
    let physicalAddressStateSynonyms = [|"state"|]
    let physicalAddressPostalCodeSynonyms = [|"zip"; "zipcode"; "postal code"|]
    let physicalAddressCountrySynonyms = [|"country"|]

    //to do: complete address coverage (i.e. potential multiples)
    let headersNotExluded excludes headers =
        headers
        |> List.filter (fun n -> (List.exists (fun x -> x = n) excludes) |> not)

    let physicalAddressBuilderParms headers =
        let cityList = headerOffsets headers physicalAddressCitySynonyms |> Array.toList
        let stateList = headerOffsets headers physicalAddressStateSynonyms |> Array.toList
        let postalCodeList = headerOffsets headers physicalAddressPostalCodeSynonyms |> Array.toList
        let countryList = headerOffsets headers physicalAddressCountrySynonyms |> Array.toList

        let cityIndex =
            match cityList with
            | hd::_ -> Some hd
            | [] -> None
        let stateIndex =
            match stateList with
            | hd::_ -> Some hd
            | [] -> None
        let postalCodeIndex =
            match postalCodeList with
            | hd::_ -> Some hd
            | [] -> None
        let countryIndex = 
            match countryList with
            | hd::_ -> Some hd
            | [] -> None

        let excludes =
            (headerOffsets headers emailSynonyms)
            |> Array.toList
            |> List.append ([cityList; stateList; postalCodeList; countryList] |> List.concat )
            |> List.distinct

        let addressIndex = 
            headerOffsets headers physicalAddressAddressSynonyms 
            |> Array.filter (fun x ->
                List.exists (fun x' -> x' = x) excludes
                |> not
                )
            |> Array.toList
            |> headersNotExluded excludes

        {
        AddressIndex = addressIndex
        CityIndex = cityIndex
        StateIndex = stateIndex
        PostalCodeIndex = postalCodeIndex
        CountryIndex = countryIndex
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
                        (None, (sourceTags sourceMeta.PrimaryName.Value sourceMeta.Headers columns [displ]) )
                | None ->
                    (None, (sourceTags sourceMeta.PrimaryName.Value sourceMeta.Headers columns [displ]) )
                     
    let entityBuilders sourceMeta tryParse coveredHeaderColumns entityCstr =
        let builders =
            coveredHeaderColumns
            |> Array.fold (fun s i -> 
                (simpleEntityBuilder sourceMeta tryParse i >> rawToFinalResult entityCstr)::s ) []

        builders, coveredHeaderColumns

    let getAddressBuilders sourceMeta =
        let physicalAddressBuilderParms = physicalAddressBuilderParms sourceMeta.Headers

        let usedPhysicalAddressHeaderColumns = 
            [physicalAddressBuilderParms.CityIndex;
                physicalAddressBuilderParms.StateIndex;
                physicalAddressBuilderParms.PostalCodeIndex;
                physicalAddressBuilderParms.CountryIndex]
            |> List.choose id
            |> List.append physicalAddressBuilderParms.AddressIndex
            |> List.toArray

        let builders, usedHeaderColumns = 
            [|entityBuilders sourceMeta PhoneNumber.TryParse (headerOffsets sourceMeta.Headers phoneNumberSynonyms) Address.PhoneNumber;
            entityBuilders sourceMeta EmailAddress.TryParse (headerOffsets sourceMeta.Headers emailSynonyms) Address.EmailAddress; 
            entityBuilders sourceMeta UriTagged.TryParse (headerOffsets sourceMeta.Headers uriSynonyms) Address.Url; 
            (physicalAddressBuilders physicalAddressBuilderParms sourceMeta), usedPhysicalAddressHeaderColumns;|]
            |> Array.unzip

        (builders |> List.concat), (usedHeaderColumns |> Array.concat)
        
    let getNameBuilders sourceMeta =
        let fullNameBuilderParms = fullNameBuilderParms sourceMeta.Headers

        let usedNameHeaderColumns = 
            [fullNameBuilderParms.FirstIndex;
                fullNameBuilderParms.FamilyIndex;]
            |> List.choose id
            |> List.append fullNameBuilderParms.MiddleIndex
            |> List.toArray

        let namesOtherThanFullName = 
            (headerOffsets sourceMeta.Headers simpleNameSynonyms)
            |> List.ofArray
            |> headersNotExluded (usedNameHeaderColumns |> List.ofArray)
            |> Array.ofList

        let builders, usedHeaderColumns = 
            [|entityBuilders sourceMeta SimpleName.TryParse namesOtherThanFullName ContactName.SimpleName; 
            (fullNameBuilders fullNameBuilderParms sourceMeta), usedNameHeaderColumns|]
            |> Array.unzip

        (builders |> List.concat), (usedHeaderColumns |> Array.concat)

    let commonBuilders sourceMeta =
        let addressBuilders, usedAddressColumns = getAddressBuilders sourceMeta
        let nameBuilders, usedNameColumns = getNameBuilders sourceMeta

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
            let namesAndTags =
                ([], nameBuilders)
                ||> Seq.fold (fun s f -> (f columns)::s )
            let addressesAndTags =
                ([], addressBuilders)
                ||> Seq.fold (fun s f -> (f columns)::s )

            let contactNames, nameTags =
                namesAndTags
                |> List.unzip

            let addressOpts, addressTags =
                addressesAndTags
                |> List.unzip

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
