namespace Jackfoxy.PersonalServer

open System

module AgentImport =

    //to do: eventually resource file
    let phoneNumberSynonyms = [|"phone"; "fax"; "pager"; "mobile"; "cell"; "telephone";|]

    let emailSynonyms = [|"email"; "e-mail";|]

    let uriSynonyms = [|"web";|]

    let personNameSynonyms = [|"display name"; "nickname"; "screen name"; "name";|]

    let valueFromIndexOpt (columns : string []) indexOpt = 
        match indexOpt with
        | Some i -> 
            if String.IsNullOrWhiteSpace columns.[i] then
                None
            else
                Some columns.[i]
        | None ->
            None

    let sourceHeaderTags source (headers : string []) (contentIndices : int seq) =
        contentIndices
        |> Seq.map (fun index -> 
            match Tag.TryParse headers.[index] with
            | Some _ ->
                Tag.TryParse <| sprintf "%s::%s" source headers.[index]
            | None ->
                None )
        |> Seq.choose id
        |> Set.ofSeq

    let sourceTags source (headers : string []) (columns : string []) (contentIndices : int seq) =
        contentIndices
        |> Seq.map (fun index -> 
            match Tag.TryParse columns.[index] with
            | Some _ ->
                Tag.TryParse <| sprintf "%s::%s::%s" source headers.[index] columns.[index]
            | None ->
                None )
        |> Seq.choose id
        |> Set.ofSeq

    //email, phone, uri, person name
    let simpleEntityBuilder (tryParse : string * Set<Tag> -> 'a option) displ source (headers : string []) =
        fun (columns : string []) ->
            if String.IsNullOrWhiteSpace columns.[displ] then
                (None, Set.empty)
            else
                let tags = sourceHeaderTags source headers [displ]
                match tryParse (columns.[displ], tags) with
                | Some entity ->
                    (Some entity, Set.empty)
                | None ->
                    (None, (sourceTags source headers columns [displ]) )

    type PhysicalAddressBuilderParms =
        {
        AddressIndex : int list
        CityIndex : int option
        StateIndex : int option
        PostalCodeIndex : int option
        CountryIndex : int option
        }

    let physicalAddressBuilder (physicalAddressBuilderParms : PhysicalAddressBuilderParms) source (headers : string []) =
        fun (columns : string []) ->
            let contentIndices =
                physicalAddressBuilderParms.AddressIndex @
                   ([physicalAddressBuilderParms.CityIndex;
                    physicalAddressBuilderParms.StateIndex;
                    physicalAddressBuilderParms.PostalCodeIndex;
                    physicalAddressBuilderParms.CountryIndex]
                    |> List.choose id)
                
            let tags = sourceHeaderTags source headers contentIndices

            let streetAddress: string list =
                physicalAddressBuilderParms.AddressIndex
                |> List.map (fun i -> valueFromIndexOpt columns <| Some i)
                |> List.choose id
                
            let city = valueFromIndexOpt columns physicalAddressBuilderParms.CityIndex
            let state = valueFromIndexOpt columns physicalAddressBuilderParms.StateIndex
            let postalCode = valueFromIndexOpt columns physicalAddressBuilderParms.PostalCodeIndex
            let country = valueFromIndexOpt columns physicalAddressBuilderParms.CountryIndex

            match PhysicalAddress.TryParse (streetAddress, city, state, postalCode, country, tags) with
            | Some physicalAddress ->
                (Some physicalAddress, Set.empty)
            | None ->
                (None, (sourceTags source headers columns contentIndices) )

    type FullNameBuilderParms =
        { 
        FirstIndex : int option
        MiddleIndex : int list
        FamilyIndex : int option
        NameOrder : NameOrder
        }

    let fullNameBuilder (fullNameBuilderParms : FullNameBuilderParms) source (headers : string []) =
        fun (columns : string []) ->
            let contentIndices =
                fullNameBuilderParms.MiddleIndex @
                   ([fullNameBuilderParms.FirstIndex;
                    fullNameBuilderParms.FamilyIndex]
                    |> List.choose id)
                
            let tags = sourceHeaderTags source headers contentIndices

            let middle: string list =
                fullNameBuilderParms.MiddleIndex
                |> List.map (fun i -> valueFromIndexOpt columns <| Some i)
                |> List.choose id
                
            let first = valueFromIndexOpt columns fullNameBuilderParms.FirstIndex
            let family = valueFromIndexOpt columns fullNameBuilderParms.FamilyIndex
            let nameOrder = fullNameBuilderParms.NameOrder

            match FullName.TryParse (first, middle, family, nameOrder, tags) with
            | Some physicalAddress ->
                (Some physicalAddress, Set.empty)
            | None ->
                (None, (sourceTags source headers columns contentIndices) )

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

    let entityBuilders source headers coveredHeaderColumns tryParse entityCstr =
        let builders =
            coveredHeaderColumns
            |> Array.fold (fun s i -> 
                (simpleEntityBuilder tryParse i source headers >> rawToFinalResult entityCstr)::s ) []

        builders, coveredHeaderColumns

        //to do: complete first/last name coverage (i.e. potential multiples)
        //to do: eventually resource file
    let fullNameFirstNameSynonyms = [|"first name"; "given name"|]
    let fullNameLastNameSynonyms = [|"last name"; "family name"|]

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

    let fullNameBuilders fullNameBuilderParms source headers =
        [fullNameBuilder fullNameBuilderParms source headers >> rawToFinalResult NameOfPerson.FullName]

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
            [cityList; stateList; postalCodeList; countryList]
            |> List.concat
            |> List.distinct

        let addressIndex = 
            headerOffsets headers physicalAddressAddressSynonyms |> Array.toList
            |> headersNotExluded excludes

        {
        AddressIndex = addressIndex
        CityIndex = cityIndex
        StateIndex = stateIndex
        PostalCodeIndex = postalCodeIndex
        CountryIndex = countryIndex
        }

    let physicalAddressBuilders physicalAddressBuilderParms source headers =
        [physicalAddressBuilder physicalAddressBuilderParms source headers >> rawToFinalResult Address.PhysicalAddress]

    let getAddressBuilders source headers =
        let physicalAddressBuilderParms = physicalAddressBuilderParms headers

        let usedPhysicalAddressHeaderColumns = 
            [physicalAddressBuilderParms.CityIndex;
                physicalAddressBuilderParms.StateIndex;
                physicalAddressBuilderParms.PostalCodeIndex;
                physicalAddressBuilderParms.CountryIndex]
            |> List.choose id
            |> List.append physicalAddressBuilderParms.AddressIndex
            |> List.toArray

        let builders, usedHeaderColumns = 
            [|entityBuilders source headers (headerOffsets headers phoneNumberSynonyms) PhoneNumber.TryParse Address.PhoneNumber;
            entityBuilders source headers (headerOffsets headers emailSynonyms) EmailAddress.TryParse Address.EmailAddress; 
            entityBuilders source headers (headerOffsets headers uriSynonyms) UriTagged.TryParse Address.Url; 
            (physicalAddressBuilders physicalAddressBuilderParms source headers), usedPhysicalAddressHeaderColumns;|]
            |> Array.unzip

        (builders |> List.concat), (usedHeaderColumns |> Array.concat)
        
    let getNameBuilders source headers =
        let fullNameBuilderParms = fullNameBuilderParms headers

        let usedNameHeaderColumns = 
            [fullNameBuilderParms.FirstIndex;
                fullNameBuilderParms.FamilyIndex;]
            |> List.choose id
            |> List.append fullNameBuilderParms.MiddleIndex
            |> List.toArray

        let namesOtherThanFullName = 
            (headerOffsets headers personNameSynonyms)
            |> List.ofArray
            |> headersNotExluded (usedNameHeaderColumns |> List.ofArray)
            |> Array.ofList

        let builders, usedHeaderColumns = 
            [|entityBuilders source headers namesOtherThanFullName PersonName.TryParse NameOfPerson.Name; 
            (fullNameBuilders fullNameBuilderParms source headers), usedNameHeaderColumns|]
            |> Array.unzip

        (builders |> List.concat), (usedHeaderColumns |> Array.concat)

    let agentImport sources sourceBuilder nameBuilders addressBuilders =
        sources
        |> Seq.map (fun source ->
            let columns = sourceBuilder source
            let names =
                ([], nameBuilders)
                ||> Seq.fold (fun s f -> (f columns)::s )
            let addresses =
                ([], addressBuilders)
                ||> Seq.fold (fun s f -> (f columns)::s )

            let nameOfPersons, nameTags =
                names
                |> List.unzip

            let addressesOpts, addressTags =
                addresses
                |> List.unzip

            let tagSet = 
                [nameTags; addressTags]
                |> List.concat
                |> List.collect Set.toList 
                |> Set.ofList
   
            let names = nameOfPersons |> List.choose id
            let addresses = addressesOpts |> List.choose id
            match names, addresses, tagSet with
            | [], [], s when s.IsEmpty -> None
            | _ ->

                {Names = names |> Set.ofList
                 Addresses = addresses |> Set.ofList
                 Tags = tagSet
                 } |> Some
            )
        |> Seq.choose id

    let commonBuilders source headers =
        let addressBuilders, usedAddressColumns = getAddressBuilders source headers
        let nameBuilders, usedNameColumns = getNameBuilders source headers

        let usedColumns =
            Array.concat [|usedAddressColumns; usedNameColumns|]
            |> Array.distinct
            
        let unUsedColumns = 
            headers
            |> Array.mapi (fun i _ ->
                if Array.exists (fun t -> i = t) usedColumns then
                    None
                else Some i)
            |> Array.choose id

        nameBuilders, addressBuilders, unUsedColumns
