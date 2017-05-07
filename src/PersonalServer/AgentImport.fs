namespace Jackfoxy.PersonalServer

open System
open FSharp.Data

module AgentImport =

    //to do: eventually resource file
    let phoneNumberSynonyms = [|"phone"; "fax"; "pager"; "mobile"; "cell"; "telephone";|]

    let emailSynonyms = [|"email"; "e-mail";|]

    let uriSynonyms = [|"web";|]

    let personNameSynonyms = [|"display name"; "nickname"; "screen name";|]

    let fullNameFirstNameSynonyms = [|"First Name"|]
    let fullNameLastNameSynonyms = [|"Last Name"|]

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
            Tag.TryParse <| sprintf "%s::%s" source headers.[index])
        |> Seq.choose id
        |> Set.ofSeq

    let sourceTags source (headers : string []) (columns : string []) (contentIndices : int seq) =
        contentIndices
        |> Seq.map (fun index -> 
            Tag.TryParse <| sprintf "%s::%s::%s" source headers.[index] columns.[index])
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
                |> Array.exists (fun x -> header.ToLower().Contains (x) )
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

    let entityBuilders indices source headers tryParse entityCstr =
        indices
        |> Array.fold (fun s i -> 
            (simpleEntityBuilder tryParse i source headers >> rawToFinalResult entityCstr)::s ) []
        
    let importCsv source (path : string) =
        let importFile = CsvFile.Load(path).Cache()
        let headers = importFile.Headers.Value

        let phoneNumberBuilders =
            entityBuilders (headerOffsets headers phoneNumberSynonyms) 
                source headers PhoneNumber.TryParse Address.PhoneNumber

        let emailAddressBuilders =
            entityBuilders (headerOffsets headers emailSynonyms) 
                source headers EmailAddress.TryParse Address.EmailAddress

        let uriBuilders =
            entityBuilders (headerOffsets headers uriSynonyms) 
                source headers Uri.TryParse Address.Url

        let personNameBuilders =
            entityBuilders (headerOffsets headers personNameSynonyms) 
                source headers PersonName.TryParse NameOfPerson.Name

        //to do: complete first/last name coverage (i.e. potential multiples)
        let fullNameBuilderParms =
            { 
            FirstIndex =
                match headerOffsets headers fullNameFirstNameSynonyms |> Array.toList with
                | hd::tl -> Some hd
                | [] -> None
            MiddleIndex = []
            FamilyIndex =
                match headerOffsets headers fullNameLastNameSynonyms |> Array.toList with
                | hd::tl -> Some hd
                | [] -> None
            NameOrder = NameOrder.Western
            }

        let fullNameBuilders =
            [fullNameBuilder fullNameBuilderParms source headers >> rawToFinalResult NameOfPerson.FullName]

        //to do: eventually resource file
        let physicalAddressAddressSynonyms = [|"address"|]
        let physicalAddressCitySynonyms = [|"city"|]
        let physicalAddressStateSynonyms = [|"state"|]
        let physicalAddressPostalCodeSynonyms = [|"zip"; "zipcode"|]
        let physicalAddressCountrySynonyms = [|"country"|]

        //to do: complete address coverage (i.e. potential multiples)
        let physicalAddressBuilderParms =
            {
            AddressIndex = headerOffsets headers physicalAddressAddressSynonyms |> Array.toList
            CityIndex =
                match headerOffsets headers physicalAddressCitySynonyms |> Array.toList with
                | hd::tl -> Some hd
                | [] -> None
            StateIndex =
                match headerOffsets headers physicalAddressStateSynonyms |> Array.toList with
                | hd::tl -> Some hd
                | [] -> None
            PostalCodeIndex =
                match headerOffsets headers physicalAddressPostalCodeSynonyms |> Array.toList with
                | hd::tl -> Some hd
                | [] -> None
            CountryIndex =
                match headerOffsets headers physicalAddressCountrySynonyms |> Array.toList with
                | hd::tl -> Some hd
                | [] -> None
            }

        let physicalAddressBuilders =
            [physicalAddressBuilder physicalAddressBuilderParms source headers >> rawToFinalResult Address.PhysicalAddress]
        ()