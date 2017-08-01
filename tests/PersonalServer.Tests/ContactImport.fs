namespace PersonalServer.Tests

open DomainGeneratorsCode
open Jackfoxy.PersonalServer
open ContactImport
open Expecto
open FsCheck
open System

module ContactImport =
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000}

    [<Tests>]
    let simpleEntityBuilder =

        let testSimpleEntity input tryParse =
            let t1 = tryParse (input, Set.empty<Tag>, Sources.sourceSet)

            let columns = [|"a"; input; ""|]

            let sourceMeta =
                {
                PrimaryName = TrimNonEmptyString.Parse ["test"] |> List.head
                TimeStamp = DateTime.UtcNow
                Headers = [|"doodle"; "web"; "doodle2";|]
                }

            let result = Jackfoxy.PersonalServer.ContactImport.simpleEntityBuilder sourceMeta tryParse 1 columns

            match t1 with 
            | Some _ -> 
                let sources = (Set.singleton (Source.TryParse ("test", Some "web", DateTime.UtcNow, DateTime.UtcNow)).Value |> NonEmptySet.TryParse).Value
                (fst result) = tryParse (input, Set.empty, sources)
            | None ->                        
                (snd result) = Set.singleton (Tag.TryParse ((sprintf "test::web::%s" input), Sources.sourceSet)).Value
            
        testList "ContactImport.SimpleEntityBuilder" [
            testPropertyWithConfig config10k "PhoneNumber equality" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genPhoneNumber())
                        (fun  (_, _, _, phoneNumber) ->
                            testSimpleEntity phoneNumber PhoneNumber.TryParse )

            testPropertyWithConfig config10k "EmailAddress equality" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun  (emailAddress) ->
                            testSimpleEntity emailAddress EmailAddress.TryParse )

            testPropertyWithConfig config10k "UriTagged equality" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| genUriTagged())
                        (fun  (uri) ->
                            testSimpleEntity uri UriTagged.TryParse )

            testPropertyWithConfig config10k "SimpleName equality" <|
                fun  () ->
                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
                        (fun  (simpleName) ->
                            testSimpleEntity simpleName SimpleName.TryParse )

        ]

    [<Tests>]
    let fullNameBuilder =
        testList "ContactImport.FullNameBuilder" [
//            testPropertyWithConfig config10k "equality" <|
//                fun  () ->
//                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
//                        (fun  (simpleName) ->
//                            testSimpleEntity simpleName ContactName.TryParse )
        ]

//        let fullNameBuilders =
//            [fullNameBuilder fullNameBuilderParms source headers >> rawToFinalResult NameOfPerson.FullName]

    [<Tests>]
    let physicalAddressBuilder =
        testList "ContactImport.PhysicalAddressBuilder" [
        ]
//        let physicalAddressBuilders =
//            [physicalAddressBuilder physicalAddressBuilderParms source headers >> rawToFinalResult Address.PhysicalAddress]

    [<Tests>]
    let contactElimination =

        let nameElim (names : ContactName list) expectToElim =
            let expectedLength = names.Length - expectToElim

            List.permutations names
            |> Seq.iteri (fun i x ->
                let res = ContactName.elimination x
                Expect.isTrue (res.Length = expectedLength) (sprintf "Expected perumutation %i to be %i, but was %i" i  expectedLength res.Length) )

        testList "ContactImport.ContactElimination" [
            testCase "simple name elim1" <| fun () ->
                nameElim Contacts.simpleNameElim1 1 

            testCase "simple name elim2" <| fun () ->
                nameElim Contacts.simpleNameElim2 1

            testCase "simple name elim3" <| fun () ->
                nameElim Contacts.simpleNameElim3 1

            testCase "simple name elim4" <| fun () ->
                nameElim Contacts.simpleNameElim4 1

            testCase "simple name elim5" <| fun () ->
                nameElim Contacts.simpleNameElim5 1

            testCase "simple name elim6" <| fun () ->
                nameElim Contacts.simpleNameElim6 2

            testCase "fullname elim1" <| fun () ->
                nameElim Contacts.fullNameElim1 1

            testCase "NameAndAffixes elim1" <| fun () ->
                nameElim Contacts.nameAndAffixesElim1 2

            testCase "NameAndAffixes elim2" <| fun () ->
                nameElim Contacts.nameAndAffixesElim2 3
        ]

    [<Tests>]
    let contactEliminationTagMerge =
        testList "ContactImport.ContactEliminationTagMerge" [
            testCase "simple name tag merge" <| fun () ->
                List.permutations Contacts.simpleNameTagMerge
                |> Seq.iteri (fun i x ->
                    let res = ContactName.elimination x
                    let simpleName = match res.Head with | ContactName.SimpleName x -> x | _ -> invalidArg "can't" "get here"
                    Expect.isTrue (simpleName.Tags = Tags.tagSet1) (sprintf "Expected perumutation %i to match tagSet1" i) )

            testCase "fullname tag merge" <| fun () ->
                List.permutations Contacts.fullNameTagMerge
                |> Seq.iteri (fun i x ->
                    let res = ContactName.elimination x
                    let fullName = match res.Head with | ContactName.FullName x -> x | _ -> invalidArg "can't" "get here"
                    Expect.isTrue (fullName.Tags = Tags.tagSet2) (sprintf "Expected perumutation %i to match tagSet2" i) )

            testCase "NameAndAffixes tag merge" <| fun () ->
                List.permutations Contacts.nameAndAffixesTagMerge
                |> Seq.iteri (fun i x ->
                    let res = ContactName.elimination x
                    let nameAndAffixes = match res.Head with | ContactName.NameAndAffixes x -> x | _ -> invalidArg "can't" "get here"
                    Expect.isTrue (nameAndAffixes.SimpleName.Tags = Tags.tagSet3) (sprintf "Expected perumutation %i to match tagSet3" i) )
        ]

    [<Tests>]
    let addressElimination =

        let addressElim (addresses : Address list) expectToElim =
            let expectedLength = addresses.Length - expectToElim

            let test i x =
                let res = Address.elimination x
                let resA = Array.ofList res
                Expect.isTrue (res.Length = expectedLength) (sprintf "Expected perumutation %i to be %i, but was %i" i  expectedLength res.Length) 
                
            List.permutations addresses
            |> Seq.iteri (fun i x -> test i x)

        testList "ContactImport.AddressElimination" [
            testCase "address elim physical 1" <| fun () ->
                addressElim Addresses.physicalElim1 1 

            testCase "address elim physical 2" <| fun () ->
                addressElim Addresses.physicalElim2 4 

            testCase "address elim email 1" <| fun () ->
                addressElim Addresses.emailElim1 1

            testCase "address elim phone number 1" <| fun () ->
                addressElim Addresses.phoneNumberElim1 3

            testCase "address elim uri 1" <| fun () ->
                addressElim Addresses.uriElim1 1

            testCase "address elim handle 1" <| fun () ->
                addressElim Addresses.handleElim1 1

            testCase "address elim half all 1" <| fun () ->
                addressElim Addresses.halfAllElim1 3

            testCase "address elim half all 2" <| fun () ->
                addressElim Addresses.halfAllElim2 3
        ]

    [<Tests>]
    let addressEliminationTagMerge =
        testList "AddressImport.AddressEliminationTagMerge" [
            testCase "physical address tag merge" <| fun () ->
                List.permutations Addresses.physicalAddressTagMerge
                |> Seq.iteri (fun i x ->
                    let res = Address.elimination x
                    let physicalAddress = match res.Head with | Address.PhysicalAddress x -> x | _ -> invalidArg "can't" "get here"
                    Expect.isTrue (physicalAddress.Tags = Tags.tagSet1) (sprintf "Expected perumutation %i to match tagSet1" i) )

            testCase "email tag merge" <| fun () ->
                List.permutations Addresses.emailTagMerge
                |> Seq.iteri (fun i x ->
                    let res = Address.elimination x
                    let emailAddress = match res.Head with | Address.EmailAddress x -> x | _ -> invalidArg "can't" "get here"
                    Expect.isTrue (emailAddress.Tags = Tags.tagSet3) (sprintf "Expected perumutation %i to match tagSet3" i) )

            testCase "phone number tag merge" <| fun () ->
                List.permutations Addresses.phoneNumberTagMerge
                |> Seq.iteri (fun i x ->
                    let res = Address.elimination x
                    let phoneNumber = match res.Head with | Address.PhoneNumber x -> x | _ -> invalidArg "can't" "get here"
                    Expect.isTrue (phoneNumber.Tags = Tags.tagSet2) (sprintf "Expected perumutation %i to match tagSet2" i) )

            testCase "uri tag merge" <| fun () ->
                List.permutations Addresses.uriTagMerge
                |> Seq.iteri (fun i x ->
                    let res = Address.elimination x
                    let uri = match res.Head with | Address.Url x -> x | _ -> invalidArg "can't" "get here"
                    Expect.isTrue (uri.Tags = Tags.tagSet1) (sprintf "Expected perumutation %i to match tagSet1" i) )

            testCase "handle tag merge" <| fun () ->
                List.permutations Addresses.handleTagMerge
                |> Seq.iteri (fun i x ->
                    let res = Address.elimination x
                    let handle = match res.Head with | Address.Handle x -> x | _ -> invalidArg "can't" "get here"
                    Expect.isTrue (handle.Tags = Tags.tagSet1) (sprintf "Expected perumutation %i to match tagSet1" i) )
        ]
