namespace PersonalServer.Tests

open DomainGeneratorsCode
open Jackfoxy.PersonalServer
open Expecto
open FsCheck

module ContactImport =
    let config10k = { FsCheckConfig.defaultConfig with maxTest = 10000}

    [<Tests>]
    let testSimpleEntityBuilder =

        let testSimpleEntity input tryParse =
            let t1 = tryParse (input, Set.empty<Tag>)

            let headers = [|"doodle"; "web"; "doodle2";|]
            let columns = [|"a"; input; ""|]

            let result = Jackfoxy.PersonalServer.ContactImport.simpleEntityBuilder tryParse 1 "test" headers columns

            match t1 with 
            | Some _ -> 
                let tags = Set.add (Tag.TryParse "test::web").Value Set.empty
                (fst result) = tryParse (input, tags)
            | None ->                        
                (snd result) = Set.add (Tag.TryParse <| sprintf "test::web::%s" input).Value Set.empty
            
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
    let testFullNameBuilder =
        testList "ContactImport.FullNameBuilder" [
//            testPropertyWithConfig config10k "equality" <|
//                fun  () ->
//                    Prop.forAll (Arb.fromGen <| nonEmptyNonAllWhitespaceString())
//                        (fun  (simpleName) ->
//                            testSimpleEntity simpleName PersonName.TryParse )
        ]

//        let fullNameBuilders =
//            [fullNameBuilder fullNameBuilderParms source headers >> rawToFinalResult NameOfPerson.FullName]

    [<Tests>]
    let testPhysicalAddressBuilder =
        testList "ContactImport.PhysicalAddressBuilder" [
        ]
//        let physicalAddressBuilders =
//            [physicalAddressBuilder physicalAddressBuilderParms source headers >> rawToFinalResult Address.PhysicalAddress]

