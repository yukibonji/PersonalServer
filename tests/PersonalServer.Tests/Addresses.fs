namespace PersonalServer.Tests

open Jackfoxy.PersonalServer

module Addresses =
    let physicalAddress1 =
        Address.PhysicalAddress <| (PhysicalAddress.TryParse (["40 Garden Lane";"apt. 2";], (Some "Belmont"), (Some "Ca"), (Some "94002"), (Some "USA"), Tags.tagSet1a)).Value
    let physicalAddress2 =
        Address.PhysicalAddress <| (PhysicalAddress.TryParse (["53 Rosebud Dr.";], (Some "San Ramon"), (Some "Ca"), (Some "94562"), (Some "USA"), Tags.tagSet2a)).Value
    
    let emailAddress1 =
        Address.EmailAddress <| (EmailAddress.TryParse ("xx@gg.com", Tags.tagSet3a)).Value
    let emailAddress2 =
        Address.EmailAddress <| (EmailAddress.TryParse ("adsf@ggp.com", Tags.tagSet3a)).Value

    let phoneNumber1 =
        Address.PhoneNumber <| (PhoneNumber.TryParse ("4441231234", Tags.tagSet1a) ).Value
    let phoneNumber2 =
        Address.PhoneNumber <| (PhoneNumber.TryParse ("15101231234", Tags.tagSet2a) ).Value
    let phoneNumber3 =
        Address.PhoneNumber <| (PhoneNumber.TryParse ("1231234", Tags.tagSet3a) ).Value

    let uriTagged1 =
        Address.Url <| (UriTagged.TryParse ("https://fsprojects.github.io/Paket/dependencies-file.html", Tags.tagSet1a) ).Value
    
    let handle1 =
        Address.Handle <| {Address = (TrimNonEmptyString.TryParse "foxyjackfox").Value; Tags = Tags.tagSet1a}

    let physicalElim1 =
        [
        physicalAddress1
        Address.PhysicalAddress <| (PhysicalAddress.TryParse (["40 Garden Lane";"apt. 2";], (Some "Belmont"), (Some "Ca"), (Some "94002"), (Some "USA"), Tags.tagSet1a)).Value
        physicalAddress2
        emailAddress1
        phoneNumber3
        uriTagged1
        handle1
        ]

    let physicalElim2 =
        [
        physicalAddress1
        Address.PhysicalAddress <| (PhysicalAddress.TryParse (["40 Garden Lane";"apt. 2";], None, None, None, None, Tags.tagSet1a)).Value
        Address.PhysicalAddress <| (PhysicalAddress.TryParse ([], (Some "Belmont"), None, None, None, Tags.tagSet1a)).Value
        Address.PhysicalAddress <| (PhysicalAddress.TryParse ([], (Some "Belmont"), None, (Some "94002"), None, Tags.tagSet1a)).Value
        Address.PhysicalAddress <| (PhysicalAddress.TryParse ([], (Some "Belmont"), None, None, (Some "USA"), Tags.tagSet1a)).Value
        physicalAddress2
        ]

    let emailElim1 =
        [
        physicalAddress1
        emailAddress1
        Address.EmailAddress <| (EmailAddress.TryParse ("xx@gg.com", Tags.tagSet3a)).Value
        emailAddress2
        phoneNumber3
        uriTagged1
        handle1
        ]

    let phoneNumberElim1 =
        [
        physicalAddress2
        emailAddress1
        phoneNumber1
        phoneNumber2
        Address.PhoneNumber <| (PhoneNumber.TryParse ("15101231234", Tags.tagSet2a) ).Value
        Address.PhoneNumber <| (PhoneNumber.TryParse ("1231234", Tags.tagSet3a) ).Value
        phoneNumber3
        uriTagged1
        handle1
        ]

    let uriElim1 =
        [
        physicalAddress2
        emailAddress1
        phoneNumber3
        uriTagged1
        Address.Url <| (UriTagged.TryParse ("https://fsprojects.github.io/Paket/dependencies-file.html", Tags.tagSet1a) ).Value
        handle1
        ]
    let handleElim1 =
        [
        physicalAddress2
        emailAddress1
        phoneNumber1
        uriTagged1
        handle1
        Address.Handle <| {Address = (TrimNonEmptyString.TryParse "foxyjackfox").Value; Tags = Tags.tagSet1a}
        ]

    let halfAllElim1 =
        [
        physicalAddress1
        Address.PhysicalAddress <| (PhysicalAddress.TryParse (["40 Garden Lane";"apt. 2";], (Some "Belmont"), (Some "Ca"), (Some "94002"), (Some "USA"), Tags.tagSet1a)).Value
        emailAddress1
        Address.EmailAddress <| (EmailAddress.TryParse ("xx@gg.com", Tags.tagSet3a)).Value
        handle1
        Address.Handle <| {Address = (TrimNonEmptyString.TryParse "foxyjackfox").Value; Tags = Tags.tagSet1a}
        ]

    let halfAllElim2 =
        [
        phoneNumber2
        Address.PhoneNumber <| (PhoneNumber.TryParse ("15101231234", Tags.tagSet2a) ).Value
        Address.PhoneNumber <| (PhoneNumber.TryParse ("1231234", Tags.tagSet3a) ).Value
        phoneNumber3
        uriTagged1
        Address.Url <| (UriTagged.TryParse ("https://fsprojects.github.io/Paket/dependencies-file.html", Tags.tagSet1a) ).Value
        ]

    let physicalAddressTagMerge =
        [
        physicalAddress1
        Address.PhysicalAddress <| (PhysicalAddress.TryParse (["40 Garden Lane";"apt. 2";], (Some "Belmont"), (Some "Ca"), (Some "94002"), (Some "USA"), Tags.tagSet1b)).Value
        ]

    let emailTagMerge =
        [
        emailAddress1 
        Address.EmailAddress <| (EmailAddress.TryParse ("xx@gg.com", Tags.tagSet3b)).Value
        ]

    let phoneNumberTagMerge =
        [
        phoneNumber2
        Address.PhoneNumber <| (PhoneNumber.TryParse ("15101231234", Tags.tagSet2b) ).Value
        ]

    let uriTagMerge = 
        [
        uriTagged1
        Address.Url <| (UriTagged.TryParse ("https://fsprojects.github.io/Paket/dependencies-file.html", Tags.tagSet1b) ).Value
        ]
        
    let handleTagMerge =
        [
        handle1
        Address.Handle <| {Address = (TrimNonEmptyString.TryParse "foxyjackfox").Value; Tags = Tags.tagSet1b}
        ]