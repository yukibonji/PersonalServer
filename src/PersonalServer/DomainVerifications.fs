namespace Jackfoxy.PersonalServer

open Utilities
open FSharpx.Choice
open FsVerbalExpressions.VerbalExpression
open System

module internal DomainVerifications =

    let verifyConstructor f =
        match f with
        | Success _ -> ()
        | Failure (caller,msg) -> invalidArg caller msg

    let verifyTag tag =
        if String.IsNullOrEmpty tag then Failure ("Tag", "tag is null or empty")
        else Success ()

    let verifyNonEmptyString value =
        if String.IsNullOrEmpty value then Failure ("NonEmptyString", "value is empty or null")
        else  Success ()

    let zipCode5 zip =
        verifyStringInt "ZipCode5" "zip" zip 5

    let verifyDigitString localValue length value =
        let label = sprintf "DigitString%i" length
        match verifyStringInt label label value length with
        | Success _ -> 
            localValue := value
        | failure -> 
            verifyConstructor failure

    let zipCode5Plus4 (zip : string)  =
        let zipParts = zip.Split '-'
        let caller = "ZipCode5Plus4"

        if zipParts.Length = 2 then
            match choose {
                            do! verifyStringInt caller "zip5" zipParts.[0] 5
                            do! verifyStringInt caller "zip4" zipParts.[1] 4
                            return ()
                            } with
            | Success _ -> Success ()
            | Failure (_,msg) -> Failure (caller, msg)
        else
            (caller, sprintf "%s is not numeric and dash" caller)
            |> Failure

    let emailAddress email =
        let caller = "EmailAddress"
        //to do: @ is present, not start or end
        //      no illegal characters
        // https://tools.ietf.org/html/rfc2822
        // https://tools.ietf.org/html/rfc2822#section-3.2.4
        // https://tools.ietf.org/html/rfc2822#section-3.4.1
        // http://www.ex-parrot.com/pdw/Mail-RFC822-Address.html
        //
        // http://emailregex.com/
        // ( see also RFC 5322 )
        //  [a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?

        // http://stackoverflow.com/questions/297420/list-of-email-addresses-that-can-be-used-to-test-a-javascript-validation-script
        // http://code.iamcal.com/php/rfc822/tests/

        let emailValid = new VerbEx("""[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?""")
        match emailValid.IsMatch email with
        | true -> Success ()
        | false -> 
            (caller, sprintf "%s is not a valid email address" email)
            |> Failure

    let usPhone (areaCode : string option) (exchange : string) (suffix : string) =
        let caller = "UsPhone"

        let area() =
            match areaCode with
            | Some x ->
                verifyStringInt caller "areaCode" x 3
            | None -> Success ()
        
        match choose {
                        do! area()
                        do! verifyStringInt caller "exchange" exchange 3
                        do! verifyStringInt caller "suffix" suffix 4
                        return ()
                        } with
        | Success _ -> Success ()
        | Failure (_,msg) -> Failure (caller, msg)

    let otherPhone (phone : string) =
        let caller = "OtherPhone"

        let passed, _ = dashAndNumberOnly phone
        if passed then 
            Success ()
        else
            (caller, sprintf "'%s' contains other than numbers and dashes" phone)
            |> Failure

    let phoneNumber countryCode _ _ = 
        let caller = "PhoneNumber"

        match countryCode with
        | Some x ->
            let passed, dashCount = dashAndNumberOnly x
            if passed then
                if dashCount < 2 then 
                    Success ()
                else
                    (caller, sprintf "countryCode '%s' contains other than numbers and dashes" x)
                    |> Failure
            else
                (caller, sprintf "countryCode '%s' contains other than numbers and dashes" x)
                |> Failure
        | None -> 
            Success ()

    let port portNumber =
        let caller = "Port"

        if portNumber < 0 || portNumber > 65535 then
            (caller, sprintf "port number '%i' outside of range 0 - 65535" portNumber)
            |> Failure
        else 
            Success ()