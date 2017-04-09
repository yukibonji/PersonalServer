namespace Jackfoxy.PersonalServer

open Utilities
open FSharpx.Choice
open FsVerbalExpressions.VerbalExpression
open System
open System.Text.RegularExpressions

module internal DomainVerifications =  

    let verifyTrimNonEmptyString (value : string) =
        if String.IsNullOrWhiteSpace value then
            None        
        else 
            Some <| value.Trim()

    let isMiddleEmpty (middle : string list) =
        match middle with
        | [] -> true
        | _ ->
            match middle |> List.filter (String.IsNullOrWhiteSpace >> not) with
            | [] -> true
            | _ -> false
        
    let fullName (first : string option) (middle : string list) (family : string option) =
        match first, middle, family with  
        | None, [], None ->
            None

        | (Some x), _, (Some y) when String.IsNullOrWhiteSpace x && String.IsNullOrWhiteSpace y ->
            if isMiddleEmpty middle then
                None
            else
                Some (first, middle, family)

        | (Some x), _, _ when String.IsNullOrWhiteSpace x |> not ->
            Some (first, middle, family)

        |_, _, (Some y) when String.IsNullOrWhiteSpace y |> not ->
            Some (first, middle, family)

        | _, _:_, _ -> 
            if isMiddleEmpty middle then
                None
            else
                Some (first, middle, family)

    let verifyStringInt part length =
        if String.length(part) <> length then 
            None
        else
            let regex = new Regex("^[0-9]+$")

            if regex.IsMatch part then 
                Some part
            else 
                None

    let zipCode5Plus4 (zip : string)  =
        let zipParts = zip.Split '-'

        if zipParts.Length = 2 then
            match choose {
                            do! 
                                match verifyStringInt zipParts.[0] 5 with
                                | Some _ -> Success ()
                                | _ -> Failure ""
                            do! 
                                match  verifyStringInt zipParts.[1] 4 with
                                | Some _ -> Success ()
                                | _ -> Failure ""
                            return ()
                            } with
            | Success _ -> Some zip
            | _ -> None
        else
            None

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

        let area() =
            match areaCode with
            | Some x ->
                match verifyStringInt x 3 with
                | Some _ -> Success ()
                | _ -> Failure ""
            | None -> Success ()
        
        match choose {
                        do! area()
                        do! 
                            match verifyStringInt exchange 3 with
                            | Some _ -> Success ()
                            | _ -> Failure ""
                        do! 
                            match verifyStringInt suffix 4 with
                            | Some _ -> Success ()
                            | _ -> Failure ""
                        return ()
                        } with
        | Success _ -> Some (areaCode, exchange, suffix)
        | Failure _ -> None

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