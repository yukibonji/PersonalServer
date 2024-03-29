namespace Jackfoxy.PersonalServer

open System
open System.Text.RegularExpressions

module internal Utilities =
        
    let (|Success|Failure|) = 
        function
        | Choice1Of2 a -> Success a
        | Choice2Of2 e -> Failure e

    let inline Success x = Choice1Of2 x
    let inline Failure x = Choice2Of2 x

    let digitsFromString (s : string) =
        s.ToCharArray()
        |> Array.fold (fun s t -> 
            if t >= '0' && t <= '9' then
                t::s
                else s) []
        |> List.rev
        |> List.toArray
            
    let verifyTrimNonEmptyString (value : string) t =
        if String.IsNullOrWhiteSpace value then
            None        
        else 
            Some (t <| value.Trim())

    let verifyStringInt (s : string) length t =
        if String.IsNullOrWhiteSpace s then
            None
        else
            let s' = s.Trim()
            if String.length(s') <> length then 
                None
            else
                let regex = new Regex("^[0-9]+$")

                if regex.IsMatch s' then 
                    Some <| t s'
                else 
                    None

    let verifyUpperLatinString (s : string) length t =
        if String.IsNullOrWhiteSpace s then
            None
        else
            let s' = s.Trim()
            if String.length(s') <> length then 
                None
            else
                let regex = new Regex("^[A-Z]+$")

                if regex.IsMatch s' then 
                    Some <| t s'
                else 
                    None

    let inline toOption x  = 
        match x with
        | true, v    -> Some v
        | _   -> None

    let unixMillisecondToDateTime (timestamp:int64) =
      let start = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
      start.AddSeconds(float (timestamp / 1000L)).ToLocalTime()

    let defaultArgRev x y =
        defaultArg y x

    let port portNumber =
        let caller = "Port"

        if portNumber < 0 || portNumber > 65535 then
            (caller, sprintf "port number '%i' outside of range 0 - 65535" portNumber)
            |> Failure
        else 
            Success ()