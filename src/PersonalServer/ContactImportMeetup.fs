namespace Jackfoxy.PersonalServer

open ContactImport
open FSharp.Data
open FSharp.Data.JsonExtensions
open System
open System.Net.Http
open Utilities

module ContactImportMeetup =

    type Meetup =
        {
        Id : TrimNonEmptyString
        Name : TrimNonEmptyString
        Link : Uri
        NextEvent : JsonValue option
        EventSample : JsonValue option
        LastEvent : JsonValue option
        Organizer : JsonValue option
        MemberSample : JsonValue option
        }

    type MeetupMember =
        {
        Id : TrimNonEmptyString
        Name : TrimNonEmptyString option
        Bio : TrimNonEmptyString option
        Joined : DateTime option
        City : TrimNonEmptyString option
        State : TrimNonEmptyString option
        Country : TrimNonEmptyString option
        Photo : Uri option
        GroupProfile : JsonValue option
        }

    let headerAsInt (response : HttpResponseMessage) key =
        response.Headers.GetValues key 
        |> Seq.cast<string>
        |> Seq.head
        |> int
        
    let get (url : Uri) = 
        use client = new HttpClient ()
        let request = new HttpRequestMessage(Net.Http.HttpMethod.Get, url)
        let response = (client.SendAsync request).Result

        match response.IsSuccessStatusCode with
        | true  ->
            try
                if headerAsInt response "X-RateLimit-Remaining" = 0 then
                    printfn "waiting"
                    Threading.Thread.Sleep (headerAsInt response "X-RateLimit-Remaining")
                else
                    ()
            with e ->
                printfn "error 1"
                () //to do: log response
            Some (response.Content.ReadAsStringAsync().Result)
        | false -> 
            //to do: log response
            printfn "error 2"
            None 

    let myGroups apiKey = 
        let request = sprintf "https://api.meetup.com/self/groups?key=%s&fields=event_sample,last_event" apiKey
        get (Uri request)

    let groupByUrlName apiKey nameFromUrl = 
        let request = sprintf "https://api.meetup.com/%s?key=%s&fields=event_sample,rsvp_sample,member_sample,last_event" nameFromUrl apiKey
        get (Uri request)

    let groupMembers apiKey offset nameFromUrl = 
        let request = sprintf "https://api.meetup.com/%s/members?key=%s&page=200&offset=%i" nameFromUrl apiKey offset
        get (Uri request)

    let meetupMember apiKey id = 
//info over and above what is returned with group member info: birthday, gender, hometown, visited (time of last activity on meetup),
// messagable (see messaging_pref for settings), other services (e.g. twitter & facebook handles)
        let request = sprintf "https://api.meetup.com/2/member/%s?key=%s&fields=messaging_pref" id apiKey
        get (Uri request)

    let matchPropertyToString (properties : (string * JsonValue) seq) searchLabel =
        match properties |> Seq.tryFind (fun (label, _) -> label = searchLabel) with
        | Some (_, value) -> Some <| value.AsString()
        | _ -> None
        |> TrimNonEmptyString.TryParse

    let matchPropertyToValue (properties : (string * JsonValue) seq) searchLabel =
        match properties |> Seq.tryFind (fun (label, _) -> label = searchLabel) with
        | Some (_, value) -> Some value
        | _ -> None

    let parseMeetup meetupRaw =
        match meetupRaw with
        | JsonValue.Record _ as record -> 
            let keyProperties =
                record.Properties
                |> Seq.filter (fun (label, _) ->
                    Array.exists (fun x -> x = label) 
                        [|"id"; "name"; "link"; "next_event"; "event_sample"; "last_event"|])

            let id = matchPropertyToString keyProperties "id"
            let name = matchPropertyToString keyProperties "name"
            let link = matchPropertyToString keyProperties "link"
            let nextEvent = matchPropertyToValue keyProperties "next_event"
            let eventSample = matchPropertyToValue keyProperties "event_sample"
            let lastEvent = matchPropertyToValue keyProperties "last_event"
            let organizer =  matchPropertyToValue keyProperties "organizer"
            let memberSample = matchPropertyToValue keyProperties "member_sample"

            match id, name, link with
            | Some id, Some name, Some link ->
                match Uri.TryCreate (link.Value, UriKind.Absolute) |> toOption with
                | Some x ->
                    {
                    Id = id
                    Name = name
                    Link = x
                    NextEvent = nextEvent
                    EventSample = eventSample
                    LastEvent = lastEvent
                    Organizer = organizer
                    MemberSample = memberSample
                    } |> Some
                | None -> None
            | _ -> None
        | _ -> None

    let meetupNameFromUri (uri : Uri) =
        uri.ToString().Replace("https://www.meetup.com/", "").Replace("/", "")
        
//to do: contact log information, also from posts, comments
//        //organizer
//            //event_sample::rsvp_sample
//            //member_sample

    let fullGroup apiKey (group : Meetup) =
        let nameFromUrl = meetupNameFromUri group.Link
        let groupDetail = 
            groupByUrlName apiKey nameFromUrl
            |> (defaultArgRev String.Empty)
            |> JsonValue.Parse
            |> parseMeetup

        match groupDetail with
        | Some x ->
            x
        | None ->
            group

    let groupsParsed apiKey groupsRaw =
        match JsonValue.Parse groupsRaw with
        | JsonValue.Array _ as array -> 
            array.AsArray()
            |> Array.choose parseMeetup
            //|> Array.map (fullGroup apiKey) 
            |> Some
        | _ -> None

    let parseMember memberRaw =
        match memberRaw with
        | JsonValue.Record _ as record -> 
            let keyProperties = record.Properties

            let id = matchPropertyToString keyProperties "id"
            let name = matchPropertyToString keyProperties "name"
            let bio = matchPropertyToString keyProperties "bio"
            let joined = matchPropertyToString keyProperties "joined"
            let city = matchPropertyToString keyProperties "city"
            let state = matchPropertyToString keyProperties "state"
            let country = matchPropertyToString keyProperties "country"

            let photo = 
                match defaultArgRev JsonValue.Null <| matchPropertyToValue keyProperties "photo" with
                | JsonValue.Record _ as record -> 
                    let keyProperties' = record.Properties
                    matchPropertyToString keyProperties' "photo_link"
                | _ -> None

            let groupProfile = matchPropertyToValue keyProperties "group_profile"

            match id with
            | Some id ->
                (id,
                    {
                    Id = id
                    Name = name
                    Bio = bio
                    Joined = 
                        match joined with
                        | Some x -> 
                            match Int64.TryParse x.Value |> toOption with
                            | Some unixM ->
                                unixMillisecondToDateTime unixM
                                |> Some
                            | None -> None
                        | None -> None
                    City = city
                    State = state
                    Country = country
                    Photo = 
                        match photo with
                        | Some x ->
                            Uri.TryCreate (x.Value, UriKind.Absolute) |> toOption
                        | _ -> None
                    GroupProfile = groupProfile
                    }) |> Some
            | _ -> None
        | _ -> None

    let membersParsed membersRaw =
        match JsonValue.Parse membersRaw with
        | JsonValue.Array _ as array -> 
            array.AsArray()
            |> Array.choose parseMember //to do: individual member retrieval for more info
            |> Some
        | _ -> None

//    let jsonValueToString jsonValue = 
//        match JsonValue.Parse jsonValue with
//        | JsonValue.String _ as x -> x.AsString() |> Some
//        | JsonValue.Number _ as n -> n.AsDecimal().ToString() |> Some
//        | JsonValue.Float _ as f -> f.AsFloat().ToString() |> Some
//        | JsonValue.Record _ as record -> 
//            let record' = record.Properties
//            for (name, value) in record' do
//                printfn "name: %s ; value: %s" name <| value.ToString()
//            record.AsArray().ToString() |> Some
//        | JsonValue.Array _ as array -> 
//            for x in array.AsArray() do
//                printfn "%A" x
//            array.AsArray().ToString() |> Some
//        | JsonValue.Boolean _ as b -> b.ToString() |> Some
//        | JsonValue.Null -> None

    let membersFromGroup apiKey (url : Uri) =
        let nameFromUrl = meetupNameFromUri url

        let rec loop offset members lastRawMembers =
            match groupMembers apiKey offset nameFromUrl with
            | Some rawMembers ->
                if rawMembers = lastRawMembers then
                    members
                else
                    let newMembers = defaultArgRev Array.empty <| membersParsed rawMembers
                    loop (offset + 1) (Array.append newMembers members) rawMembers 
            | None ->
                members
            
        loop 0 Array.empty String.Empty

    let members apiKey = 
        let groupsRaw = defaultArgRev String.Empty <| myGroups apiKey
        match groupsParsed apiKey groupsRaw with
        | Some x ->
            x
            |> Array.map (fun group -> membersFromGroup apiKey group.Link)
            |> Array.collect id
            |> Array.groupBy (fun (x, _) -> x)
            |> Some
        | None ->
            None

    let nonEmptyValueOrEmpty (x : TrimNonEmptyString option) =
        x
        |> Option.map (fun t -> t.Value)
        |> defaultArgRev String.Empty

    let meetupMemberRowSequenceBuilder (row : (TrimNonEmptyString * (TrimNonEmptyString * MeetupMember)[])) =
        let id, members = row

        let profile = members.[0] |> snd

        //to do parse group profiles

        [|profile.Id.Value; 
            nonEmptyValueOrEmpty profile.Name; 
            nonEmptyValueOrEmpty profile.Bio; 
            (match profile.Joined with | Some x -> x.ToShortDateString() | None -> String.Empty);
            nonEmptyValueOrEmpty profile.City;
            nonEmptyValueOrEmpty profile.State;
            nonEmptyValueOrEmpty profile.Country;
            (match profile.Photo with | Some x -> x.ToString() | None -> String.Empty);
            String.Empty;
            String.Empty;
            |]

    let import apiKey =
        let sourceMeta = 
            {
            PrimaryName = TrimNonEmptyString.Parse ["Meetup"] |> List.head
            TimeStamp = DateTime.UtcNow
            Headers = [|"member id"; "name"; "bio"; "joined meetup"; "city"; "state"; "country"; "photo link"; "intros"; "roles"|]
            }
            
        match members apiKey with
        | Some rows -> 
            let nameBuilders, addressBuilders, unUsedColumns = commonBuilders sourceMeta
            let defaultBuilders, _ = entityBuilders sourceMeta UriTagged.TryParse unUsedColumns Address.Url

            contactImport rows meetupMemberRowSequenceBuilder nameBuilders (defaultBuilders @ addressBuilders)

        | None -> Seq.empty