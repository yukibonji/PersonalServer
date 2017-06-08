namespace Jackfoxy.PersonalServer

open System
open Tweetinvi
open Utilities

module AgentImportTwitter =

    let headers =
        [||]

    type TwitterCredentials =
        {
        ConsumerKey : string
        ConsumerSecret : string
        UserAccessToken : string
        UserAccessSecret : string
        }

    let getFollowers twitterCredential = 
        Auth.SetUserCredentials(twitterCredential.ConsumerKey, twitterCredential.ConsumerSecret, twitterCredential.UserAccessToken, twitterCredential.UserAccessSecret)
        |> ignore
        let user = User.GetAuthenticatedUser()

        user.GetFollowers(5000)
        |> Seq.cast<Models.IUser>
        |> Seq.toList

    let tryTag prefix (content : string) =
        match TrimNonEmptyString.TryParse content
            |> Option.map (fun x -> 
                sprintf "%s::%s" prefix x.Value) with
        | Some x ->
            Tag.TryParse x
        | None -> None

    
    let setOfOptionList l =
        l
        |> List.choose id
        |> Set.ofList

    let parseFollower (follower : Models.IUser) =
        let createdAt =
            sprintf "Twitter::Id::%O" follower.CreatedAt
            |> Tag.TryParse
        let identity =
            sprintf "Twitter::Id::%s" follower.IdStr
            |> Tag.TryParse
        let description = tryTag "Twitter::Description" follower.Description
        let followers =
            sprintf "Twitter::Followers::%O::%i" DateTime.UtcNow follower.FollowersCount
            |> Tag.TryParse
        let following =
            sprintf "Twitter::Following::%O::%i" DateTime.UtcNow follower.FriendsCount
            |> Tag.TryParse
        let verified = tryTag "Twitter::UserIsVerified" <| follower.Verified.ToString()
            
        let handle = 
            {
            Address = 
                ("@" + follower.ScreenName 
                |> TrimNonEmptyString.TryParse).Value
            Tags = setOfOptionList [createdAt; identity; description; followers; following; verified] 
            }

        let name =
            SimpleName.TryParse (follower.Name, Set.add (Tag.TryParse "Twitter::Name").Value Set.empty)

        let uriTagged =
            UriTagged.TryParse (follower.Url, Set.add (Tag.TryParse "Twitter::Url").Value Set.empty)

        let uriPhoto =
            UriTagged.TryParse ((if follower.DefaultProfileImage then String.Empty else follower.ProfileImageUrlFullSize), Set.add (Tag.TryParse "Twitter::ProfileImageUrlFullSize").Value Set.empty)
   
        let timeZone = tryTag "Twitter::TimeZone" follower.TimeZone

        let tagSet = setOfOptionList [timeZone; Tag.TryParse "Twitter::Location"]

        let s = follower.Location.Split ','

        let physicalAddress =
            if s.Length = 2 then
                match Countries.byName.TryGetValue (s.[1].Trim()) |> toOption, 
                    Countries.stateByAbbreviation.TryGetValue (s.[1].Trim()) |> toOption, 
                    Countries.stateByName.TryGetValue (s.[1].Trim()) |> toOption with
                | Some _, None, None ->
                    PhysicalAddress.TryParse ([], Some s.[0], None, None, Some s.[1], tagSet)
                | None, Some _, None ->
                    PhysicalAddress.TryParse ([], Some s.[0], Some s.[1], None, None, tagSet)
                | None, None, Some _ ->
                    PhysicalAddress.TryParse ([], Some s.[0], Some s.[1], None, None, tagSet)
                | _ ->
                    PhysicalAddress.TryParse ([], Some follower.Location, None, None, None, tagSet)
            else
                match Countries.byName.TryGetValue (follower.Location.Trim()) |> toOption, 
                    Countries.stateByName.TryGetValue (follower.Location.Trim()) |> toOption with
                | Some _, None ->
                    PhysicalAddress.TryParse ([], None, None, None, Some follower.Location, tagSet)
                | None, Some _ ->
                    PhysicalAddress.TryParse ([], None, Some follower.Location, None, None, tagSet)
                | _ ->
                    PhysicalAddress.TryParse ([], Some follower.Location, None, None, None, tagSet)

        let uriAddresses =
            [uriTagged; uriPhoto]
            |> List.choose id
            |> List.map Address.Url

        let addresses =
            match physicalAddress with
            | Some x ->
                (Address.PhysicalAddress x)::(Address.Handle handle)::uriAddresses
            | None ->
                (Address.Handle handle)::uriAddresses
            |> Set.ofList

        match name with
        | Some x ->
            {Names = Set.add (ContactName.SimpleName x) Set.empty
             Addresses = addresses
             Tags = Set.empty}
        | None ->
            {Names = Set.empty
             Addresses = addresses
             Tags = Set.empty}
        
    let parseFollowers twitterCredential = 
        let followers = getFollowers twitterCredential

        followers
        |> List.map parseFollower