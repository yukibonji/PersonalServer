namespace Jackfoxy.PersonalServer

open System
open Tweetinvi
open Utilities

module ContactImportTwitter =

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

    let tryTag prefix (content : string) sources =
        match TrimNonEmptyString.TryParse content
            |> Option.map (fun x -> 
                sprintf "%s::%s" prefix x.Value) with
        | Some x ->
            Tag.TryParse(x, sources)
        | None -> None

    
    let setOfOptionList l =
        l
        |> List.choose id
        |> Set.ofList

    let parseFollower (follower : Models.IUser) =
        let primarySource = (TrimNonEmptyString.TryParse "Twitter").Value
        let timeStamp = DateTime.UtcNow

        let createSources secondary =
            Source.Parse(primarySource, Some secondary, timeStamp, timeStamp)
            |> Set.singleton
            |> NonEmptySet.TryParse
         
        let createdAt =
            Tag.TryParse ((sprintf "Twitter::Id::%O" follower.CreatedAt), (createSources "Id").Value)
        let identity =
            Tag.TryParse((sprintf "Twitter::Id::%s" follower.IdStr), (createSources "Id").Value)
        let description = 
            tryTag "Twitter::Description" follower.Description (createSources "Description").Value
        let followers =
            Tag.TryParse((sprintf "Twitter::Followers::%O::%i" DateTime.UtcNow follower.FollowersCount), (createSources "Followers").Value)
        let following =
            Tag.TryParse((sprintf "Twitter::Following::%O::%i" DateTime.UtcNow follower.FriendsCount), (createSources "Following").Value)
        let verified = 
            tryTag "Twitter::UserIsVerified" (follower.Verified.ToString()) (createSources "UserIsVerified").Value
            
        let handle = 
            {
            Address = 
                ("@" + follower.ScreenName 
                |> TrimNonEmptyString.TryParse).Value
            Tags = setOfOptionList [createdAt; identity; description; followers; following; verified] 
            }

        let name =
            let sourceSet = 
                Set.singleton <| Source.Parse (primarySource, Some "Name", timeStamp, timeStamp)
                |> NonEmptySet.TryParse
            SimpleName.TryParse (follower.Name, Set.empty, sourceSet.Value)

        let uriTagged =
            let sourceSet =
                Set.singleton <| Source.Parse (primarySource, Some "Url", timeStamp, timeStamp)
                |> NonEmptySet.TryParse
            UriTagged.TryParse (follower.Url, Set.empty, sourceSet.Value)

        let uriPhoto =
            let sourceSet =
                Set.singleton <| Source.Parse (primarySource, Some "ProfileImageUrlFullSize", timeStamp, timeStamp)
                |> NonEmptySet.TryParse
            UriTagged.TryParse ((if follower.DefaultProfileImage then String.Empty else follower.ProfileImageUrlFullSize), Set.empty, sourceSet.Value)
   
        let timeZone = tryTag "Twitter::TimeZone" follower.TimeZone (createSources "TimeZone").Value

        let tagSet = Set.singleton timeZone.Value
        let sourceSet = 
            Set.singleton <| Source.Parse (primarySource, Some ":Location", timeStamp, timeStamp)
            |> NonEmptySet.TryParse

        let s = follower.Location.Split ','

        let physicalAddress =
            if s.Length = 2 then
                match Countries.byName.TryGetValue (s.[1].Trim()) |> toOption, 
                    Countries.stateByAbbreviation.TryGetValue (s.[1].Trim()) |> toOption, 
                    Countries.stateByName.TryGetValue (s.[1].Trim()) |> toOption with
                | Some _, None, None ->
                    PhysicalAddress.TryParse ([], Some s.[0], None, None, Some s.[1], tagSet, sourceSet.Value)
                | None, Some _, None ->
                    PhysicalAddress.TryParse ([], Some s.[0], Some s.[1], None, None, tagSet, sourceSet.Value)
                | None, None, Some _ ->
                    PhysicalAddress.TryParse ([], Some s.[0], Some s.[1], None, None, tagSet, sourceSet.Value)
                | _ ->
                    PhysicalAddress.TryParse ([], Some follower.Location, None, None, None, tagSet, sourceSet.Value)
            else
                match Countries.byName.TryGetValue (follower.Location.Trim()) |> toOption, 
                    Countries.stateByName.TryGetValue (follower.Location.Trim()) |> toOption with
                | Some _, None ->
                    PhysicalAddress.TryParse ([], None, None, None, Some follower.Location, tagSet, sourceSet.Value)
                | None, Some _ ->
                    PhysicalAddress.TryParse ([], None, Some follower.Location, None, None, tagSet, sourceSet.Value)
                | _ ->
                    PhysicalAddress.TryParse ([], Some follower.Location, None, None, None, tagSet, sourceSet.Value)

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
            {Names = Set.singleton (ContactName.SimpleName x)
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