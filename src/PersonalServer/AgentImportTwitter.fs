namespace Jackfoxy.PersonalServer

open Tweetinvi

module AgentImportTwitter =

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


