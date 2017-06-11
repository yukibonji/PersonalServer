namespace Jackfoxy.PersonalServer

open ContactImportCsv
open FSharp.Data
open System.IO

//https://www.linkedin.com/help/linkedin/answer/66844?query=Export

module ContactImportLinkedIn =
    let splitMultiPhones (headers : string []) (rows : seq<CsvRow>) =
        match headers |> Array.tryFindIndex (fun t -> t.ToLower() = "phonenumbers") with
        | Some i ->
            rows
            |> Seq.collect (fun row -> 
                let x = row.Columns
                if x.[i].Contains(",") then
                    let phones = x.[i].Split ','

                    ([], phones)
                    ||> Array.fold (fun s t ->
                        let x' = Array.copy x
                        x'.[i] <- t
                        x'::s
                        ) 
                else
                    [x])

        | None ->
            rows
            |> Seq.map (fun row -> row.Columns)

    let import folderPath =
        let csvBuilder1 = getCsvBuilder "LinkedIn" <| Path.Combine(folderPath, "LinkedInContacts.csv")
        let csvBuilder2 = getCsvBuilder "LinkedIn" <| Path.Combine(folderPath, "LinkeInConnections.csv")
        
        let rowSequenceBuilder (row : string []) =
            row

        let rows1 = splitMultiPhones csvBuilder1.Headers csvBuilder1.Rows
        let rows2 = splitMultiPhones csvBuilder2.Headers csvBuilder2.Rows

        let x1 = 
            ContactImport.contactImport rows1 rowSequenceBuilder csvBuilder1.NameBuilders csvBuilder1.AddressBuilders
        let x2 = 
            ContactImport.contactImport rows2 rowSequenceBuilder csvBuilder2.NameBuilders csvBuilder2.AddressBuilders

        x1
        |> Seq.append x2

