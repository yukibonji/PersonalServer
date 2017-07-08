namespace Jackfoxy.PersonalServer

open FSharp.Data
open ContactImport

module ContactImportCsv =

    type CsvBuilder =
        {
        Headers : string []
        Rows : seq<CsvRow>
        NameBuilders : (string [] -> ContactName option * Set<Tag>) list
        AddressBuilders : (string [] -> Address option * Set<Tag>) list
        }

    let getCsvBuilder source (path : string) =
        let importFile = CsvFile.Load(path).Cache()
        let headers = 
            importFile.Headers.Value
            |> Array.map (fun x -> x.Replace("\r\n", " ").Replace("\n", " "))
 
        let nameBuilders, addressBuilders, unUsedColumns = commonBuilders source headers
        let defaultBuilders, _ = entityBuilders source headers UriTagged.TryParse unUsedColumns Address.Url
        
        {
        Headers = headers
        Rows = importFile.Rows
        NameBuilders = nameBuilders
        AddressBuilders = defaultBuilders @ addressBuilders
        }

    let import source path =
        let csvRowSequenceBuilder (row : CsvRow) =
            row.Columns
        let csvBuilder = getCsvBuilder source path

        contactImport csvBuilder.Rows csvRowSequenceBuilder csvBuilder.NameBuilders csvBuilder.AddressBuilders

