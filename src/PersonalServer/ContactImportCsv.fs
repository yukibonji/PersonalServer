namespace Jackfoxy.PersonalServer

open FSharp.Data
open ContactImport

module ContactImportCsv =

    type CsvBuilder =
        {
        Rows : seq<CsvRow>
        NameBuilders : (string [] -> ContactName option * Set<Tag>) list
        AddressBuilders : (string [] -> Address option * Set<Tag>) list
        SourceMeta : ImportSourceMeta
        }

    let getCsvBuilder sourceMeta (path : string) =
        let importFile = CsvFile.Load(path).Cache()
        let headers = 
            importFile.Headers.Value
            |> Array.map (fun x -> x.Replace("\r\n", " ").Replace("\n", " "))

        let sourceMeta' = {sourceMeta with Headers = headers}
            
        let nameBuilders, addressBuilders, unUsedColumns = commonBuilders sourceMeta' 
        let defaultBuilders, _ = entityBuilders sourceMeta' UriTagged.TryParse unUsedColumns Address.Url
        
        {
        Rows = importFile.Rows
        NameBuilders = nameBuilders
        AddressBuilders = defaultBuilders @ addressBuilders
        SourceMeta = sourceMeta'
        }

    let import sourceMeta path =
        let csvRowSequenceBuilder (row : CsvRow) =
            row.Columns
        let csvBuilder = getCsvBuilder sourceMeta path

        contactImport csvBuilder.Rows csvRowSequenceBuilder csvBuilder.NameBuilders csvBuilder.AddressBuilders

