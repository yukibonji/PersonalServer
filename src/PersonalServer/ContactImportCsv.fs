namespace Jackfoxy.PersonalServer

open FSharp.Data
open ContactImport

module ContactImportCsv =

    let import source (path : string) =
        let importFile = CsvFile.Load(path).Cache()
        let headers = 
            importFile.Headers.Value
            |> Array.map (fun x -> x.Replace("\r\n", " ").Replace("\n", " "))

        let csvRowSequenceBuilder (row : CsvRow) =
            row.Columns
 
        let nameBuilders, addressBuilders, unUsedColumns = commonBuilders source headers
        let defaultBuilders, _ = entityBuilders source headers unUsedColumns UriTagged.TryParse Address.Url

        contactImport importFile.Rows csvRowSequenceBuilder nameBuilders (defaultBuilders @ addressBuilders)

