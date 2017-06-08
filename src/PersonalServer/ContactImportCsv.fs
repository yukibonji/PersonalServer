namespace Jackfoxy.PersonalServer

open FSharp.Data
open AgentImport

module AgentImportCsv =

    let import source (path : string) =
        let importFile = CsvFile.Load(path).Cache()
        let headers = 
            importFile.Headers.Value
            |> Array.map (fun x -> x.Replace("\r\n", " ").Replace("\n", " "))

        let csvRowSequenceBuilder (row : CsvRow) =
            row.Columns
 
        let nameBuilders, addressBuilders, unUsedColumns = commonBuilders source headers
        let defaultBuilders, _ = entityBuilders source headers unUsedColumns UriTagged.TryParse Address.Url

        agentImport importFile.Rows csvRowSequenceBuilder nameBuilders (defaultBuilders @ addressBuilders)

