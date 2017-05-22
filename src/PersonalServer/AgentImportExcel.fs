namespace Jackfoxy.PersonalServer

open System
open System.Data
open System.IO
open AgentImport

module AgentImportExcel =

    type ExcelSheet =
        {
        Name : string
        HeaderRows : int []
        }   
        
    let getExcelSheetHeaders (workSheet : DataTable) (headerRows : int []) =
        let index = headerRows.Length - 1
        let lastHeader =
            workSheet.Rows.[headerRows.[index] - 1].ItemArray
            |> Array.map (fun x -> x.ToString())

        let rec loop i currentHeader =
            if i < 0 then
                currentHeader
                |> Array.map (fun (x : string) -> x.Replace("\r\n", " ").Replace("\n", " "))
            else
                let nextHeader =
                    (workSheet.Rows.[headerRows.[i] - 1].ItemArray, currentHeader)
                    ||> Array.map2 (fun t1 t2 -> 
                        (sprintf "%s %s" (t1.ToString()) t2).Trim())
                loop (i - 1) nextHeader
                
        loop (index - 1) lastHeader

    let importExcelSheet source workSheet headerRows =
        let headers = getExcelSheetHeaders workSheet headerRows
            
        let excelRowSequenceBuilder (row : DataRow) =
            row.ItemArray
            |> Array.map (fun x -> x.ToString())
        
        let nameBuilders, addressBuilders, unUsedColumns = commonBuilders source headers
        let defaultBuilders, _ = entityBuilders source headers unUsedColumns UriTagged.TryParse Address.Url

        let rows = workSheet.Rows |> Seq.cast |> Seq.skip headerRows.[headerRows.Length - 1]

        agentImport rows excelRowSequenceBuilder nameBuilders (defaultBuilders @ addressBuilders)

    let import source (path : string) (excelSheets : ExcelSheet list) =

        use stream =
            File.OpenRead(path)
             
        use reader =          
            if path.EndsWith(".xlsx", StringComparison.OrdinalIgnoreCase)
            then Excel.ExcelReaderFactory.CreateOpenXmlReader(stream)
            else Excel.ExcelReaderFactory.CreateBinaryReader(stream)

        // if not reader.IsValid then fail action (Exception reader.ExceptionMessage)

        let dataset = reader.AsDataSet()

        (Seq.empty, excelSheets)
        ||> List.fold (fun s t ->
            importExcelSheet source dataset.Tables.[t.Name] t.HeaderRows
            |> Seq.append s ) 

