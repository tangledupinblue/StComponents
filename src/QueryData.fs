module Components.QueryData

open System

let (|MatchStr|) str1 str2 = str1 = str2

// let fieldTypeFromName fieldTypeName =
//     match fieldTypeName with
//         | MatchStr 

type FieldType =
    | Booly
    | Inty
    | Floaty
    | Stringy
    | Datey
    | Duration
    with static member fromString str = 
                            match str with
                                | MatchStr "Booly" true -> Booly
                                | MatchStr "Inty" true -> Inty
                                | MatchStr "Floaty" true -> Floaty
                                | MatchStr "Stringy" true -> Stringy
                                | MatchStr "Datey" true -> Datey
                                | MatchStr "Duration" true -> Duration
                                | _ -> failwith <| sprintf "Unknown FieldType %s" str 

type Cell = {
    MaybeFloat: float option
    MaybeString: string option
}

type Row = {
    Key: string
    Cells: Cell list
    }    

type RowComparator =
    | Empty
    | LeftOnly of Row
    | RightOnly of Row
    | Same of Row*Row
    | Diff of Row*Row

type ComparedRow = {
    Key: string
    Compared: RowComparator
    }


type QueryData = {
    Headers: string list
    FieldTypes: FieldType list
    Rows: Row list //TODO - list bad for performance, this should be an array
    }
    with 
        static member empty = { Headers= []; FieldTypes= []; Rows= [] }
        static member isEmpty qd = qd.Headers |> List.length = 0


type FieldHeader= {
    FieldName: string
    FieldType: FieldType
}
    with 
        static member create_ name ft = { FieldName= name ; FieldType= ft }
        static member toString cp = sprintf "%s: %A" cp.FieldName cp.FieldType
        static member headers fhs = fhs |> List.map (fun x -> x.FieldName)
        static member fieldTypes fhs = fhs |> List.map (fun x -> x.FieldType)
        static member toQueryData fhs = { Headers = FieldHeader.headers fhs ; FieldTypes= FieldHeader.fieldTypes fhs ; Rows= [] }


//    let empty = { Headers= []; FieldTypes= []; Rows= [] }


// type FieldHeaders = {
//     Headers: string list
//     FieldTypes: FieldType list
// }

let fieldTypeFromDotNet (typeName:string) : FieldType =
    //printfn "TypeName= %s" typeName
    let mappedTypes = [ 
        ("Boolean", Booly )
        ("Int16", Inty)
        ("Int32", Inty)
        ("Int64", Inty)
        ("Double", Floaty)
        ("Decimal", Floaty)  
        ("DateTime", Datey)
        ] 
    let findMatch = mappedTypes |> List.filter (fun (x,_) -> x = typeName) |> List.tryHead
    match findMatch with
        | Some (_,y) -> y
        | None -> Stringy 



let setHeaders headers (qd:QueryData) = 
    { qd with Headers = headers }

let setFieldTypes fieldTypes (qd:QueryData)=
    { qd with FieldTypes= fieldTypes }

let changeFieldType fieldName fieldType (qd:QueryData) = 
    List.mapi (fun i h -> i,h) qd.Headers
        |> List.tryFind (fun (i,h) -> h = fieldName)
        |> Option.bind (fun (i,h) -> Some i)
        |> (fun oi ->
                match oi with
                    | Some index -> { qd with FieldTypes= qd.FieldTypes |> List.mapi (fun i x -> if i = index then fieldType else x) }
                    | None -> qd )

let setRows rows (qd:QueryData)=
    { qd with Rows= rows }

let cellFromFloat (value:float) =
    { MaybeFloat = Some value; MaybeString = None }

let cellFromString (value:string) =
    { MaybeFloat = None; MaybeString = Some value }

let cellFromInt (value:int) = 
    { MaybeFloat= float value |> Some ; MaybeString= None }

let cellFromBool (value:bool) =
    { MaybeFloat= None ; MaybeString= (if value then "true" else "false") |> Some }

let cellFromGuid (value:Guid) = 
    { MaybeFloat = None; MaybeString = Some (string value) }

let cellFromFieldType fieldType (value:string) =
    match fieldType with
        | Booly -> cellFromString value
        | Inty -> cellFromInt (Int32.Parse(value))
        | Floaty -> cellFromFloat (Double.Parse(value))
        | Stringy -> cellFromString value
        | Datey -> cellFromString value
        | Duration -> cellFromFloat (Double.Parse(value))
   
let cellText (cell: Cell) =
    match (cell.MaybeFloat, cell.MaybeString) with
        | (_, Some s) -> s
        | (Some f, _) -> sprintf "%.2f" f
        | (None, None) -> ""
     
let cellFormattedText (ft:FieldType) (cell:Cell)  = 
    match ft with
        | Booly ->
            Option.defaultValue "false" cell.MaybeString |> (fun str -> str.ToLower())
        | Inty -> 
            Option.defaultValue 0.0 cell.MaybeFloat
                |> int
                |> sprintf "%i" 
        | Floaty -> 
            let f = Option.defaultValue 0.0 cell.MaybeFloat
            sprintf "%.2f" f
        | Duration -> 
            let h, mins = Option.defaultValue 0.0 cell.MaybeFloat
                            |> (fun f -> f / 60. |> Math.Floor  |> int , f % 60. |> int )
            sprintf "%i h %i mins" h mins
        | Datey | Stringy -> cellText cell
        
let cellToString (c:Cell) =
    Option.get c.MaybeString

let cellToFloat (c:Cell) =
    Option.get c.MaybeFloat

let cellToInt (c:Cell) =
    let toInt (f:float) = (Convert.ToInt32(f))
    Option.get c.MaybeFloat |> toInt

let cellToGuid (c:Cell) = 
    c.MaybeString |> Option.map (fun s -> Guid.Parse(s) ) |> Option.get

let getCell (headers:string list) (r:Row) (fieldName:string) =
    try 
        r.Cells 
            |> List.map2 (fun x y -> (x,y)) headers 
            |> List.filter (fun (x,_) -> x = fieldName)
            |> List.head 
            |> snd
    with e -> "No Field found for " + fieldName |> failwith

let getRowsFromKeys (keys:string list) (qd:QueryData) =
    qd.Rows |> List.filter (fun r -> keys |> List.exists (fun k -> k = r.Key) )

// let cellEqualsString headers r fieldName compare =
//     getCell headers r fieldName 
//         |> cellToString
//         |> (fun str -> str = compare)

let comparableCells = List.map cellText >> String.concat "|"

let comparableRow = (fun r -> r.Cells) >> comparableCells

let compareRows r1 r2 = 
    comparableRow r1 = comparableRow r2

let filterOnField fieldName filterValue qd =
    let ofieldInfo = 
        List.mapi2 (fun i h f -> i,h,f) qd.Headers qd.FieldTypes
            |> List.tryFind (fun (_,h,_) -> h = fieldName)
    match ofieldInfo with
        | Some (i,h,f) -> 
            qd.Rows
                |> List.filter (fun r -> r.Cells.[i] |> cellFormattedText f = filterValue)
        | None ->
            qd.Rows
        
        |> (fun rows -> { Headers= qd.Headers ; FieldTypes= qd.FieldTypes ; Rows= rows } )


let showQueryData (qd:QueryData) = 
    Console.WriteLine(qd.Headers |> String.concat ";" |> string)
    Console.WriteLine(String.Format("{0} Rows", qd.Rows.Length) |> string)

type QueryData with
    static member displayString qd =
        [   qd.Headers |> List.map (sprintf "%10s") |> String.concat ""
            qd.FieldTypes |> List.map (string >> sprintf "%10s") |> String.concat ""
            qd.Rows 
                |> List.map 
                    (fun row -> 
                        row.Cells |> List.map (cellText >> sprintf "%10s") |> String.concat "" ) 
                |> String.concat "\r\n" 
        ]
            |> String.concat "\r\n" 




module private this =
    let stringToInty (s:string) = Convert.ToInt32 s |> Convert.ToDouble |> cellFromFloat
    let stringToFloaty (s:string) = Convert.ToDouble s |> cellFromFloat
    let stringToStringy (s:string) = s |> cellFromString
    let stringToDatey (s:string) = s |> cellFromString //s |> DateTime.Parse(s) |> string |> cellFromString
    let stringToBooly (s:string) = s |> cellFromString

    let tryConvertTo<'i,'o> (op:'i -> 'o) (s:'i) : Result<'o,string> =
        try
            Ok (op s) 
        with 
            | err -> Error err.Message

    let stringToCell (str:string) (fieldType:FieldType) : Result<Cell,string> =
        match fieldType with
            | Booly ->tryConvertTo stringToBooly str
            | Inty -> tryConvertTo stringToInty str
            | Floaty | Duration -> tryConvertTo stringToFloaty str
            | Stringy -> tryConvertTo stringToStringy str
            | Datey -> tryConvertTo stringToDatey str    

    let cellProperties (qd:QueryData) =
        List.map2 (fun x y -> { FieldName= x ; FieldType= y } ) qd.Headers qd.FieldTypes

    let sameValues (left:Row) (right:Row) =
        List.map2 (fun x y -> x.MaybeFloat = y.MaybeFloat && x.MaybeString = y.MaybeString) left.Cells right.Cells
            |> List.forall (fun x -> x)

    let cellToJsonPart prop cell =
        let valString=
            match prop.FieldType with
                | Booly -> cellToString cell |> (fun str -> str.ToLower())
                | Inty -> cellToInt cell |> sprintf "%i"
                | Floaty | Duration -> cellToFloat cell |> sprintf "%f"
                | Stringy | Datey -> cellToString cell |> sprintf "\"%s\"" 
        sprintf "\"%s\": %s" prop.FieldName valString

    let inline allOrError<'a> (lst:Result<'a,string> list) =
        let successes = 
            lst |> List.choose (fun r -> r |> (function | Ok x -> Some x | Error s -> None ) )
        if successes.Length = lst.Length then Ok successes
        else  
            match lst |> List.tryFind (fun res -> res |> (function | Error _ -> true | Ok _ -> false)) with
                | Some res -> 
                    match res with 
                        | Ok _ -> Error "Unknown error decoding list - no error found on failed operation"
                        | Error err -> Error <| sprintf "First Error: %s" err
                | None -> Error "Unknown error decoding list - no errors found on all"

let insertColumn colName fieldType defaultCell index qd = 
    let insert i a lst = lst |> List.splitAt i |> (fun (start,finish) -> start @ [ a ] @ finish )
    {   Headers= qd.Headers |> insert index colName
        FieldTypes= qd.FieldTypes |> insert index fieldType
        Rows= qd.Rows
                |> List.map (
                    fun r -> { r with Cells= r.Cells |> insert index defaultCell }
                )
    }

let compareSets (left:QueryData) (right:QueryData) =
    let max l r = if l > r then l else r
    let sortedl = left.Rows |> List.sortBy (fun x -> x.Key)
    let sortedr = right.Rows |> List.sortBy (fun x -> x.Key)
    let maxRows = max sortedl.Length sortedr.Length
    let mutable li = 0
    let mutable ri = 0
    let mutable finished = false
    let merged =
        [ while not finished do
            let left = sortedl |> List.tryItem li
            let right = sortedr |> List.tryItem ri
            let compared = 
                match left, right with
                    | Some l, Some r when l.Key = r.Key -> 
                        li <- li + 1
                        ri <- ri + 1
                        if this.sameValues l r then Same (l,r)
                        else Diff (l,r)
                    | Some l, Some r ->
                        if l.Key < r.Key then 
                            li <- li + 1    
                            LeftOnly l
                        else
                            ri <- ri + 1
                            RightOnly r
                    | Some l, None -> 
                            li <- li + 1
                            LeftOnly l
                    | None, Some r -> 
                            ri <- ri + 1
                            RightOnly r
                    | None, None -> 
                            finished <- true
                            Empty
            yield compared
        ]                       
    merged

type RowForJson = {
    Key: string
    CellVals: string list
    }
    with 
        static member fromQueryDataRow (row:Row) : RowForJson = 
            {
                Key = row.Key
                CellVals = row.Cells |> List.map cellText
            }


type QueryDataForJson = {
    Headers: string list
    FieldTypes: string list    
    RowsForJson: RowForJson list
    }
    with static member fromQueryData (qd:QueryData) : QueryDataForJson = {
                                Headers = qd.Headers
                                FieldTypes = (qd.FieldTypes |> List.map string)
                                RowsForJson = qd.Rows |> List.map RowForJson.fromQueryDataRow
                            }

let toCsvRows delim (qd:QueryData) = 
    //TODO - at the moment only intended for use with TAB ; no handling of special characters
    let buildRow = String.concat delim
    [
        for row in qd.Rows do
            let csvRow = 
                row.Cells 
                    |> List.map2 (fun ft c -> cellFormattedText ft c) qd.FieldTypes 
                    |> buildRow
            yield csvRow      
        ]
        |> List.append [ buildRow qd.Headers ] 


let queryDataFromJson (qdj:QueryDataForJson) : QueryData =
    let fieldTypes = qdj.FieldTypes |> List.map FieldType.fromString
    let cellFromJson (c:string) (fieldType:FieldType): Cell =
        match fieldType with
            | Inty -> this.stringToInty c 
            | Floaty | Duration -> this.stringToFloaty c
            | Datey -> this.stringToDatey c
            | _ -> cellFromString c
    let rows =
        qdj.RowsForJson    
            |> List.map (fun x -> { Key = x.Key; Cells= (x.CellVals |> List.mapi (fun i y -> cellFromJson y fieldTypes.[i]) ) } )
    { Headers= qdj.Headers; FieldTypes= fieldTypes; Rows= rows }

let getRowAsJson qd row =
    try
        let cellProps= this.cellProperties qd
        row.Cells
            |> List.map2 (fun prop cell -> this.cellToJsonPart prop cell) cellProps 
            |> String.concat ", "
            |> sprintf "{ %s }" 
            |> Ok
    with e -> 
        QueryData.displayString { qd with Rows= [ row ] }           
            |> sprintf "Error ecoding to Json\r\n%s\r\nData:%s" e.Message
            |> Error

let getRowAsJsonWithFieldHeaders (fieldHeaders:FieldHeader list) row =
    try
        // let cellProps= List.map2 (fun h ft -> { FieldName= h ; FieldType= ft } ) fieldHeaders.Headers fieldHeaders.FieldTypes
        row.Cells
            |> List.map2 (fun prop cell -> this.cellToJsonPart prop cell) fieldHeaders 
            |> String.concat ", "
            |> sprintf "{ %s }" 
            |> Ok
    with e -> 
        QueryData.displayString (FieldHeader.toQueryData fieldHeaders)                         
            |> sprintf "Error ecoding to Json\r\n%s\r\nData:%s" e.Message
            |> Error


let getRowAsJsonFromKey qd key =
    qd.Rows
        |> List.tryFind (fun x -> x.Key = key)
        |> (fun o_r ->   
            match o_r with
                | Some r -> getRowAsJson qd r
                | None -> Error <| sprintf "No row found with key %s" key )

let queryDataToJson qd =
    qd.Rows 
        |> List.map (getRowAsJson qd)
        |> this.allOrError
        |> Result.map (fun lines -> "[\r\n" + (String.concat ",\r\n" lines) + "\r\n]" )        
        // |> function Ok json -> printfn "%s" json ; Ok json | Error err -> printfn "%s" err ; Error err

let convertCellTypes (cells:Cell list) (fieldTypes:FieldType list) : Result<Cell list,string> =
    let errWithFieldNumber i (r:Result<Cell,string>) =
        match r with
            | Ok c -> (Some c,"")
            | Error err -> (None, (sprintf "Column %d:" (i + 1)) + err) 
    try
        if cells.Length <> fieldTypes.Length then 
            failwith (sprintf "Expecting %d fields but given %d" fieldTypes.Length cells.Length) 
        else
            let converted = 
                List.map2 (fun inCell ft -> this.stringToCell (cellText inCell) ft) cells fieldTypes
                    |> List.mapi errWithFieldNumber
            let checkErrors =
                match converted |> List.tryFind (fun (x,y) -> y <> "") with
                    | Some err -> (true, snd err)
                    | None _ -> (false, "")
            if fst checkErrors then failwith ( snd checkErrors )
            else Ok (converted |> List.map (fun (x,_) -> Option.get x))
    with
        | err -> Error err.Message

let getColumn headerName (qd:QueryData) =
    match qd.Headers |> List.tryFindIndex (fun h -> h = headerName) with
        | Some i ->
            qd.Rows |> List.map (fun r -> r.Cells.[i])
        | None ->
            []

let selectColumnsInOrder (newColumnOrder:string list) (qd:QueryData) =
    let thrd (_,_,i) = i
    let mapCols =
        newColumnOrder 
            |> List.mapi (fun i colName -> colName, i , qd.Headers |> List.tryFindIndex (fun h -> h = colName) )
            |> List.groupBy (fun (colName,i,onewi) -> colName)
            |> List.map (
                fun (colGroup,tups) ->
                    match tups |> List.tryHead with
                        | Some (colName,inew,oinow) -> (colName,inew,oinow) |> Some                       
                        | None -> None
            )
            |> List.choose (
                fun otup ->
                    match otup with 
                        | Some (c,i,oinow) ->
                            match oinow with 
                                | Some inow -> Some (c,i,inow)
                                | None -> None
                        | None -> None
            )
            // |> List.filter (fun (_,_,oinow) -> oinow |> Option.defaultValue -1 |> (fun i -> i > -1 ) )
            |> List.sortBy (fun (_,inew,_) -> inew)
    {   Headers = mapCols |> List.map (fun (_,_,inow) -> qd.Headers.[inow] )
        FieldTypes = mapCols |> List.map (fun (_,_,inow) -> qd.FieldTypes.[inow] )
        Rows =
            qd.Rows
                |>  List.map (
                    fun r -> {
                        Key= r.Key 
                        Cells = mapCols 
                                    |> List.map (fun (_,_,inow) -> r.Cells.[inow]) 
                    }
                )
    }





