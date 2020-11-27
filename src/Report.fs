module Components.Report

open System
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Fable.Import
open Components.QueryData
open Components

module List =
    let prepend a b = List.append b a


module ColumnAction =
    let GROUP_BY_ID = "repGroupBy"
    let AGGREGATE_ID = "repAggregate"
    let UNALLOCATED_ID = "repUnallocated"

type AggregateType =
    | NotAggregated = -1
    | Count = 0
    | Sum = 1
    | Average = 2

module AggregateType =
    let fromString str =
        try
            Enum.Parse(typedefof<AggregateType>,str) :?> AggregateType |> Ok
        with err -> 
            Browser.Dom.console.error err
            Error err
    let name enm = Enum.GetName(typedefof<AggregateType>,enm)

// type CalculatedColumn =
//     | Float of (float -> float)

type SummaryColumn = {
    ColumnName: string
    AggregateType: AggregateType
    FieldType: FieldType
}
    with
        static member createUnassigned name fieldType = { ColumnName = name ; AggregateType = AggregateType.NotAggregated ; FieldType= fieldType }
        static member createNewAggregated name fieldType= 
                        match fieldType with
                            | Inty | Floaty | Duration -> 
                                { ColumnName= name ; AggregateType= AggregateType.Sum ; FieldType= fieldType }        
                            | Stringy | Datey | Booly -> 
                                { ColumnName= name ; AggregateType= AggregateType.Count ; FieldType= Inty }   
        static member createNewAggregatedOf name fieldType aggType =
                        match fieldType with
                            | Inty | Floaty | Duration -> 
                                { ColumnName= name ; AggregateType= aggType ; FieldType= fieldType }        
                            | Stringy | Datey | Booly -> 
                                { ColumnName= name ; AggregateType= aggType ; FieldType= Inty }   

        static member header ac =   match ac.AggregateType with
                                            | AggregateType.Count -> sprintf "Count of %s"
                                            | AggregateType.Average -> sprintf "Average %s"
                                            | AggregateType.Sum -> sprintf "Total %s"
                                            // | AggregateType.NotAggregated -> sprintf "%s"
                                            | _ -> sprintf "%s"
                                    |> (fun spr -> spr ac.ColumnName)

// type ButtonKey =
//     | Named of string
//     | Aggregate of SummaryColumn

type DisplayParameter = {
    ParamName: string
    ParamValue: string
}

type ReportLayout = {
    GroupByColumns: SummaryColumn list
    AggregateColumns: SummaryColumn list
    ShowDetail: bool
    EditorExpanded: bool
    Hideable: bool
    Hidden: bool
}
    with 
        static member empty= { GroupByColumns= [] ; AggregateColumns= [] ; ShowDetail= true ; EditorExpanded= false ; Hideable= true ; Hidden= false }

        static member parse (qd:QueryData) str = 
            //format= "Group:Col1,Col2;Aggregate:Count(Col3),Sum(Col4),Average(Col5);ShowDetail;Expanded;Hidden"
            let readCol (colstr:string) =
                let getFieldType colName =
                    qd.Headers |> List.tryFindIndex (fun c -> c = colName) |> Option.bind (fun i -> qd.FieldTypes |> List.tryItem i)
                match colstr.Split([| '(' ; ')' |], StringSplitOptions.RemoveEmptyEntries) |> Array.filter (fun l -> l.Length > 0) with
                    | arr when arr |> Array.length = 1 ->
                        match arr |> Array.tryHead, arr |> Array.tryHead |> Option.bind getFieldType with
                            | Some col, Some ft -> 
                                Some {  ColumnName= col
                                        AggregateType= AggregateType.NotAggregated
                                        FieldType= ft  }
                            | _, _ -> None
                    | arr when arr |> Array.length = 2 ->    
                        // printfn "Agg Array %A" arr                    
                        match arr |> Array.tryHead |> Option.bind (fun h -> h |> AggregateType.fromString |> function | Ok a -> Some a | Error _ -> None), 
                                arr |> Array.tryItem 1, 
                                    arr |> Array.tryItem 1 |> Option.bind getFieldType with
                            | Some aggType, Some col, Some ft ->       
                                // printfn "col %A agg %A type %A" col aggType ft                          
                                Some (SummaryColumn.createNewAggregatedOf col ft aggType) //|> function | Floaty -> Floaty | _ -> Inty  }
                            | o1, o2, o3 -> 
                                // printfn "NO MATCH: %A %A %A" o1 o2 o3 
                                None
                    | _ -> None
            let readCols tag fullstr= 
                fullstr
                    |> (fun (s:string) -> s.Split(';') )
                    |> Array.tryFind (fun (l:string) -> l.Trim().StartsWith(sprintf "%s:" tag) )
                    |> Option.map (fun (l:string) -> l.Replace(sprintf "%s:" tag,"") )                    
                    |> Option.map (fun (l:string) -> l.Split(','))
                    |> Option.map (fun arr -> arr |> Array.map readCol)
                    |> Option.map (Array.choose id >> Array.toList)
                    |> Option.defaultValue []
            let findFlag (flag:string) (str:string) = str.Contains(flag)
            {   GroupByColumns= readCols "Group" str
                AggregateColumns= readCols "Aggregate" str
                ShowDetail= findFlag "ShowDetail" str
                EditorExpanded= findFlag "EditorExpanded" str
                Hideable= findFlag "Hideable" str
                Hidden= findFlag "Hidden" str 
            }

type Model = {
    ReportTitle: string
    ReportID: string
    DisplayParameters: DisplayParameter list
    ReportLayout: ReportLayout
    DataPresentation: QueryData
    Dragging: SummaryColumn option
    EditingAggregate: SummaryColumn option
    DataGrid: DataGrid.Model
}
    with static member empty=   {   ReportTitle= "Set Report Title And ID"
                                    ReportID= "SetID"
                                    DisplayParameters= []
                                    ReportLayout= ReportLayout.empty                                    
                                    DataPresentation= QueryData.empty
                                    Dragging= None
                                    EditingAggregate= None
                                    DataGrid= DataGrid.Model.defaultSettings
                                                |> DataGrid.setStyle DataGrid.GridStyleSet.Minimal
                                                |> DataGrid.setSearchBar false
                                }

type StReport = Model

type Msg =
    | LayoutChanged of ReportLayout
    | FilterChanged of DataGrid.GridFilter
    | StartDrag of SummaryColumn
    | Dropped of string
    | AggregateClicked of SummaryColumn*bool
    | ChangeLayoutEditorVisible of bool
    | ChangeShowDetail of bool
    | ChangeHidden of bool
    | ResetLayout
    | RowClicked of DataGrid.RowClickArgs
    | DataGridAction of Components.DataGrid.Msg

let update (msg:Msg) (model:Model) (qd:QueryData) =
    match msg with
        | LayoutChanged layout ->
            { model with ReportLayout= layout }, Cmd.none
        | FilterChanged filter ->
            { model with DataGrid= model.DataGrid |> DataGrid.setFilter filter }, Cmd.none
        | StartDrag btn ->
            { model with Dragging= Some btn }, Cmd.none
        | Dropped div ->
            let addToList lst a = List.append (lst |> List.filter (fun x -> x <> a)) [ a ]
            let removeFromGroupList a = model.ReportLayout.GroupByColumns |> List.filter (fun x -> x <> a)
            let removeFromAggList a = model.ReportLayout.AggregateColumns |> List.filter (fun x -> x.ColumnName <> a)
            let groupByCols, aggCols =
                // let dragName =
                //     match model.Dragging |> Option.bind  (fun b -> Some b) with
                //         | Some b ->     match b with
                //                             | Named str -> Some str
                //                             | Aggregate ac -> Some ac.ColumnName
                //         | None -> None
                match div, model.Dragging with
                    | MatchStr ColumnAction.GROUP_BY_ID true, Some col ->
                        addToList model.ReportLayout.GroupByColumns col, 
                            removeFromAggList col.ColumnName
                    | MatchStr ColumnAction.AGGREGATE_ID true, Some col ->
                        removeFromGroupList col, 
                            addToList model.ReportLayout.AggregateColumns (SummaryColumn.createNewAggregated col.ColumnName col.FieldType)
                    | MatchStr ColumnAction.UNALLOCATED_ID true, Some col ->
                        removeFromGroupList col,
                            removeFromAggList col.ColumnName
                    | _ -> model.ReportLayout.GroupByColumns, model.ReportLayout.AggregateColumns
            { model with    Dragging= None
                            ReportLayout =
                                { model.ReportLayout with   GroupByColumns = groupByCols
                                                            AggregateColumns = aggCols  }
            }, Cmd.none
        | AggregateClicked (ac,selecting) ->
            if selecting then
                let repLayout = model.ReportLayout.AggregateColumns
                                    |> List.filter (fun x -> x.ColumnName <> ac.ColumnName)
                                    |> List.prepend [ ac ]
                                    |> (fun aggs -> { model.ReportLayout with AggregateColumns= aggs } )
                { model with ReportLayout= repLayout ; EditingAggregate= None }, Cmd.none
            else { model with EditingAggregate= Some ac }, Cmd.none
        | ChangeLayoutEditorVisible b ->
            { model with ReportLayout = { model.ReportLayout with EditorExpanded= b } }, Cmd.none
        | ChangeShowDetail b -> 
            { model with ReportLayout = { model.ReportLayout with ShowDetail= b } }, Cmd.none
        | ChangeHidden b ->
            { model with ReportLayout = { model.ReportLayout with Hidden = b } }, Cmd.none
        | ResetLayout ->
            { model with ReportLayout = { model.ReportLayout with   GroupByColumns = []
                                                                    AggregateColumns = [] }
            }, Cmd.none
        | RowClicked rca ->
            //printfn "REPORT2 %A" rca
            model, Cmd.none
        | DataGridAction dmsg ->
            //printfn "REPORT1 %A" dmsg
            let mdlGrid = Components.DataGrid.update dmsg model.DataGrid qd
            { model with DataGrid= mdlGrid },
                match dmsg with
                    | DataGrid.RowClicked rca ->
                        RowClicked rca |> Cmd.ofMsg
                    | _ -> Cmd.none


module dataChanger =
    type AggregateOnColumn = { Index: int ; Op: AggregateType ; Cumulative: float}
    let listFromIndices indices lst = [
        for i in indices do
            match lst |> List.tryItem i with
                | Some x -> yield x
                | _ -> ()
    ]

    let tryFloat v = try float v |> Some with _ -> None

    let applyAggregates (ops:AggregateOnColumn list) (lst:Row list) =
        let countRow ops row = [
            for op in ops do
                match row.Cells |> List.tryItem op.Index with
                    | Some c ->
                        let f = c |> cellToFloat
                        match op.Op with
                            | AggregateType.Count -> yield { op with Cumulative = op.Cumulative + 1. }
                            | AggregateType.Sum 
                            | AggregateType.Average -> yield { op with Cumulative = op.Cumulative + f }
                            // | AggregateType.NotAggregated -> yield op
                            | _ -> yield op
                    | None -> yield op
        ]
        lst
            |> List.fold countRow ops
            |> List.map (fun op ->  if op.Op = AggregateType.Average then { op with Cumulative =      
                                                                                        if lst |> List.length = 0 then 0.
                                                                                        else op.Cumulative / (lst |> List.length |> float ) }
                                    else  op )

    let summarise (repLayout:ReportLayout) (qd:QueryData) =
        let orderedGroupBys = [
            let indexedCols = qd.Headers |> List.mapi (fun i x -> i,x)
            for groupCol in repLayout.GroupByColumns do
                for i,hname in indexedCols do
                    if groupCol.ColumnName = hname then
                        yield i
        ]
        let orderedAggs = [
            let indexedCols = qd.Headers |> List.mapi (fun i x -> i,x)
            for aggCol in repLayout.AggregateColumns do
                for i,name in indexedCols do
                    if aggCol.ColumnName = name then
                        yield i, aggCol
        ]
        let allCols =
            List.append
                ( orderedGroupBys |> List.map (fun x -> x, None) )
                ( orderedAggs |> List.map (fun x -> fst x, Some (snd x) ) )
                |> List.mapi (fun i x -> i,{| Index= fst x ; Agg= snd x |})

        let groupIndices, aggColumns =
            allCols |> List.filter (fun (_,x) -> Option.isNone x.Agg) |> List.map (fun (_,x) -> x.Index),
                allCols
                    // |> List.filter (fun (_,x) -> Option.isSome x.Agg)
                    |> List.choose (fun (_,x) -> if Option.isSome x.Agg then Some (x.Index, Option.get x.Agg) else None) //UNSAFE
                    |> List.map (fun (i,x) -> { Index=i ; Op= x.AggregateType ; Cumulative= 0. })
        // printfn "%A" allCols
        // printfn "%A" groupIndices
        // printfn "%A" aggColumns
        let rows =
            qd.Rows
                |> List.groupBy (fun (r) -> r.Cells |> listFromIndices groupIndices )
                |> List.map (fun (grps,rows) ->
                                    let aggCells = applyAggregates aggColumns rows
                                                    |> List.map (fun op -> op.Cumulative |> cellFromFloat)
                                    let cells =List.append grps aggCells
                                    { Key= grps |> comparableCells ; Cells= cells })
        let headers =
            listFromIndices orderedGroupBys qd.Headers
                |> List.prepend (orderedAggs |> List.map (fun (i,x) -> SummaryColumn.header x ) )
        //now get the groups
        QueryData.empty
            |> QueryData.setHeaders headers
            |> QueryData.setFieldTypes (listFromIndices orderedGroupBys qd.FieldTypes |> List.prepend (List.map (fun (_,x) -> x.FieldType) orderedAggs ) )
            |> QueryData.setRows rows
            // |> (fun x -> printfn "%A" x ; x )

module exporter =
    let inline toCsvFile (lst:string list) =
        sprintf "data:text/csv;charset=utf-8,"  //;filename=%s," fileName
            + (lst |> String.concat "\n")

    let downloadFile delim qd fileName =
        let contents = QueryData.toCsvRows delim qd |> toCsvFile
        // let openFile =
        //     Fable.Core.JS.encodeURI contents
        //         |> (fun x -> Browser.Dom.window.``open``(x,"_blank") )
        // console.log(openFile)
        let link = Browser.Dom.document.createElement("a")
        link.setAttribute("download",fileName)
        link.setAttribute("href",contents)
        link.click()

module this =
    let splitCamelCase (str:string) =
        str.ToCharArray()
            |> Array.fold (fun s c -> match Char.IsUpper c, fst s |> String.length, snd s |> String.length with
                                        | _, 0, 0 -> "", sprintf "%c" (c |> Char.ToUpper)
                                        | true, _, _ -> fst s, (snd s) + (sprintf "%c" c)
                                        | false, 0, _ -> (fst s) + (snd s) + (sprintf "%c" c), ""
                                        | false, _, 0 -> (fst s) + (sprintf "%c" c), ""
                                        | false, _, _ -> (fst s) + " " + (snd s) + (sprintf "%c" c), "" )
                            ("","")
            |> (fun (x,y) -> x + " " + y)

    let showTableHeaders (data:QueryData)dispatch=
        thead [ Class "thead-light" ] [
            tr [ ]
                ( data.Headers
                    |> List.map (fun x ->
                            th [] [ str (splitCamelCase x) ] )
                )
        ]

    let showRows (data:QueryData) dispatch =
        let showDataRow (r:Row) =
            tr [
            ] (
                r.Cells
                    |> List.map (fun (x) ->
                            td [] [ str (QueryData.cellText x) ]
                        )
            )
        data.Rows
        |> List.map showDataRow


    let selectAggregateTypeView ac dispatch =
        let columnButton agg colName  =
            button [    Class "btn btn-outline-primary mr-1 mb-1"
                        Draggable true
                        Id colName
                        OnClick (fun _ -> AggregateClicked (agg,true) |> dispatch)
            ] [ str colName ]
        let options =
            [ AggregateType.Count ; AggregateType.Sum ; AggregateType.Average ]
                |> List.map (fun x -> { ColumnName= ac.ColumnName ; AggregateType= x ; FieldType= ac.FieldType } )

        div [ Class "container" ] [
            div [ Class "row" ] [
                div [ Class "card col-12" ] [
                    div [ Class "card-header" ] [
                        h5 [ ] [ str "Select Summary Type" ]
                    ]
                    div [ Class "card-body" ] (
                        options |> List.map (fun x -> SummaryColumn.header x |> columnButton x )
                    )
                ]
            ]
        ]

    let layoutDesignCollapsedView model (dataView:QueryData) dispatch =
        div [ ] [
            div [ Class "row" ] [
                div [ Class "col-8" ] [
                    h2 [ Class <| if model.ReportLayout.Hidden then "text-secondary text-left" else "text-primary text-left" ] [ str model.ReportTitle ]
                    (if not model.ReportLayout.Hidden then
                        div [ Class "row" ] (
                            model.DisplayParameters
                                |> List.map (
                                    fun dp -> [
                                            div [ Class "col-2" ] [ p [ Class "text-secondary" ] [ str dp.ParamName ] ]
                                            div [ Class "col-2 text-right" ] [ p [ ] [ str dp.ParamValue ] ]
                                        ]
                                )
                                |> List.collect id                        
                        )
                    else div [] [] )
                ]
                div [ Class "col-4 text-right" ] [
                    // button [
                    //     Class "btn btn-sm btn-outline-secondary mb-1 mr-1"
                    //     OnClick (fun _ ->
                    //                 let fname = model.ReportTitle.Replace(" ","") + "_" + DateTime.Now.ToString("yyyyMMdd_HHmm") + ".csv"
                    //                 exporter.downloadFile "\t" dataView fname )
                    // ] [ str "Export" ]
                    button [
                        Class "btn btn-sm btn-outline-secondary mb-1 mr-1"
                        OnClick (fun _ -> ChangeHidden (not model.ReportLayout.Hidden) |> dispatch )
                    ] [ str <| if model.ReportLayout.Hidden then "Show" else "Hide" ]
                    button [
                        Class "btn btn-sm btn-outline-secondary mb-1 mr-1"
                        OnClick (fun _ ->
                                    let fname = model.ReportTitle.Replace(" ","") + "_" + DateTime.Now.ToString("yyyyMMdd_HHmm") + ".csv"
                                    exporter.downloadFile "\t" dataView fname )
                    ] [ str "Export" ]
                    button [
                        Class "btn btn-sm btn-outline-secondary mb-1 mr-1"
                        OnClick (fun _ -> Browser.Dom.window.print() )
                    ] [ str "Print" ]
                    button [
                        Class "btn btn-sm btn-outline-secondary mb-1 mr-1"
                        OnClick (fun _ -> ChangeLayoutEditorVisible true |> dispatch )
                    ] [ str "Change Layout" ]
                ]
            ]
        ]

    let layoutDesignView model (data:QueryData) dispatch =
        let columnButton sumCol  =
            let displayText = sumCol.AggregateType 
                                |> (function    | AggregateType.NotAggregated -> sumCol.ColumnName
                                                | _ -> sprintf "%s of %s" (AggregateType.name sumCol.AggregateType) sumCol.ColumnName ) 
            button [    Class "btn btn-outline-secondary mr-1 mb-1"
                        Draggable true
                        Id sumCol.ColumnName
                        OnDragStart (fun e -> Msg.StartDrag sumCol |> dispatch )
                                        // match sumCol.AggregateType with
                                        //     | Some aggCol -> Msg.StartDrag (Aggregate aggCol) |> dispatch
                                        //     | NotAggregated -> Msg.StartDrag (Named colName) |> dispatch
                                        //)
                        (match sumCol.AggregateType with
                            | AggregateType.Sum 
                            | AggregateType.Count 
                            | AggregateType.Average ->
                                OnClick (fun _ -> AggregateClicked (sumCol,false) |> dispatch)
                            // | AggregateType.NotAggregated -> OnClick (fun _ -> () )
                            | _ -> OnClick (fun _ -> () )
                        )
            ] [ str displayText ]
        let freeColumns = [
            for h,ft in List.map2 (fun h ft -> h,ft ) data.Headers data.FieldTypes do
                let mutable taken = false
                for y in model.ReportLayout.GroupByColumns
                            |> List.append (model.ReportLayout.AggregateColumns |> List.map (fun x -> x) ) do
                    if h = y.ColumnName then taken <- true
                if not taken then yield SummaryColumn.createUnassigned h ft
        ]
        div [ Class "container-fluid border border-primary rounded" ] [
            div [ Class "row mt-1 p-1" ] [
                div [ Class "col-6" ] [
                    h4 [ Class "text-primary" ] [ str <| sprintf "Report Layout Editor - %s" model.ReportTitle ]
                ]
                div [ Class "col-2" ] [
                    div [   Class "form-group form-check" 
                    ] [
                        div [ Class "d-inline"; Style [ PaddingRight "20px" ] ] [ str "" ]
                        input [ Type "checkbox"
                                Class "form-check-input form-control-sm"
                                Checked (model.ReportLayout.ShowDetail)  
                                OnChange (fun _ -> ChangeShowDetail (not model.ReportLayout.ShowDetail) |> dispatch )
                        ]
                        // , label [for (id_ name) ] [text name]
                        label [
                            OnChange (fun _ -> ChangeShowDetail (not model.ReportLayout.ShowDetail) |> dispatch )
                        ] [ str "Show Detail" ]
]               ]
                div [ Class "col-4 text-right" ] [
                    button [    Class "btn btn-outline-info mr-1 mb-1"
                                OnClick (fun _ -> ResetLayout |> dispatch )
                    ] [ str "Reset" ]
                    button [    Class "btn btn-outline-danger mr-1 mb-1"
                                OnClick (fun _ -> ChangeLayoutEditorVisible false |> dispatch )
                    ] [ str "Close" ]
                ]
            ]
            div [ Class "row p-1" ] [
                div [ Class "card col-12" ] [
                    div [ Class "card-header" ] [
                        h5 [] [ str "Select columns" ]
                    ]
                    div [ Class "card-body" ] [
                        div [   Class "row"
                                Style [ MinHeight "40px" ]
                                Id ColumnAction.UNALLOCATED_ID
                                OnDragEnter (fun e -> e.preventDefault() )
                                OnDragOver (fun e -> e.preventDefault() )
                                OnDrop (fun e ->
                                            let targetid = e.target?id
                                            Msg.Dropped e.target?id |> dispatch )
                        ] (
                            freeColumns |> List.map columnButton
                        )
                    ]
                ]
            ]
            div [ Class "row p-1" ] [
                div [ Class "card col-6" ] [
                    div [ Class "card-header" ] [
                        h5 [] [ str "Group By" ]
                    ]
                    div [ Class "card-body" ] [
                        div [   Class "row" ; Style [ MinHeight "40px" ]
                                Id ColumnAction.GROUP_BY_ID
                                OnDragEnter (fun e -> e.preventDefault() )
                                OnDragOver (fun e -> e.preventDefault() )
                                OnDrop (fun e ->
                                            let targetid = e.target?id
                                            Msg.Dropped e.target?id |> dispatch )
                        ] (
                            model.ReportLayout.GroupByColumns |> List.map columnButton
                        )
                    ]
                ]
                div [ Class "card col-6" ] [
                    div [ Class "card-header" ] [
                        h5 [] [ str "Aggregate" ]
                    ]
                    div [ Class "card-body" ] [
                        div [   Class "row" ; Style [ MinHeight "40px" ]
                                Id ColumnAction.AGGREGATE_ID
                                OnDragEnter (fun e -> e.preventDefault() )
                                OnDragOver (fun e -> e.preventDefault() )
                                OnDrop (fun e ->
                                            let targetid = e.target?id
                                            Msg.Dropped e.target?id |> dispatch )
                        ] (
                            model.ReportLayout.AggregateColumns |> List.map columnButton
                        )
                    ]
                ]
            ]
        ]

    let filterBar model (qd:QueryData) dispatch =
        let ALL_FIELDS= "All Fields"
        let options = List.append [ ALL_FIELDS ] qd.Headers
        let getFilterString gf =
            match gf with
                | DataGrid.OnAll str -> str
                | DataGrid.OnColumn (_,str) -> str
        let getColumnName gf =
            match gf with
                | DataGrid.OnAll _ -> None
                | DataGrid.OnColumn (str,_) -> Some str
        let getGridFilterFromColumn opt filter =
            if opt = ALL_FIELDS then DataGrid.OnAll filter
            else DataGrid.OnColumn (opt,filter)
        let getGridFilterFromFilter gf filter =
            match gf with
                | DataGrid.OnAll _ -> DataGrid.OnAll filter
                | DataGrid.OnColumn (col,_) -> DataGrid.OnColumn (col,filter)
        let toOption (key,value,select) =
            if select then option [ Value key; Selected true ] [ str value ]
            else option [ Value key ] [ str value ]
        let columnValue = getColumnName model.DataGrid.State.Filter |> Option.defaultValue ALL_FIELDS
        let filterValue = getFilterString model.DataGrid.State.Filter
        div [] [
            div [ Class "row mt-2 mb-3" ] [
                div [ Class "col-2" ] [
                    h5 [] [ str "Filter" ]
                ]
                div [ Class "col-6" ] [
                    input [     Type "text"
                                Id "reportSearch"
                                Placeholder "Search for...."
                                Class "form-control"
                                OnChange (fun e ->
                                            let filter= getGridFilterFromFilter (model.DataGrid.State.Filter) e.Value
                                            FilterChanged filter |> dispatch)
                    ]
                ]
                div [ Class "col-4" ] [
                    div [ Class "row" ] [
                        div [ Class "col-6" ] [
                            h5 [ Class "text-secondary" ] [ str "Filter On:" ]
                        ]
                        div [ Class "col-6" ] [
                            select [    Id "col-selector"
                                        Class "form-control form-control-sm" //Value columnValue;
                                        OnChange (fun e -> FilterChanged (getGridFilterFromColumn e.Value filterValue)  |> dispatch) ]
                                ( options |> List.map (fun x -> toOption (x,x,x = columnValue)) )
                        ]
                    ]
                ]
            ]
        ]

let view model (qd:QueryData) dispatch =
    let headers data =  this.showTableHeaders data dispatch
    //let qdSummary =
    let isAggregated = (model.ReportLayout.AggregateColumns |> List.length) + (model.ReportLayout.GroupByColumns |> List.length) > 0
    let visibleQd =
        if isAggregated then
            dataChanger.summarise model.ReportLayout qd
        else qd
    div [ Class "container-fluid" ] [
        (if model.ReportLayout.EditorExpanded then
            (match model.EditingAggregate with
                | Some ac ->
                    this.selectAggregateTypeView ac dispatch
                | None ->
                    this.layoutDesignView model qd dispatch
            )
        else 
            let qdForExport = if model.ReportLayout.ShowDetail then qd else visibleQd 
            this.layoutDesignCollapsedView model qdForExport dispatch )
        (if model.ReportLayout.Hidden then
            div [] []
        else
            div [] [
                this.filterBar model visibleQd dispatch
                // Components.DataGrid.view (model.DataGrid |> DataGrid.setFilter model.ReportLayout.Filter) visibleQd (DataGridAction >> dispatch)
                (if model.ReportLayout.ShowDetail && isAggregated then
                    let groupByColumns = model.ReportLayout.GroupByColumns |> List.map (fun x -> x.ColumnName)
                    Components.DataGrid.viewWithDetail model.DataGrid groupByColumns [ visibleQd ; qd ] (DataGridAction >> dispatch) 
                else if isAggregated then
                    let dataGrid = model.DataGrid |> Components.DataGrid.setHiddenCols []
                    Components.DataGrid.view dataGrid visibleQd (DataGridAction >> dispatch) 
                else
                    Components.DataGrid.view model.DataGrid visibleQd (DataGridAction >> dispatch) )
            ]
        )
    ]

module StReport =
    let setTitleAndId title id model =
        { model with ReportTitle= title ; ReportID = id }

    let setParameters (ps:DisplayParameter list) model = 
        { model with DisplayParameters= ps }

    let setLayout layout model = 
        { model with ReportLayout= layout }
    
    let hideColumns cols model =
        { model with DataGrid= DataGrid.setHiddenCols cols model.DataGrid } 

    let view = view


