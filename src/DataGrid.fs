module Components.DataGrid

open System
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Fable.Import
open Components.QueryData

type GridStyleSet= {    //these are bootstrap style appendages to any layout style classes
    SearchBarButton: string
    ActiveRow: string
    SelectedRow: string
    TableHead: string
    GroupRowHeader: string
    Table: string
    SearchBar: string
}
    with 
        static member Light= {
                        SearchBarButton= "btn btn-info btn-sm mr-1"
                        ActiveRow= "bg-primary text-white"
                        SelectedRow= "bg-secondary text-white"
                        TableHead= "thead-light"
                        GroupRowHeader= "text-primary"
                        Table= "table table-sm table-striped table-hover"
                        SearchBar= "navbar navbar-dark bg-dark navbar-expand-sm fixed-bottom"
                    }
        static member Minimal= {
                        SearchBarButton= "btn btn-info btn-sm mr-1"
                        ActiveRow= "bg-light text-primary"
                        SelectedRow= "bg-light text-primary"
                        TableHead= "text-primary"
                        GroupRowHeader= "text-info bg-light font-weight-bold"
                        Table= "table table-sm table-hover"
                        SearchBar= "navbar navbar-expand-sm fixed-bottom"
                    }

type SortOrder = 
    | NoSort
    | Asc
    | Desc

type SortableColumn = {
    ColumnName: string
    SortOrder: SortOrder
}
    with 
        static member Asc colName = { ColumnName= colName ; SortOrder= Asc }
        static member Desc colName = { ColumnName= colName ; SortOrder= Desc }


type ColumnHeader = {
    Index: int
    ColumnName: string
    FieldType: FieldType
    Visible: bool
}


type GridButton = {
    Id: string
    Text: string
    Class: string
}
    with 
        static member fromString str= { Id= str ; Text= str ; Class= "btn-outline-secondary" }
        static member fromTuple (tup:string*string)= { Id= fst tup ; Text= fst tup ; Class= snd tup } 

type RowClickArgs = {
    //RecordKey: string
    // Headers: FieldHeaders
    Headers: ColumnHeader list
    Row: Row
    Height: float
}
    //with static member create (key:string,height:float) = { RecordKey= key ; Height= height }

type SingleButtonClickArgs = {
    ButtonId: string
    Row: Row
    Height: float
}
    with static member create (buttonid:string,row:Row,height:float) = { ButtonId= buttonid ; Row= row ; Height= height }


type MultiButtonClickArgs = {
    ButtonId: string
    RecordKeys: string list
    Height: float
}
    with static member create (buttonid:string,keys:string list,height:float) = { ButtonId= buttonid ; RecordKeys= keys ; Height= height }

type GridFilter = 
    | OnAll of string
    | OnColumn of (string*string)

type Msg =
    | FilterChanged of GridFilter
    | RowClicked of RowClickArgs //string*float    //height of the click for returning
    | RowDoubleClicked of RowClickArgs //string*float  //height of the click for returning
    | HeaderClicked of string
    | ClearSelection
    | SelectAll
    | ClearRefresh
    | GridSingleButtonClicked of SingleButtonClickArgs //string*string    //the button id and the row key
    | GridMultiButtonClicked of MultiButtonClickArgs //string*string list   //the button id and the row keys

type Pixels = int

type GridLayout = {
    HideColumns: string list
    ShowOnlyColumns: string list
    Multiselect: bool
    ShowSearchBar: bool
    SingleRowButtons: GridButton list
    MultiRowButtons: GridButton list
    SelectedCheckBox: bool
    StyleSet: GridStyleSet
    BottomSpacing: Pixels
}
    with static member defaultSettings = {
            HideColumns= []
            ShowOnlyColumns= []
            Multiselect= false
            ShowSearchBar= true
            SingleRowButtons= []
            MultiRowButtons= []
            SelectedCheckBox= false
            StyleSet= GridStyleSet.Light
            BottomSpacing= 60
        }

type GridState = {
    Filter: GridFilter
    Freeze: bool    
    SelectedRows: string list
    ActiveRow: string option
    SortColumn: SortableColumn
    GroupColumn: SortableColumn
    LastScrollPos: float
}


type Rendering = 
    | ScrollRefresh  //refresh back to stored posiition

//type HeaderList = string list

type Behaviour = {
    RefreshOptions: Rendering list 
    HideRowButtons: (ColumnHeader list -> Row -> unit -> string list) option
}
    with static member defaultSettings = { 
                            RefreshOptions= [] 
                            HideRowButtons= None                        
                        }

type Model = {
    State: GridState
    Layout: GridLayout
    Behaviour: Behaviour
    //RefreshOptions: Rendering list
    // QueryData: QueryData
}
    with static member defaultSettings = { 
            State= { 
                Filter= OnAll "" 
                Freeze= false
                SelectedRows= []
                ActiveRow = None
                SortColumn = { ColumnName= ""; SortOrder= NoSort } 
                GroupColumn = { ColumnName= ""; SortOrder= NoSort }
                LastScrollPos= 0.
            } 
            Layout = GridLayout.defaultSettings
            Behaviour= Behaviour.defaultSettings
            //RefreshOptions= []
            // QueryData= QueryData.empty
        }


module private config =
    let functionButtons (settings:Model) dispatch = 
        let selections = 
            if settings.Layout.Multiselect then [
                    button [ 
                        Class settings.Layout.StyleSet.SearchBarButton //"btn btn-info btn-sm mr-1"   
                        Type "button"                   
                        OnClick (fun _ -> SelectAll |> dispatch)                             
                    ] [ str "Select All" ]
                    button [ 
                        Class settings.Layout.StyleSet.SearchBarButton //"btn btn-info btn-sm mr-1"
                        Type "button"                   
                        OnClick (fun _ -> ClearSelection |> dispatch) ] [ str "Clear Selection" ]
                ]
            else []
        selections

module Utils = 
    let splitCamelCase (str:string) = 
        str.ToCharArray()     
            //|> Array.map (fun c -> Char.IsUpper c, c)
            //using two strings, first is main string, second is buffer that we add to the main when we get a space
            |> Array.fold (fun s c -> match Char.IsUpper c, fst s |> String.length, snd s |> String.length with
                                        | _, 0, 0 -> "", sprintf "%c" (c |> Char.ToUpper)
                                        | true, _, _ -> fst s, (snd s) + (sprintf "%c" c)  
                                        | false, 0, _ -> (fst s) + (snd s) + (sprintf "%c" c), ""
                                        | false, _, 0 -> (fst s) + (sprintf "%c" c), "" 
                                        | false, _, _ -> (fst s) + " " + (snd s) + (sprintf "%c" c), "" )
                            ("","")
            |> (fun (x,y) -> x + " " + y)

    let evalOptional ofunc =
        ofunc |> Option.map (fun func -> func())

    


module private this =

    let getColumnHeaders headers fieldTypes hideColumns showOnlyColumns =
        if showOnlyColumns |> List.length > 0 then
            List.mapi2 (fun i h ft -> { Index= i ; ColumnName= h ; FieldType= ft ; Visible= List.exists (fun x -> x = h) showOnlyColumns })
        else
            List.mapi2 (fun i h ft -> { Index= i ; ColumnName= h ; FieldType= ft ; Visible= List.exists (fun x -> x = h) hideColumns |> not })
        |> (fun func -> func headers fieldTypes)

    let withColumnsToShow<'a> (columns:ColumnHeader list) (lst:'a list) =
        List.map2 (fun col a -> col,a) columns lst
            |> List.choose (fun (col,a) -> if col.Visible then Some a else None)

    let missingIndices (lstAll:'a list) (lst:'a list) =
        [ for (i,x) in lstAll |> List.mapi (fun i x -> i,x) do
            match lst |> List.tryFind (fun y -> x = y) with
                | Some _ -> ()
                | None -> yield i ]

    let foundIndices lstAll lst =
        [ for (i,x) in lstAll |> List.mapi (fun i x -> i,x) do
            match lst |> List.tryFind (fun y -> x = y) with
                | Some _ -> yield i
                | None -> () ]

    let filterRecords (filter:GridFilter) columns data = 
        let rowForFilter (rowCells:Cell list) = 
            rowCells |> List.map cellText |> withColumnsToShow columns |> List.toSeq |> String.concat "|" 
        let filteredRows =
            match filter with
                | OnAll fstr -> 
                    data.Rows   
                        |> List.filter (fun x -> (rowForFilter x.Cells).ToLower().Contains(fstr.ToLower()))        
                | OnColumn (col,fstr) ->
                    match data.Headers |> List.tryFindIndex (fun x -> x= col) with
                        | Some i ->
                            data.Rows   
                                |> List.filter (fun x -> (x.Cells.[i] |> cellText).ToLower().Contains(fstr.ToLower()))        
                        | None -> 
                            data.Rows
        filteredRows

    let colIndex (colName:string) (data:QueryData) =
        let mayfind = data.Headers |> List.mapi (fun i x -> i,x) |> List.tryFind (fun (i,x) -> x = colName)
        match mayfind with 
            | Some (i,x) when x <> "" -> Some i
            | _ -> None

    let groupValsForCol (data:QueryData) (groupcol:SortableColumn) = 
        //printfn "%A" groupcol
        let mayi = colIndex groupcol.ColumnName data
        //printfn "%A" mayi
        let sort = match groupcol.SortOrder with
                        | NoSort | Asc -> List.sort
                        | Desc -> List.sortDescending
        match mayi with
            | Some i -> 
                data.Rows |> List.map (fun x -> x.Cells.[i]) |> List.distinct |> sort |> Some
            | None -> None
   
    let isSelected (key:string) (state:GridState) =
        List.exists (fun x -> x = key) state.SelectedRows

    let isActive (key:string) (state:GridState) = 
        key = Option.defaultValue "" state.ActiveRow

    let matchSelectedToStyle model isSelected isActive isSummary =
        if isActive then Class model.Layout.StyleSet.ActiveRow // "bg-primary text-white"
        else if isSelected then Class model.Layout.StyleSet.SelectedRow //"bg-secondary text-white"
        else if isSummary then Class model.Layout.StyleSet.GroupRowHeader //"bg-secondary text-white"
        else Class ""
        // let matches = List.exists (fun x -> x = key) selectedRows
        // if matches then Style [ Color "white"; BackgroundColor "navy" ]
        // else Style []

    let filterMatchingRows (matchOn:Cell) (mayi:int option) (rows:Row list) =
        match mayi with
            | Some i -> rows |> List.filter (fun x -> cellText x.Cells.[i] = cellText matchOn) 
            | None ->  rows

    let sortRows (sortCol:SortableColumn) columns (rows:Row list)  = 
        let maybeIndex = 
                columns 
                    |> List.tryFind (fun x -> x.ColumnName = sortCol.ColumnName)
                    |> Option.bind (fun x -> Some x.Index)
        match maybeIndex with 
            | Some i -> 
                match sortCol.SortOrder with
                    | NoSort | Asc -> 
                        rows |> List.sortBy (fun x -> x.Cells.[i])
                    | Desc -> 
                        rows |> List.sortByDescending (fun x -> x.Cells.[i])
            | _ -> rows

    let showTableHeaders (grid:Model) (data:QueryData) columns dispatch= 
        let addSelectedCol lst = 
            if grid.Layout.SelectedCheckBox then List.append [ "" ] lst
            else lst
        thead [ Class "thead-light" ] [
            tr [ ] 
                ( data.Headers 
                    //|> withoutColumnsToHide hideIndices
                    |> withColumnsToShow columns
                    |> addSelectedCol
                    |> List.map (fun x -> 
                            th [ OnClick (fun ev -> x |> string |> HeaderClicked |> dispatch )] [ str (Utils.splitCamelCase x) ] )
                )
        ]

    let showDataRow grid (r:Row) (columns:ColumnHeader list) isactive isselected issummary dispatch =     
        let align (ft:FieldType) = 
            match ft with
                | Inty | Floaty | Duration -> TextAlign TextAlignOptions.Right //"right"
                | _ -> TextAlign TextAlignOptions.Left //"left"
        let selectedCell check = td [] [ input [ Type "checkbox" ; Checked check ] ]
        tr [ 
            matchSelectedToStyle grid isactive isselected issummary
            OnDoubleClick (fun ev ->                                    
                    let scrollPos = Browser.Dom.document.documentElement.scrollTop
                    { Headers= columns ; Row= r; Height= scrollPos } |> RowDoubleClicked |> dispatch 
            )
            OnClick (fun ev -> 
                    let scrollPos = Browser.Dom.document.documentElement.scrollTop
                    { Headers= columns ; Row= r; Height= scrollPos } |> RowClicked |> dispatch 
            )
        ] (
            r.Cells 
                // |> List.map2 (fun y z -> (y.FieldType, z)) columns
                // //|> withoutColumnsToHide hideIndices
                // |> withColumnsToShow columns
                |> List.map2 (fun col c -> col,c) columns
                |> List.choose (fun (col,c) ->
                        if col.Visible then  
                            td [ Style [ align col.FieldType ] ] [ str (cellFormattedText col.FieldType c) ]                                                                
                                |> Some
                        else None
                    ) 
                |> ( fun dataCells ->   if grid.Layout.SelectedCheckBox then 
                                            List.append [ selectedCell (isactive || isselected) ] dataCells
                                        else dataCells )
        )


    let showTableRows (rows:Row list) (grid:Model) columns dispatch =
        let numVisibleCols = columns |> List.filter (fun c -> c.Visible) |> List.length //(headers.Headers |> List.length) - (hideIndices |> List.length) 
        let showSingleButtons isactive isselected = 
            isselected && grid.State.SelectedRows.Length = 1 && grid.Layout.SingleRowButtons.Length > 0 
        let calcActiveButtons row =
                match grid.Behaviour.HideRowButtons |> Option.map (fun func -> func columns row () ) with
                    | Some hids ->  
                        [   for btn in grid.Layout.SingleRowButtons do
                                if hids |> List.contains btn.Text |> not then yield btn
                        ]
                    | None -> grid.Layout.SingleRowButtons 
        let singleButtonCell b row = 
            button [    
                Class <| "btn btn-sm mr-1 " + b.Class
                OnClick (fun _ ->   let scrollPos = Browser.Dom.document.documentElement.scrollTop
                                    (b.Id, row,scrollPos) |> SingleButtonClickArgs.create |> GridSingleButtonClicked |> dispatch )
            ] [ str b.Text ]            
        rows 
            |> sortRows grid.State.SortColumn columns
            |> List.map (
                fun x ->    let isactive, isselected = isActive x.Key grid.State, isSelected x.Key grid.State
                            List.concat [
                                [ showDataRow grid x columns isactive isselected false dispatch ]
                                (
                                    if showSingleButtons isactive isselected then 
                                        [ 
                                            tr [] [ 
                                                td [ ColSpan numVisibleCols
                                                ] ( 
                                                    let visibleButtons= calcActiveButtons x
                                                    visibleButtons |> List.map (fun b -> singleButtonCell b x) )
                                            ]
                                        ]
                                    else []
                                )
                            ]
                        )
            |> List.collect (fun x -> x)

    let groupedBody (data:QueryData) (gridState:Model) hideIndices dispatch =
        [ div [] [] ]

    let showFooter model dispatch =
        //TODO - have implemented filter on column functionality elsewhere - need to incorporate into this search bar
        let filterString str = 
            match model.State.Filter with
                | OnAll str -> str
                | OnColumn (_,str) -> str
        div [] [ 
            (if model.Layout.BottomSpacing > 0 then
                div [ Style [ Display DisplayOptions.Block ; Height (sprintf "%ipx" model.Layout.BottomSpacing) ] ] []
            else div [] [] )
            nav [ Class model.Layout.StyleSet.SearchBar ; Style [ MarginBottom 5 ] ] [            
                form [ Class "form-inline" ] (
                    List.concat [
                        [
                            input [ Type "text"; Class "form-control mr-sm-2 pull-right"; Placeholder "Search"; 
                                DefaultValue filterString;
                                OnChange (fun ev -> ev.target?value |> string |> OnAll |> FilterChanged |> dispatch)  ]
                        ]
                        (config.functionButtons model dispatch)                        
                        (if model.State.SelectedRows.Length > 1 then 
                            (   let keys = model.State.SelectedRows
                                model.Layout.MultiRowButtons 
                                    |>  List.map (fun b ->      
                                                        button [                                                                        
                                                            Class <| "btn btn-sm " + b.Class
                                                            OnClick (fun _ ->   let scrollPos = Browser.Dom.document.documentElement.scrollTop
                                                                                (b.Id, keys,scrollPos) |> MultiButtonClickArgs.create |> GridMultiButtonClicked  |> dispatch )
                                                        ] [ str b.Text ]  )          
                            )
                        else [] )                       
                    ]
                ) 
            ]
        ]


//Public Accessors

// type Model with 
    //getters for DataGrid
    // static member getSelectedKey (g: Model) =
let getSelectedKey (g: Model) =
    match g.State.SelectedRows.Length, List.tryHead g.State.SelectedRows with
        | (i,Some h) when i = 1 -> h
        | _ -> ""

    // static member getSelectedKeys (g: Model) =
let getSelectedKeys (g: Model) =
    g.State.SelectedRows

    // static member getActiveKey (g:Model) =
let getActiveKey (g:Model) =
    match g.State.ActiveRow with 
        | Some s -> s
        | None -> ""

let getVisibleRows (g:Model) (d:QueryData) = 
    d |> this.filterRecords g.State.Filter (this.getColumnHeaders d.Headers d.FieldTypes g.Layout.HideColumns g.Layout.ShowOnlyColumns)

let getColumnValueInRow (cols:ColumnHeader list) row colName = 
    match cols |> List.tryFind (fun c -> c.ColumnName = colName) with
        | Some col ->
            row.Cells |> List.tryItem col.Index |> Option.map (fun c -> Components.QueryData.cellToString c)
        | None -> None

    //setters for DataGrid
    // static member setToFreeze (freeze:bool) (settings:Model) =
let setToFreeze (freeze:bool) (settings:Model) =
    { settings with 
        State = { settings.State with Freeze = freeze } }

    // static member setFilter (filter:string) (settings:Model) = 
let setFilterAll (filter:string) (settings:Model) = 
        { settings with 
            State = { settings.State with Filter = OnAll filter } }

let setFilter (filter:GridFilter) (settings:Model) = 
        { settings with 
            State = { settings.State with Filter = filter } }

let setSelectedRow (model:Model) key =
    { model.State with SelectedRows= [ key ] }
        |> (fun state -> { model with State= state } )

let setSelectedRows keys (model:Model) = 
    { model.State with SelectedRows= keys }
        |> (fun state -> { model with State= state } )

    // static member setHiddenCols (colsToHide:string list) (settings:Model) =
let setHiddenCols (colsToHide:string list) (settings:Model) =
    { settings with 
        Layout = { settings.Layout with HideColumns = colsToHide } }

let setVisibleCols (colsToShow: string list) (settings:Model) = 
    { settings with 
        Layout = { settings.Layout with ShowOnlyColumns = colsToShow } }
 
let setSortOrder sortableColumn (settings:Model) =
    { settings with 
        State = { settings.State with SortColumn = sortableColumn } }


    // static member setMultiSelect (multiSelect:bool) (settings:Model) =
let setMultiSelect (multiSelect:bool) (settings:Model) =
    { settings with 
        Layout = { settings.Layout with Multiselect = true } }

    // static member setGroupByColumn (colName:string) (settings:Model) = 
let setGroupByColumn (colName:string) (settings:Model) = 
    { settings with
        State = { settings.State with GroupColumn = { ColumnName= colName; SortOrder= Asc } } }

    //remember scroll position and return there
    // static member setScrollRefresh (settings:Model) =        
let setScrollRefresh (settings:Model) =        
    { settings with
        Behaviour = { settings.Behaviour with RefreshOptions= [ ScrollRefresh ] }
    }

let setHideRowButtonsCalc func grid =
    { grid with
        Behaviour = { grid.Behaviour with HideRowButtons= Some func }
    }

    // static member setSingleRowButtons (btns:GridButton list) (settings: Model) =
let setSingleRowButtons (btns:GridButton list) (settings: Model) =
    { settings with 
        Layout = { settings.Layout with SingleRowButtons= btns } }

    // static member setMultiRowButtons (btns:GridButton list) (settings: Model) =
let setMultiRowButtons (btns:GridButton list) (settings: Model) =
    { settings with 
        Layout = { settings.Layout with MultiRowButtons= btns } }

    // static member setShowSelectedCheckBox (checkBox:bool) (settings:Model) = 
let setShowSelectedCheckBox (checkBox:bool) (settings:Model) = 
    { settings with 
        Layout= { settings.Layout with SelectedCheckBox= checkBox } }

let setSearchBar (showSearchBar:bool) (settings:Model) = 
    { settings with 
        Layout= { settings.Layout with ShowSearchBar= showSearchBar } }

    // static member setData (qd:QueryData) (settings:Model) =
    //     { settings with QueryData= qd }

let setStyle (styleSet:GridStyleSet) (grid:Model) =
    { grid.Layout with StyleSet= styleSet }
        |> (fun newLayout -> { grid with Layout= newLayout })    

let setBottomSpacing  pixels (grid:Model) = 
    { grid with Layout =    
                { grid.Layout with BottomSpacing= pixels } }

//type DataGrid = Model 

module Interact=
    let clickFirst (model:Model) (qd:QueryData) =
        match qd.Rows |> List.tryHead with
            | Some fst -> setSelectedRow model fst.Key
            | None -> model


module private grid = 
    let update (qd:QueryData) (msg:Msg) (gridState:Model) =
        let appendOrRemove (existing:string list) key =
            let orig = existing.Length
            let filtered = List.filter (fun x -> x <> key) existing
            if filtered.Length = orig then List.append [ key ] filtered
            else filtered             
        let addSelected key =
            if gridState.Layout.Multiselect then appendOrRemove gridState.State.SelectedRows key
            else [ key ]            
        let noRefresh gs = { gs with Behaviour= { gs.Behaviour with RefreshOptions = [] } }
        match msg with
            | FilterChanged str -> 
                //Browser.Dom.console.log("Filter Changed: " + str)
                { gridState with State = { gridState.State with Filter = str } }
            | RowClicked rca ->  
                // Console.WriteLine("Clicked: (" + rca.RecordKey + "," + rca.Height.ToString() + ")")
                let selected = addSelected rca.Row.Key
                { gridState with State = { 
                                gridState.State with 
                                    ActiveRow = if selected |> List.length = 1 then selected |> List.tryHead else None //None
                                    SelectedRows = selected //addSelected rca.RecordKey 
                                    LastScrollPos = rca.Height }
                }
            | RowDoubleClicked rca ->  
                // Console.WriteLine("Double Clicked: (" + rca.RecordKey + "," + rca.Height.ToString() + ")")
                { gridState with State = { 
                                gridState.State with 
                                        ActiveRow = Some rca.Row.Key
                                        SelectedRows = [ rca.Row.Key ] 
                                        LastScrollPos = rca.Height } 
                } |> noRefresh
            | HeaderClicked str -> 
                let sort = 
                    match gridState.State.SortColumn with
                        | { SortOrder = NoSort } -> {  ColumnName= str; SortOrder= Asc }
                        | { SortOrder = Desc } -> { ColumnName = str; SortOrder = Asc }
                        | { SortOrder = Asc } when str = gridState.State.SortColumn.ColumnName -> { ColumnName = str; SortOrder = Desc }
                        | _ -> { ColumnName = str; SortOrder = Asc }                                       
                { gridState with State = { gridState.State with SortColumn = sort } } |> noRefresh
            | SelectAll -> 
                { gridState with State = { 
                                gridState.State with 
                                    ActiveRow = None 
                                    SelectedRows = qd.Rows |> List.map (fun x -> x.Key) } 
                } |> noRefresh 
            | ClearSelection -> 
                { gridState with State = { 
                                gridState.State with 
                                    ActiveRow = None
                                    SelectedRows = [] } 
                } |> noRefresh
            | ClearRefresh -> gridState |> noRefresh
            | _ -> gridState |> noRefresh


    let view (model:Model) (data:QueryData) dispatch =
        let columns = this.getColumnHeaders data.Headers data.FieldTypes model.Layout.HideColumns model.Layout.ShowOnlyColumns
        //let hideIndices = this.hideColumnIndices model data
        let filteredRows = data |> this.filterRecords model.State.Filter columns //hideIndices
        //printfn "%A" model.State.GroupColumn
        let groupBands = this.groupValsForCol data model.State.GroupColumn
        //printfn "%A" groupBands
        //let headers = this.showTableHeaders model data hideIndices dispatch
        let headers = this.showTableHeaders model data columns dispatch
        // let showRows rows = this.showTableRows rows { Headers= data.Headers; FieldTypes= data.FieldTypes } model hideIndices dispatch
        let showRows rows = this.showTableRows rows model columns dispatch
        div (if model.Behaviour.RefreshOptions |> List.exists (fun x -> x = ScrollRefresh) then
                [ Ref (fun _ -> 
                    // Console.WriteLine("scrolling to " + model.State.LastScrollPos.ToString())
                    Browser.Dom.window.scrollTo(0.,model.State.LastScrollPos) ) ]
            else [] ) [
            div [] (   
                    match groupBands with 
                        | Some gbs ->
                            gbs 
                                |> List.map 
                                    ( fun g -> 
                                        let rowsForGroup = this.filterMatchingRows g (this.colIndex model.State.GroupColumn.ColumnName data) filteredRows
                                        if rowsForGroup.Length = 0 then text [] []
                                        else 
                                            div [] [
                                                // h5 [ Class "text-primary" ] [ str <| cellText g ]
                                                h5 [ Class model.Layout.StyleSet.GroupRowHeader ] [ str <| cellText g ]
                                                // table [ Class <| "table table-sm table-striped table-hover" ] [
                                                table [ Class <| model.Layout.StyleSet.Table ] [
                                                    headers 
                                                    tbody [] (
                                                        showRows rowsForGroup
                                                    )
                                                ]
                                            ]
                                    )
                        | None -> [
                            div [] [
                                // table [ Class <| "table table-sm table-striped table-hover" ] [
                                table [ Class <| model.Layout.StyleSet.Table ] [
                                    headers
                                    tbody [] (                                    
                                        showRows filteredRows
                                    )
                                ]
                            ]
                        ]
            )
            div [ Class "row" ] [
                div [ Class "col-12 text-right" ] [
                    p [ Class "text-secondary" ] [ str <| sprintf "Showing %i of %i records" (filteredRows |> List.length) (data.Rows |> List.length) ]
                ]
            ]
            (if model.Layout.ShowSearchBar then
                this.showFooter model dispatch
            else div [] [] )
        ]

    let rowKey (indices:int list) (row:Row) = 
        indices
            |> List.map (fun i -> row.Cells.[i])         
            |> List.map (fun x -> x |> cellText) 
            |> String.concat "|"

    let normaliseSummaryRow groupByColCount totalDetailColCount row =
        let aggCols = row.Cells.Length - groupByColCount
        [   let mutable j = 0
            for i in [ 0 .. totalDetailColCount - 1 ] do
                if i < groupByColCount then
                    yield row.Cells.[j]
                    j <- j + 1
                else if i >= totalDetailColCount - aggCols then
                    yield row.Cells.[j]
                    j <- j + 1
                else 
                    yield cellFromString "" 
        ]
            |> (fun spacedCells -> { row with Cells= spacedCells } )

    let normaliseColumns summaryColNames targetCount headerColumns = 
        // let sumCols = [
        //     for c in allColumns do
        //         match summaryColNames |> List.tryFind (fun n -> n = c.ColumnName) with
        //             | Some _ -> 
        // ]
        let sumCols = [
            for n in summaryColNames do
                match headerColumns |> List.tryFind (fun c -> n = c.ColumnName) with
                    | Some col -> yield col
                    | None -> ()
        ]
        let aggCount, sumCount = (headerColumns |> List.length) - (summaryColNames |> List.length),
                                    sumCols |> List.length
        //printfn "aggCount %i ; sumCount %i ; targetCount %i" aggCount sumCount targetCount
        [   let mutable j = 0
            for i in [ 0 .. targetCount - 1 ] do                
                if i < sumCount then
                    yield { headerColumns.[j] with Index = i }
                    j <- j + 1
                else if i >= targetCount - aggCount then
                    yield { headerColumns.[j] with Index = i }
                    j <- j + 1
                else 
                    yield { Index = i ; ColumnName= sprintf "Col%i" i ; FieldType= Stringy ; Visible= true }
        ]

    //TODO- hoping to make this recusive to support multiple levels of detail
    let viewWithDetail model (summaryCols:string list) (qds:QueryData list) dispatch =
        match qds |> List.length, qds |> List.tryHead, qds |> List.tryLast with 
            // | 1, Some h, _ -> view model h dispatch
            | i, Some summary, Some detail when i > 1 -> 
                    let matchIndices  = [
                        for i,tailHeader in detail.Headers |> List.mapi (fun i x -> i,x) do                            
                            for j,headHeader in summary.Headers |> List.mapi (fun i x -> i,x) do
                                if tailHeader = headHeader && (summaryCols |> List.exists (fun scol -> scol = headHeader) ) then 
                                    yield (j,i)
                                else ()
                    ]
                    // printfn "Match Indices"
                    // printfn "%A" matchIndices
                    let headIndices, tailIndices = 
                        matchIndices |> List.map fst, matchIndices |> List.map snd
                    let detailColumns = this.getColumnHeaders detail.Headers detail.FieldTypes model.Layout.HideColumns model.Layout.ShowOnlyColumns
                    //printfn "%A" detailColumns
                    let summaryColumns = this.getColumnHeaders summary.Headers summary.FieldTypes [] []
                                            |> normaliseColumns summaryCols (detailColumns |> List.filter (fun x -> x.Visible) |> List.length)
                    //printfn "%A" summaryColumns
                    let tailGroups = 
                        detail
                            |> this.filterRecords model.State.Filter detailColumns //hideIndices
                            |> List.groupBy (rowKey tailIndices)
                    let allRows = [
                        for sumRow in summary.Rows do 
                            let spacedRow= normaliseSummaryRow summaryCols.Length (detailColumns |> List.filter (fun x -> x.Visible) |> List.length) sumRow
                            //printfn "%s" <| rowKey headIndices sumRow
                            // printfn "%A" sumRow
                            // printfn "%A" spacedRow
                            yield this.showDataRow model spacedRow summaryColumns false false true dispatch
                            let childRows = 
                                tailGroups                                     
                                    |> List.choose (fun (key,detailrows) -> 
                                                        // printfn "%s" key
                                                        if key = rowKey headIndices sumRow then Some detailrows else None)
                                    |> List.collect (id)
                            for drow in childRows do
                                let isactive, isselected = this.isActive drow.Key model.State, this.isSelected drow.Key model.State
                                yield this.showDataRow model drow detailColumns isactive isselected false dispatch 
                    ]
                    table [ Class <| model.Layout.StyleSet.Table ] [
                        this.showTableHeaders model detail detailColumns dispatch
                        tbody [] allRows
                    ]
            | _, Some h, _ -> view model h dispatch
            | _, _, _ -> div [] []                                  

// module private cards = 
//     let view model (qd:QueryData) dispatch = 
//         div [ Class "container-fluid" ] [
//             div [ Class "row" ] [


//             ]
//         ]

let update (msg:Msg) (model:Model) (qd:QueryData) = 
    grid.update qd msg model

let view model (qd:QueryData) dispatch =
    //printfn "%A" model.State.SortColumn
    grid.view model qd dispatch

let viewWithDetail model cols qds dispatch =
    grid.viewWithDetail model cols qds dispatch

// let viewCardMode model (qd:QueryData) dispatch = 
