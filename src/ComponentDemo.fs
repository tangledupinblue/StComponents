module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Import
open Thoth.Json
open Components.QueryData
open Components.DataGrid
open Components.FormHolders
open Components.Navbar
open Components.Report

// open System.Transactions

// MODEL

module Const =
    let GRID = "Grid"
    let REPORT = "Report"
    let COMPONENTS = "Other Components"
    let DASHBOARD= "Experiments"


type ViewState = 
    | Components
    | Grid
    | Report
    | HtmlDashboard

type GridDemoSettings = {
    MultiSelect: bool
    ShowCheckBox: bool
    ShowColumns: string
    StylePresets: int
}
    with static member empty= { MultiSelect= false ; ShowCheckBox= false; ShowColumns= "" ; StylePresets= 1 }


type Model = {
        ViewState: ViewState
        Navbar: Components.Navbar.Model
        Key: string
        DataGrid: Components.DataGrid.Model
        GridData: QueryData
        GridSettingsForm: FormHolder
        ComponentSettingsForm: FormHolder
        ReportModel: Components.Report.Model
        Experiments: Experiments.Model
    }

type Msg =
    | Start
    | ChangePage of ViewState
    | NavbarAction of Components.Navbar.Msg
    | GridAction of Components.DataGrid.Msg
    | GridSettingsAction of Components.FormHolders.Msg
    | ReportAction of Components.Report.Msg
    | ExperimentAction of Experiments.Msg

let selectStringOptions = [
    { Key= "1" ; Value= "Option 1" }
    { Key= "2" ; Value= "Option 2" }
    { Key= "3" ; Value= "Option 3" }
]

let gridStylePresets = [
    { Key= "1" ; Value= "Light" }
    { Key= "2" ; Value= "Minimal" }
]
    
let htmlSample = """
<div>
<h1>Header 1</h1>
<h2>Header 2</h2>
<p>text goes here</text>
</div>
"""

let componentTestForm = {
    FormId= "componentsDemo"
    Caption= "More Components"
    Fields= [
        FieldHolder.create "EnterName" (String "")  |> FieldHolder.setHelp (Help (HelpIcon, Helpers.HelpText ("Name Help","Put your name here")) )
        FieldHolder.create "NameMessage" (Message "Say hello to ...")
        FieldHolder.create "SelectDate" (DateString (DateTime.Today.ToString("yyyy-MM-dd") ) )
        FieldHolder.create "SelectTime" (TimeString (DateTime.Now.ToString("HH:mm" ) ) )
        FieldHolder.create "SelectDateTime" (DateTimeString (DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss" ) ) )
                            |> FieldHolder.setLayout (Time TimeLayout.HourMinuteSecond)
        FieldHolder.create "Minutes" (TimeSpanInt (10,IntTimeSpan.empty ) )
        FieldHolder.create "Select " (SelectOption (IntKey 1,selectStringOptions) ) 
        FieldHolder.create "SelectRadioButtons" (SelectOption (StringKey "2",selectStringOptions) ) 
                            |> FieldHolder.setLayout (Select SelectLayout.RadioButtons)
        FieldHolder.create "SelectButtons" (SelectOption (StringKey "2",selectStringOptions) ) 
                            |> FieldHolder.setLayout (Select SelectLayout.Buttons)
        FieldHolder.create "SelectManyCheckBoxes" (SelectManyOptions ("2",selectStringOptions) )                             
        FieldHolder.create "SelectManyButtons" (SelectManyOptions ("2",selectStringOptions) )                             
                            |> FieldHolder.setLayout (SelectMany SelectManyLayout.Buttons)
        FieldHolder.create "HtmlText" (Text htmlSample)
        FieldHolder.create "HtmlPreview" (HtmlPreview htmlSample)
    ]
    ValidationMessages= ValidationMessages.none
    Validations= []
    OnRefresh= Some
        (fun _ formHolder ->
            let name= FormHolder.valueAsString formHolder "EnterName"
            let html= FormHolder.valueAsString formHolder "HtmlText"
            [   FieldHolder.create "NameMessage" (Message (sprintf "Hello %s" name) )
                FieldHolder.create "HtmlPreview" (HtmlPreview html)
            ]
                |> List.fold (
                    fun formHolder next -> 
                        Components.FormHolders.Update.replaceField formHolder next
                )   formHolder   
        )
}

let gridSettingsForm (setts:GridDemoSettings) (cols:string list) = {
    FormId= "gridDemo"
    Caption= "Grid Settings"
    Fields= [
        FieldHolder.create "MultiSelect" (Bool setts.MultiSelect)
        FieldHolder.create "ShowCheckBox" (Bool setts.ShowCheckBox)
        FieldHolder.create "ShowColumns" (BitString (new String('1',cols.Length) , cols) ) 
        FieldHolder.create "StylePresets" (SelectOption (StringKey "1",gridStylePresets)) //(SelectString ("1",gridStylePresets) )
    ]
    ValidationMessages= ValidationMessages.none
    Validations= []
    OnRefresh= None
}

let dummyData = {
        Headers = [ "FirstName"; "LastName"; "Hidden" ; "Age" ; "Last Shower" ]
        FieldTypes  = [ Stringy; Stringy; Stringy ; Inty ; Duration ] 
        Rows = [ 
            { Key= "1"; Cells= [ cellFromString "bob"; cellFromString "jones";   cellFromString "HIDE" ; cellFromFloat 24.      ; cellFromFloat <| 71.  ] }
            { Key= "2"; Cells= [ cellFromString "baz"; cellFromString "james";   cellFromString "HIDE" ; cellFromFloat 25.      ; cellFromFloat <| 171.  ] }
            { Key= "3"; Cells= [ cellFromString "baz"; cellFromString "jones";   cellFromString "HIDE" ; cellFromFloat 30.      ; cellFromFloat <| 271.  ] }
            { Key= "4"; Cells= [ cellFromString "jim"; cellFromString "jones";   cellFromString "HIDE" ; cellFromFloat 40.      ; cellFromFloat <| 71.  ] }
            { Key= "5"; Cells= [ cellFromString "jim"; cellFromString "bob";     cellFromString "HIDE" ; cellFromFloat 50.      ; cellFromFloat <| 7.  ] }
            { Key= "6"; Cells= [ cellFromString "billy"; cellFromString "ray";   cellFromString "HIDE" ; cellFromFloat 60.      ; cellFromFloat <| 721.  ] }
            { Key= "7"; Cells= [ cellFromString "baz"; cellFromString "ray";     cellFromString "HIDE" ; cellFromFloat 25.      ; cellFromFloat <| 711.  ] }
            { Key= "8"; Cells= [ cellFromString "baz"; cellFromString "crocket"; cellFromString "HIDE" ; cellFromFloat 30.      ; cellFromFloat <| 1.  ] }
            { Key= "9"; Cells= [ cellFromString "jim"; cellFromString "crocket"; cellFromString "HIDE" ; cellFromFloat 40.      ; cellFromFloat <| 61.  ] }
        ]
    }

// UPDATE

let hiddenCols (str:string,lst:string list) =
    str.ToCharArray()
        |> Array.toList
        |> List.map2 (fun x y -> (x,y) ) lst
        |> List.filter (fun (_,y) -> y = '0' )
        |> List.map (fun (x,_) -> x)

let (|StrEq|) str1 str2 = str1 = str2

let update (msg:Msg) (model:Model) =
    let msgStr = msg |> string
    let msgOnly = msgStr |> (fun s -> s.Split([| '{' |]) ) |> Array.tryHead |> Option.defaultValue "Cant Read"
    printfn "%s %s" (DateTime.Now.ToString("HH:mm:ss")) msgOnly    
    match msg with
    | Start -> model, Cmd.none
    | ChangePage vs -> 
        { model with ViewState = vs }, Cmd.none
    | NavbarAction nmsg -> 
        model, 
            match nmsg with
                | Components.Navbar.Msg.NavbarClicked nitm ->
                    match nitm.Id with
                        | StrEq Const.GRID true -> ChangePage Grid |> Cmd.ofMsg
                        | StrEq Const.REPORT true -> ChangePage Report |> Cmd.ofMsg
                        | StrEq Const.COMPONENTS true -> ChangePage Components |> Cmd.ofMsg
                        | StrEq Const.DASHBOARD true -> ChangePage HtmlDashboard |> Cmd.ofMsg
                        | _ -> Cmd.none
    | GridAction gridMsg ->
        let gmod = Components.DataGrid.update gridMsg model.DataGrid model.GridData
        { model with DataGrid= gmod }, 
            Cmd.none
    | GridSettingsAction smsg ->
        let mdlGrid = updateValue smsg model.GridSettingsForm
        let mdlComponents = updateValue smsg model.ComponentSettingsForm
        let setts =     match formToType<GridDemoSettings> mdlGrid with
                            | Ok s -> s
                            | _ -> GridDemoSettings.empty
        Console.WriteLine(setts)
        let layout = { model.DataGrid.Layout with   StyleSet= if setts.StylePresets = 2 then GridStyleSet.Minimal else GridStyleSet.Light
                                                    Multiselect= setts.MultiSelect
                                                    SelectedCheckBox= setts.ShowCheckBox }        
        { model with    ComponentSettingsForm= mdlComponents
                        GridSettingsForm= mdlGrid
                        DataGrid= { model.DataGrid with Layout = layout } 
                                        |> Components.DataGrid.setHiddenCols (hiddenCols (setts.ShowColumns,model.GridData.Headers) )
            }, Cmd.none
    | ReportAction rmsg ->
        let mdl,_ = Components.Report.update rmsg model.ReportModel model.GridData
        { model with ReportModel= mdl }, Cmd.none
    | ExperimentAction dmsg -> 
        let mdl,cmd = Experiments.update dmsg model.Experiments
        { model with Experiments= mdl }, cmd |> Cmd.map ExperimentAction

let init() = 
    let appkey = Browser.WebStorage.localStorage.getItem("storageKey").ToString()
    update Start {  ViewState= HtmlDashboard //Components //Report
                    Navbar= [ Const.GRID ; Const.REPORT ; Const.COMPONENTS ; Const.DASHBOARD ]
                                |> List.map NavbarItem.fromString
                                |> StNavbar.fromItems "StComponents"                               
                    Key = appkey
                    DataGrid= Components.DataGrid.Model.defaultSettings
                    GridData= dummyData
                    GridSettingsForm= gridSettingsForm GridDemoSettings.empty dummyData.Headers 
                    ComponentSettingsForm= componentTestForm
                    ReportModel= StReport.empty 
                                    |> StReport.setTitleAndId "Demo Report" "DemoReport"
                                    |> StReport.hideColumns [ "Hidden" ]
                    Experiments= Experiments.Model.empty
                    }

let view (model:Model) dispatch =
    div [ Class "container-fluid" ] [ 
        Components.Navbar.View.mainNavBar model.Navbar (NavbarAction >> dispatch)
        h2 [ Class "text-primary text-center mt-2 mb-2" ] [ str "St-Components Demo" ]
        match model.ViewState with
            | Grid -> 
                div [] [
                    Components.FormHolders.View.form model.GridSettingsForm (GridSettingsAction >> dispatch)
                ]
                div [] [
                    Components.DataGrid.view model.DataGrid model.GridData (GridAction >> dispatch)
                ]
            | Report -> 
                div [] [
                    StReport.view model.ReportModel model.GridData (ReportAction >> dispatch)
                ]
            | Components -> 
                div [] [
                    Components.FormHolders.View.form model.ComponentSettingsForm (GridSettingsAction >> dispatch)
                ]
            | HtmlDashboard ->
                div [] [
                    Experiments.view model.Experiments (ExperimentAction >> dispatch)
                ]
    ]

// App
//Program.mkSimple init update view
Program.mkProgram init update view
    |> Program.withReactBatched "elmish-app"
    //|> Program.withConsoleTrace
    |> Program.run


