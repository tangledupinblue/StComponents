module Components.NavbarInserts

open System
open Elmish
open Fable.Core
open Elmish.React
open Fable.React
open Fable.React.Props
open Microsoft.FSharp.Reflection

open Navbar


type Model = {
    Branding: string
    Items: NavbarItem list
    LastSelection: NavbarItem option
    Inserts: string list
}
    with
        static member fromItems branding items = { Branding= branding ; Items= items ; LastSelection= None ; Inserts= [] }


type Msg = 
    | NavbarItemClicked of NavbarItem
    | NavbarButtonClicked of string //NavbarItem
    // | ActionClicked of ClickAction


let update (msg:Msg) (model:Model) =
    match msg with
        | NavbarItemClicked itm -> 
            { model with    LastSelection= Some itm 
                            Items= setActiveNavbarItem itm.Id model.Items
            }, Cmd.none            
        | NavbarButtonClicked _ ->
            model, Cmd.none            


let showNavbarItem (dispatch: Msg -> unit) (itm:NavbarItem) =
    let classTag = if itm.Active then " active" else if itm.Disabled then " disabled" else ""
    li [ Class "nav-item" ] [ 
        // a [ Class ( "nav-link" + classTag ); Href itm.Link; OnClick (fun e -> (NavbarClicked itm) |> trigger ) ] [ str itm.Text ]
        a [ Class ( "nav-link " + classTag ); Href "#"; OnClick (fun e -> (NavbarItemClicked itm) |> dispatch ) ] [ str itm.Text ]
    ]        

let showNavbarInsert (dispatch: Msg -> unit) (itm:NavbarItem) =
    let classTag = if itm.Active then " active" else if itm.Disabled then " disabled" else ""
    li [ Class "nav-item" ] [ 
        // a [ Class ( "nav-link" + classTag ); Href itm.Link; OnClick (fun e -> (NavbarClicked itm) |> trigger ) ] [ str itm.Text ]
        a [ Class ( "nav-link " + classTag ); Href "#"; OnClick (fun e -> (NavbarButtonClicked itm.Id) |> dispatch ) ] [ str <| sprintf "*%s*" itm.Text ]
    ]        

let showNavbarInserts (dispatch: Msg -> unit) (inserts:NavbarItem list) =
    form [ Class "form-inline" ] (
        inserts 
            |> List.map (fun itm -> button [    Class "btn btn-sm btn-outline-secondary ml-1"
                                                OnClick (fun e ->  (NavbarButtonClicked itm.Id) |> dispatch )
                                    ] [ str itm.Text ] )
    )

let showWithInserts (model:Model) dispatch =
    let orderedItems = 
        let selected = model.LastSelection |> Option.defaultValue NavbarItem.empty 
        let selectedIndex = model.Items |> List.tryFindIndex (fun x -> x.Text = selected.Text) |> Option.defaultValue -1
        List.concat [
            model.Items |> List.take (selectedIndex + 1) |> List.map (showNavbarItem dispatch)
            //model.Inserts |> List.map NavbarItem.fromString |> List.map (showNavbarInsert dispatch)
            [ showNavbarInserts dispatch ( model.Inserts |> List.map NavbarItem.fromString ) ]
            model.Items |> List.skip (selectedIndex + 1) |> List.map (showNavbarItem dispatch)
        ]   
    nav [   
        Class "navbar navbar-default navbar-fixed-top navbar-secondary" 
        Role "navigation" 
    ] [
        div [ Class "container" ] [
            div [ Class "navbar-header" ] [
                div [ Class "nav nav-pills navbar-light bg-light navbar-expand-sm" ] (
                    // model.Items 
                    orderedItems
                )
            ]
        ]
    ] 


let view (model:Model) dispatch =
        showWithInserts model dispatch






