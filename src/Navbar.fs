module Components.Navbar

open System
open Elmish
open Fable.Core
// open Elmish.React
open Fable.React
open Fable.React.Props

// open Fable.Helpers.React
// open Fable.Helpers.React.Props
open Microsoft.FSharp.Reflection


// type NavbarAction = 
//     | Link of string
    // | ClickAction of (Msg -> unit)

type NavbarItem = {
    Id: string
    Text: string
    Link: string
    Active: bool
    Disabled: bool
}
    with 
        [<Obsolete>]   //change to fromString
        static member makeLink str = { Id= str; Text= str; Link= str; Active= false; Disabled= false }
        static member fromString str = { Id= str; Text= str; Link= str; Active= false; Disabled= false }
        static member empty = { Id= ""; Text= ""; Link= ""; Active= false; Disabled= false }
        // static member makeAction str (act:Msg -> unit) = { Text= str; Link= ClickAction act; Active= false; Disabled= false }

type Msg = 
    | NavbarClicked of NavbarItem
    // | SubItemClicked of string
    // | ActionClicked of ClickAction


type Model = {
    Branding: string
    Items: NavbarItem list
    LastSelection: NavbarItem option
}
    with
        static member empty= { Branding= ""; Items= []; LastSelection= None }
        static member fromItems branding items = { Branding= branding ; Items= items ; LastSelection= None }

type StNavbar = Model

// type NavbarItem = {
//     Text: string
//     Action: NavbarAction
//     Active: bool
//     Disabled: bool    
// }
//     with 
//         static member makeLink str = { Text= str; Action= Link str; Active= false; Disabled= false }
//         static member makeAction str (act:Msg -> unit) = { Text= str; Action= ClickAction act; Active= false; Disabled= false }


type NavbarShort = 
    | Name of string
    | NameAndId of string*string   
    | WithLink of string*string

module navmsg =
    let inline fromString<'a> (s:string) =
        match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s.Replace(" ","")) with
            | [|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
            | _ -> None

let navbarItemsFromShort (shorts:NavbarShort list) =
    let fromShort sh = 
        match sh with
            | Name x -> { Id= x; Text= x; Link=  ""; Active= false;  Disabled= false }
            | NameAndId (name,id) -> 
                { Id= id; Text= name; Link=  ""; Active= false;  Disabled= false }
            | WithLink (n,l) -> { Id= n; Text= n; Link= l; Active = false; Disabled= false }
    shorts 
        |> List.mapi (fun i x -> i,fromShort x ) 
        |> List.map (fun (i,x) -> if i = 0 then { x with Active= true } else x )

let navbarItems (names:string list) (selected:string) =
    let maybeSelected = names |> List.tryFind (fun x -> x = selected)
    let showSelected = maybeSelected |> Option.defaultValue (List.head names) 
    names |> List.map (fun x ->  { Id= x; Text= x; Link = ""; Active= x = showSelected;  Disabled= false } )

let setActiveNavbarItem (id:string) (navs:NavbarItem list) = 
    Console.WriteLine("Setting active: " + id)
    navs    
        |> List.map (fun x -> { x with Active = x.Id = id } )

let showNavbarItem (trigger: Msg -> unit) (itm:NavbarItem) =
    let classTag = if itm.Active then " active" else if itm.Disabled then " disabled" else ""
    li [ Class "nav-item" ] [ 
        // a [ Class ( "nav-link" + classTag ); Href itm.Link; OnClick (fun e -> (NavbarClicked itm) |> trigger ) ] [ str itm.Text ]
        a [ Class ( "nav-link " + classTag ); Href "#"; OnClick (fun e -> (NavbarClicked itm) |> trigger ) ] [ str itm.Text ]
    ]        

// let showNavbarItemWithAction (itm:NavbarItemWithAction) =
//     let classTag = if itm.Active then " active" else if itm.Disabled then " disabled" else ""
//     let action = match itm.Action with
//                     | Link x -> Href x
//                     | ClickAction x -> (OnClick (fun e -> x))
//     li [ Class "nav-item" ] [ 
//         a [ Class ( "nav-link" + classTag ); action ] [ str itm.Text ]
//     ]        

// let showNavbar (branding:string) (itms:NavbarItem list) (trigger: Msg -> unit) =
let showNavbar (branding:string) (itms:NavbarItem list) dispatch =
    let height = "36px"
    div [ Class "d-block"; Style [ ZIndex 1 ] ] [
        div [ Class "d-block fixed-top"; Style [ Height height ] ] [
            div [ Class "nav nav-pills navbar-dark bg-dark" ] (
                itms 
                    |> List.map (showNavbarItem dispatch)
                    |> List.append [
                        a [ Class "navbar-brand"; Style [ Color "white" ] ] [ str branding ]
                    ]
            )
        ]
        div [ Style [ Height height ] ] []
    ]


let showSubmenu (itms:NavbarItem list) dispatch =
    nav [   
        Class "navbar navbar-default navbar-fixed-top navbar-secondary" 
        Role "navigation" 
    ] [
        div [ Class "container" ] [
            div [ Class "navbar-header" ] [
                div [ Class "nav nav-pills navbar-light bg-light navbar-expand-sm" ] (
                    itms 
                        |> List.map (showNavbarItem dispatch)
                )
            ]
        ]
    ] 
        
    // let height = "36px"
    // div [ Class "d-block"; Style [ ZIndex 2 ] ] [
    //     // div [ Class "fixed-top"; Style [  ZIndex 2; PaddingTop height; ] ] [
    //     div [ Class ""; Style [  ZIndex 2 ] ] [
    //         div [ Class "nav nav-pills navbar-light bg-light navbar-expand-sm" ] (
    //             itms 
    //                 |> List.map (showNavbarItem dispatch)
    //         )
    //         // div [ Class "d-block"; Style [ PaddingTop height ] ] [] 
    //     ]
    //     // div [ Class "d-block"; Style [PaddingBottom height ] ] [] 
    //     // div [ Class "d-block"; Style [PaddingBottom height ] ] [] 
    // ]


let update (msg:Msg) (model:Model) =
    match msg with
        | NavbarClicked itm ->
            { model with    LastSelection= Some itm 
                            Items= setActiveNavbarItem itm.Id model.Items
            }        


module View = 
    let mainNavBar (model:Model) dispatch =
        showNavbar model.Branding model.Items dispatch
    let secondaryNavBar (model:Model) dispatch = 
        showSubmenu model.Items dispatch

type Model with
    static member fromStrings branding items = { Branding= branding; Items= navbarItems items "" ; LastSelection= None } 
    static member tryFindItem (navbar:Model) (str:string) = navbar.Items |> List.tryFind (fun x -> x.Text.ToLower() = str.ToLower())
    static member trySetItem str navbar = 
        match navbar.Items |> List.tryFind (fun x -> x.Id= str) with
            | Some itm -> update (NavbarClicked itm) navbar
            | _ -> navbar                                                    
