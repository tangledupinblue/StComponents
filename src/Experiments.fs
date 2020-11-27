module Experiments


open System
open Fable.Core
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch
open Thoth.Json


//NB: This is a work in progress - may or may not be continued

type Size = {
    Width: int
    Height: int
}
    with 
        static member toStyle size = [ Width size.Width ; Height size.Height ]
        static member square i = { Width= i ; Height= i }

type DashStatus = 
    | Info
    | Success
    | Warning
    | Danger
    with 
        static member asString status = status |> string |> (fun s -> s.ToLower())
        static member statusString = 
                        function | Info -> "info" | Success -> "success" | Warning -> "warning" | Danger -> "danger"

type DashboardComponent =
    | Status of DashStatus
    | Measure
    | Scale

type Msg =
    | ComponentClicked of DashboardComponent


type Model = {
    Components: DashboardComponent list
}
    with static member empty = { Components= [] }


// let drawComponent comp dispatch = 
//     match DashboardComponent with
//         | Status -> 
//             div [] [ 

//             ]
//         | _ -> div [] [ str "to be implemented" ]


module Status = 
    let trafficLightAndText text status size clickAction =
        div [   Class "text-center"
                OnClick clickAction                
        ] [ 
            div [   Class <| sprintf "bg-%s rounded-circle mt-2" (status |> DashStatus.asString) 
                    Style (Size.toStyle size)
            ] [ ]
            div [] [
                p [] [ str text ] 
            ]
        ]

let update (msg:Msg) (model:Model) =
    model


let demoControls = [
    Status.trafficLightAndText "System Up" Warning (Size.square 30) (fun _ -> ())
]

let view model dispatch = 
    let inGrid re = div [ Class "col-3 border border-warning rounded" ] [ re ]
    div [ Class "container" ] [
        Helpers.View.helpButton (Helpers.HelpText ("Help Button","Help Text do this and that and help with all this stuff") )
        div [ Style [ Height "30px" ] ] []
        Helpers.View.helpIcon (Helpers.HelpText ("Help Icon","Help Text do this and that and help with all this stuff launched from Icon") )
        div [ Style [ Height "30px" ] ] []
        Helpers.View.helpIcon (Helpers.HelpHtml ("Help Icon Text","<h1>Help Text</h1><p>do this and that and help with</p><small>all this stuff launched from Icon</small>") )
        div [ Style [ Height "30px" ] ] []
        Helpers.View.helpIcon (Helpers.HelpFromUrl ("Help Icon Text From Url","HelpLink.html") )
        div [ Style [ Height "30px" ] ] []
        Helpers.View.helpIcon (Helpers.HelpNewTab ("Launch new tab","HelpLink.html") )
        div [ Class "row" ] (
            demoControls 
                |> List.map inGrid
        )
    ]







