module Timer

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.Import


type Model= {
    Interval: int //millisecondsseconds
    Enabled: bool
}
    with static member default_ = { Interval= 1000 ; Enabled= false }

module __this=
    let mutable countFlag= false
    let mutable hndl= 0.

    let start i fn= 
        printfn "Timer - Start" 
        hndl <- Browser.Dom.window.setInterval(fn,i,[| |])
        
    let stop () = 
        try
            printfn "Timer - Stop" 
            if hndl <> 0. then
                Browser.Dom.window.clearInterval(hndl)
        with e ->
            Browser.Dom.console.error e
    
    let restart i fn=
        stop ()
        start i fn

type Msg=
    | Start of (int*bool)
    | SetOn
    | SetOff of bool //restart
    | Tick of DateTime
    
let update msg model = 
    match msg with
        | Start (i,b) ->
            { model with Interval= i ; Enabled= b },
                if b then SetOn |> Cmd.ofMsg else Cmd.none
        | SetOn ->        
            { model with Enabled= true }, Cmd.none
        | SetOff r ->
            __this.stop ()
            { model with Enabled= false }, Cmd.none
        | Tick dt ->
            model, Cmd.none

let mutable lastEnabled= false

let view model dispatch= 
    printfn "Timer Enabled: Now %b ; Last %b" model.Enabled lastEnabled
    if model.Enabled && not lastEnabled then
        lastEnabled <- true
        __this.start model.Interval (
                fun _ ->  
                    printfn "Timer: %s" (DateTime.Now.ToString("HH:mm:ss"))
                    Tick (DateTime.Now) |> dispatch
            )
    else if not model.Enabled then lastEnabled <- false
    div [ Style [ Display DisplayOptions.None ] ] [ str <| sprintf "Timer %s" (if model.Enabled then "On" else "Off") ]


