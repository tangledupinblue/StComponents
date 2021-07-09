module Components.FormControls

open System
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

let idOf (name:string) = name.Replace(" ","_")

let addStyle extend onto = 
    let moreStyles styles = if String.length styles > 0 then " " + styles else ""
    onto + moreStyles extend

// let toTup x = (x,x)

let tryDouble (str:string) =
    try
        if str.Length = 0 then
            Ok 0.
        else
            Convert.ToDouble(str) |> Result.Ok
    with
        | ex -> Result.Error ex.Message

let tryBool (str:string) =
    Console.WriteLine("tryBool: " + str) 
    str.ToLower() = "on"        

let onSuccessOf<'a> (res:Result<'a,string>) (trigger: 'a -> unit) =
    match res with
        | Ok v -> trigger v
        | _ -> ()


// <div class="alert alert-success" role="alert">
//   <h4 class="alert-heading">Well done!</h4>
//   <p>Aww yeah, you successfully read this important alert message. This example text is going to run a bit longer so that you can see how spacing within an alert works with this kind of content.</p>
//   <hr>
//   <p class="mb-0">Whenever you need to, be sure to use margin utilities to keep things nice and tidy.</p>
// </div>


type QuickButton = {
    Text: string
    Context: string
    // OnClick: Fable.React.MouseEvent -> unit
    OnClick: Browser.Types.MouseEvent -> unit //React.MouseEvent -> unit
    }

type Message = {
    Text: string
    Context: string
    Options: QuickButton list
    }

let messageBox (msg:Message) =
    let alertClass = "alert alert-" + msg.Context
    div [] [
        div [ Class alertClass; Role "alert"  ] (
            ( if List.isEmpty msg.Options |> not then ( [
                hr []
                div [ ] (
                    msg.Options 
                        |> List.map ( fun x -> button [ Class <| "btn btn-" + x.Context; OnClick x.OnClick ] [ str x.Text ] )
                )                
            ] )
            else 
                [ text [] [ str "" ] ]
            )
                |> List.append [ str msg.Text ]
        )
    ]

let alertHeading (lines:string list) (bootstrapType:string) = 
    let header = lines |> List.tryHead |> (fun x -> if Option.isSome x then x.Value else "" )
    let theRest = lines |> (fun x -> if x.Length > 1 then List.skip 1 x else [] )
    div [ Class <| "alert alert-" + bootstrapType; Role "alert" ] (
        List.append [ h4 [ Class "alert-heading" ] [ str header ] ]
                    (
                        theRest |> List.map (fun x -> p [] [ str x] )
                    )
    )
    
let styledTextInput name value (styles:string) (trigger:string -> unit) = 
    div [ Class ("form-group" |> addStyle styles) ] [
        label [] [ str name ]
        input [Id (idOf name); Type "text"; Class "form-control form-control-sm"; Value value; 
                OnChange (fun e -> trigger e.Value) ]
    ]

let styledOption name value keyvals styles (trigger:string -> unit) =
    let toOption (key,value,select) = 
        if select then option [ Value key; Selected true ] [ str value ]
        else option [ Value key ] [ str value ]
    in
        div [ Class ("form-group" |> addStyle styles) ] [
            label [] [ str name ]
            select [ Id (idOf name); Class "form-control form-control-sm"; //Value value; 
                OnChange (fun e -> trigger e.Value) ]
                ( keyvals |> List.map (fun (x,y) -> toOption (x,y,x = value)) )
        ]

type OptionValue = {
    Key: string
    Value: string
}
    with static member create key value = { Key= key; Value= value }

let buttonOptions selectedKey (keyvals:OptionValue list) styles (trigger:OptionValue -> unit) =
    let isSelected opt = opt.Key = selectedKey
    div [ Class "container-fluid" ] [
        div [ Class "form-inline" ] (
            keyvals 
                |> List.map ( fun x -> 
                                        button [ 
                                            Class <| "btn " + (if isSelected x then "btn-primary" else "btn-default") + " mb-1 mr-1 " + styles
                                            OnClick (fun _ -> trigger x)
                                        ] [ str x.Value ] )
        )
    ]

let styledNumberInput name value styles (trigger:double -> unit) = 
    div [ Class <| addStyle styles "form-group" ] [
        label [] [ str name ]
        input [ Id (idOf name)
                Type "number"
                Class "form-control form-control-sm"
                Value (string value)
                OnChange (fun e -> 
                            let strVal =    if string value = "0" && ((string e.Value).Length = 2) && ((string e.Value).EndsWith("0")) then
                                                ((e.Value |> string).Remove(1))
                                            else string e.Value 
                            onSuccessOf (tryDouble strVal) trigger) ]
        // input [ Id (idOf name); Type "number"; Class "form-control"; Value (string value) ] []
    ]

let styledCheckBox name value styles (trigger:bool -> unit) =
    div [ Class <| addStyle styles "form-group form-check" ] [
        div [ Class "d-inline"; Style [ PaddingRight "20px" ] ] [ str "" ]
        input [Id (idOf name); Type "checkbox"; Class "form-check-input form-control-sm"; Checked value; 
                OnChange (fun _ -> trigger (not value) ) ]
        // , label [for (id_ name) ] [text name]
        label [] [ str name ]
    ]

// --   <div class="form-group form-check">
// --     <input type="checkbox" class="form-check-input" id="exampleCheck1">
// --     <label class="form-check-label" for="exampleCheck1">Check me out</label>
// --   </div>




