module Components.TouchKeyboard
//module CompKeyboard exposing (..)

open System

open Elmish
open Elmish.React
open Fable
open Fable.React
open Fable.React.Props
open Fable.Import


type Msg =
    | TouchKeyboadPressed of string

type InputType = 
    | InputText
    | InputPassword

type Model = {
    Value: string
    InputType: InputType
}

let update msg model =
    match msg with
        | TouchKeyboadPressed str ->
            match model.InputType with
                | InputText | InputPassword ->
                    { model with Value= model.Value + str }, Cmd.none


let view (model:Model) dispatch =
    let keyboardButton txt = 
        div [ Class "col close-spacing" ] [ 
            button [    Class "btn keylike"
                        Style [ Width "50px"; Height "50px" ]            
                        OnClick (TouchKeyboadPressed txt |> dispatch)
                    ] [ str txt]
        ]           
    let asciiRangeToString intRange =  
            intRange 
            |> List.map char
            |> List.map string

    let numbers =  [ 48 .. 57 ] |> asciiRangeToString
    let letters1 = [ 65 .. 73 ] |> asciiRangeToString
    let letters2 = [ 74 .. 82 ] |> asciiRangeToString
    let letters3 = [ 83 .. 90 ] |> asciiRangeToString
    div [ Class "container"] [
        div [] [
            input [ Type (if model.InputType = InputPassword then "password" else "text" ) ]
        ]
        div [ Class "center-block"] [
            div [ Class "row" ] (List.map keyboardButton numbers)
            div [ Class "row" ] (List.map keyboardButton letters1)
            div [ Class "row" ] (List.map keyboardButton letters2)
            div [ Class "row" ] (List.map keyboardButton letters3)
        ]
    ]


