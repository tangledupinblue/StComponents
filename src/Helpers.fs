module Helpers

open System
open System.Collections.Generic

open Fable.React
open Fable.React.Props
open Fable.Core


type HelpTitle= string
type Text= string
type HtmlString= string
type UrlLink= string


type HelpContentType= 
    | HelpText of (HelpTitle*Text)
    | HelpHtml of (HelpTitle*HtmlString)
    | HelpFromUrl of (HelpTitle*UrlLink)
    | HelpNewTab of (HelpTitle*UrlLink)

type HelpView=
    | Inline of HelpContentType
    | IconPopup of HelpContentType
    | ButtonPopup of HelpContentType
    | Link of string

let HTML_NOT_FOUND = "<p>Not Found</p>"
//let LOADING= "<p>Loading...</p>"

module VarStore= 
    open Fable.Core
    let mutable varStore = new Dictionary<string,string>()

    // [<Emit("if ($0) { $0 = $1; } else { var $0 = $1; }")>]    
    // let setStringVar (varName:string) (value:string) : unit = jsNative

    // [<Emit("$0 ? $0 : \"\" ")>]
    // let getStringVar (varName:string) : string = jsNative 

    let setStringVar varName value =
        printfn "setting %s to %s" varName value
        if varStore.ContainsKey varName then varStore.[varName] <- value
        else varStore.Add(varName, value)

    let getStringVar varName=
        printfn "getting %s from %A" varName varStore
        if varStore.ContainsKey varName then varStore.[varName] else HTML_NOT_FOUND


module UrlUtils=
    open Fetch

    [<Emit("window.open($0,'_blank').focus();")>]
    let openNewTab (str:string) : unit = jsNative

    let readUrl callback id url () = promise {        
        printfn "fetching from %s for %A" url callback
        let! resp= fetch url []      
        let! txt= resp.text()        
        VarStore.setStringVar id txt
        callback ()
    }

module this=
    let idOf (name:string) = "vv" + name.Replace(" ","_")

    let idBody = (sprintf "body_%s")

    let helpTitle helpContent =
        helpContent |> function | HelpText (t,_) | HelpHtml (t,_) | HelpFromUrl (t,_) | HelpNewTab (t,_) -> t

    let helpButton helpTitle onClick id helpElement= 
        div [] [
            button [    Type "button"
                        Class "btn btn-primary"
                        DataToggle "modal"
                        HTMLAttr.Custom ("data-target",sprintf "#%s" id) 
                        OnClick onClick
            ] [ str helpTitle ]            
            // Parts._button onClick helpTitle id
            helpElement
        ]

    let helpIcon onClick id helpElement =
        let helpIconRaw= """
<svg width="1em" height="1em" viewBox="0 0 16 16" class="bi bi-question-circle-fill" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
  <path fill-rule="evenodd" d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zM5.496 6.033a.237.237 0 0 1-.24-.247C5.35 4.091 6.737 3.5 8.005 3.5c1.396 0 2.672.73 2.672 2.24 0 1.08-.635 1.594-1.244 2.057-.737.559-1.01.768-1.01 1.486v.105a.25.25 0 0 1-.25.25h-.81a.25.25 0 0 1-.25-.246l-.004-.217c-.038-.927.495-1.498 1.168-1.987.59-.444.965-.736.965-1.371 0-.825-.628-1.168-1.314-1.168-.803 0-1.253.478-1.342 1.134-.018.137-.128.25-.266.25h-.825zm2.325 6.443c-.584 0-1.009-.394-1.009-.927 0-.552.425-.94 1.01-.94.609 0 1.028.388 1.028.94 0 .533-.42.927-1.029.927z"/>
</svg>
        """
        div [] [
            div [   Style [ Cursor "pointer" ; Width "10px" ] 
                    DataToggle "modal"
                    HTMLAttr.Custom ("data-target",sprintf "#%s" id) 
                    DangerouslySetInnerHTML { __html= helpIconRaw } 
                    OnClick onClick                    
            ] []
            //modalDialog id helpTitle helpContent ocallback
            helpElement
        ]

    let helpContentNode id helpContent ocallback ()=
        printfn "helpContentNode for %s: %A" id helpContent
        match helpContent with
            | HelpText (_,s) ->
                div [] [ str s ]
            | HelpHtml (_,s) ->
                div [ DangerouslySetInnerHTML { __html = s } ] []
            | HelpFromUrl (_,s) ->
                let htmltxt = 
                    let v = VarStore.getStringVar id
                    if v = HTML_NOT_FOUND then 
                        //UrlReader.readUrl s |> (fun p -> p.then(fun t -> VarStore.setStringVar t) )
                        match ocallback with
                            | Some callback -> 
                                UrlUtils.readUrl callback id s () |> ignore
                            | None -> ()
                        v
                    else v
                div [ DangerouslySetInnerHTML { __html = htmltxt } ] []
            | HelpNewTab (_,_) ->
                div [] []

    //the callback is used after loading from html
    let modalDialog id helpTitle helpContent (ocallback:(unit -> unit) option)=
        printfn "calc dialog for %s" id
        // let content ()= 
        div [ Id id ; Class "modal" ; TabIndex -1 ; Role "dialog" ] [
            div [ Class "modal-dialog" ; Role "document" ] [
                div [ Class "modal-content" ] [
                    div [ Class "modal-header" ] [
                        h5 [ Class "modal-title" ] [ str helpTitle ]
                        button [    Type "button"
                                    Class "close"
                                    HTMLAttr.Custom ("data-dismiss","modal")
                                    HTMLAttr.Custom ("aria-label","Close") 
                        ] [     span [  HTMLAttr.Custom ("aria-hidden","true") 
                                        DangerouslySetInnerHTML { __html = "&times;" }
                                ] []                                                        
                        ] 
                    ]
                    div [ Class "modal-body" ; Id (idBody id) ] [
                        helpContentNode id helpContent ocallback ()
                    ]
                ]
            ]
        ]

    //let hostInside containerfn id=
    let updateHelpContentNodeCallback id helpContent ()=
        printfn "help callback hit for %s" id
        helpContentNode id helpContent None ()
            |> Fable.React.Helpers.mountById (idBody id) 

    let emptyClick (me:Browser.Types.MouseEvent) = ()

    let helpComponent helpContent hostInsideOf =
        let helpTitle= helpTitle helpContent
        let id= idOf helpTitle
        match helpContent with
            | HelpContentType.HelpFromUrl _ ->
                let callback=  updateHelpContentNodeCallback id helpContent
                modalDialog id helpTitle helpContent (Some callback) 
                    |> hostInsideOf emptyClick id
            | HelpContentType.HelpNewTab (_,u) ->                
                div [] [] |> hostInsideOf (fun _ -> UrlUtils.openNewTab u) id 
            | _ ->
                modalDialog id helpTitle helpContent None 
                    |> hostInsideOf emptyClick id


module View= 

    let helpButton helpContent=
        let helpTitle= this.helpTitle helpContent
        this.helpComponent helpContent (this.helpButton helpTitle )
    
    let helpIcon helpContent =
        this.helpComponent helpContent (this.helpIcon ) 

