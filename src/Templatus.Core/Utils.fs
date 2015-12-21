module Templatus.Core.Utils

open System
open System.IO
open System.Reflection
open System.Security.Cryptography
open System.Text
open Chessie.ErrorHandling

let checkTemplatesExist templateNames =
    let inexistent = templateNames
                        |> List.filter (not << File.Exists)

    match inexistent with
    | [] -> pass templateNames
    | _ -> inexistent
            |> String.concat ", "
            |> sprintf "Cannot find templates %s."
            |> fail

let countSuccessfulOperations f input =
    let last = Array.length input

    let rec countInner current =
        if last = current
        then current
        else
            let success =
                try
                    f input.[current]
                    true
                with _ -> false
                        
            if success then countInner (current + 1) else current

    countInner 0

let toAllChannels s = 
    s |> sprintf "Debug: %s" |> System.Diagnostics.Debug.WriteLine
    s |> sprintf "Trace: %s" |> System.Diagnostics.Trace.WriteLine
    let color = System.Console.ForegroundColor
    System.Console.ForegroundColor <- System.ConsoleColor.DarkRed
    s |> sprintf "Console: %s" |> System.Console.WriteLine
    System.Console.ForegroundColor <- color

let loadedAssemblies = System.Collections.Generic.HashSet<string> ()

let runAssemblyDiagnostic prefix = 
    let prefix = (if not <| String.IsNullOrEmpty prefix && prefix.[0] <> ' ' then " " + prefix else prefix)
    let getHashS (s:string) : string =
        let sb = StringBuilder()
        let getHash(s:string) =
            let algo = MD5.Create()
            s
            |> Encoding.UTF8.GetBytes
            |> algo.ComputeHash
        getHash s
        |> Seq.iter( fun b -> sb.Append(b.ToString("X2")) |> ignore)
        sb.ToString()
    let tryGetLocation (a:System.Reflection.Assembly) =
        try
            a.CodeBase
        with |lEx ->
            toAllChannels (sprintf "Failed to get CodeBase for %s" a.FullName)
            try
                a.Location
            with |cbEx ->
                toAllChannels (sprintf "Failed to get Location for %s" a.FullName)
                "Unknown"

    let assemblyNames = 
        AppDomain.CurrentDomain.GetAssemblies() 
        |> Seq.map (fun a -> a.FullName,tryGetLocation a) 
        |> Seq.sortBy fst 
        |> List.ofSeq
    assemblyNames
    |> Seq.filter (fst >> loadedAssemblies.Contains >> not)
    |> Seq.iter (fun (n,l) -> printfn "Assembly loaded%s: %s from %s" prefix n l)

    assemblyNames 
    |> Seq.iter (fst >> loadedAssemblies.Add >> ignore)

    assemblyNames
    |> Seq.map fst
    |> fun n -> String.Join(",",Array.ofSeq n)
    |> getHashS
    |> toAllChannels

let broadcastResolves () = 
    ResolveEventHandler (fun sender evArgs ->
        sprintf "resolving %s" evArgs.Name |> toAllChannels 
        Unchecked.defaultof<Assembly>
    )
    |> AppDomain.CurrentDomain.add_AssemblyResolve

let broadcastLoads () = 
    AssemblyLoadEventHandler( fun sender alArgs ->
        printfn "AssemblyLoadEvent %s by %A" alArgs.LoadedAssembly.FullName sender
    )
    |> AppDomain.CurrentDomain.AssemblyLoad.AddHandler

let private noRedirect sender (evArgs:ResolveEventArgs) =
    printfn "Not redirecting %A, requestor: %s" evArgs.Name (if evArgs.RequestingAssembly = null then "(unknown)" else evArgs.RequestingAssembly.FullName)
    Unchecked.defaultof<Assembly>

let private tryRedirect shortName targetVersion publicKeyToken beforeLoadF (sender:obj) (evArgs:ResolveEventArgs) =
    let requestedAssembly = AssemblyName(evArgs.Name)
    if requestedAssembly.Name <> shortName then
        noRedirect sender evArgs
    else
        printfn "Redirecting assembly load of %s ,\tloaded by %s" evArgs.Name (if evArgs.RequestingAssembly = null then "(unknown)" else evArgs.RequestingAssembly.FullName)
        requestedAssembly.Version <- targetVersion
        requestedAssembly.SetPublicKeyToken(AssemblyName(sprintf "x, PublicKeyToken=%s" publicKeyToken).GetPublicKeyToken())
        requestedAssembly.CultureInfo <- System.Globalization.CultureInfo.InvariantCulture
        match beforeLoadF with
        | Some f -> 
            f ()
        | None -> ()
        Assembly.Load(requestedAssembly)

let redirectAssembly shortName (targetVersion:Version) publicKeyToken =
    let onResolveEvent = new ResolveEventHandler( tryRedirect shortName targetVersion publicKeyToken None)
    AppDomain.CurrentDomain.add_AssemblyResolve onResolveEvent

let redirectAssemblyOnce shortName targetVersion publicKeyToken = 
    let partial = tryRedirect shortName targetVersion publicKeyToken 
    // rec is to allow unhooking of created event handler
    let rec onResolveEvent = new ResolveEventHandler( partial (Some (fun () -> AppDomain.CurrentDomain.remove_AssemblyResolve onResolveEvent)))
    AppDomain.CurrentDomain.add_AssemblyResolve onResolveEvent