namespace Templatus

open System
open Templatus.Core
open Chessie.ErrorHandling
open Templatus

type Arg = 
        |TemplateName of string
        |Parameters of string
        |Parallellism of int

module Main =
    let getTemplateNames (parsedArgs: Arg list) =
        match parsedArgs with
        | [] 
        | _ when not <| List.exists(fun l -> match l with |TemplateName _ -> true | _ -> false) parsedArgs 
            -> "No templates provided.\nUsage:" |> fail
        | list -> list |> List.choose (fun l -> match l with |TemplateName s -> Some s | _ -> None) |> pass

    let getTemplateParameters (parsedArgs: Arg) =
        match parsedArgs with
        | Parameters parameters -> 
            parameters.Split ';'
                |> List.ofArray
                |> List.map (fun p -> p.Split '=')
                |> List.choose (fun ps -> if ps.Length <> 2 then None else Some (ps.[0], ps.[1]))
        | _ -> []

    let getDegreeOfParallelism (parsedArgs: Arg) =
        match parsedArgs with
        | Parallellism i -> i
        | _ -> 1
        //match parsedArgs.TryGetResult <@ Parallelization @> with
        //| Some n -> if n < 1 then 1 else n
        //| None -> 

    [<EntryPoint>]
    let main s =
        do
            let microsoftKey = "b03f5f7f11d50a3a"
            Utils.broadcastResolves ()
            Utils.broadcastLoads ()

            Utils.redirectAssembly "Microsoft.Build.Framework" (Version("14.0.0.0")) microsoftKey
            Utils.redirectAssembly "FSharp.Core" (Version("4.4.0.0")) microsoftKey

        let rec results (args:string list) : Arg list = 
            match args with 
            | [] -> List.empty
            | "-t"::x::remainder  -> 
                [ 
                    yield Arg.TemplateName x
                    yield! results remainder
                ]
            | "-p"::x::remainder -> 
                [
                    yield Arg.Parameters x
                    yield! results remainder
                ]
            | "-parallelization"::i::remainder ->
                [
                    yield Arg.Parallellism (i |> int)
                    yield! results remainder
                ]
            | x -> failwithf "arg options unrecognized %A" args
        let results = results ( s |> List.ofArray)
        let parameters = results |> Seq.choose (function | Parameters s as p -> getTemplateParameters p |> Some | _ -> None) |> Seq.head
        let parallelism = results |> Seq.choose( function | Parallellism i as x ->  getDegreeOfParallelism x |> Some | _ -> None) |> Seq.tryHead |> function | Some i -> i | None -> 1
        let runDiagnostic prefix x = Utils.runAssemblyDiagnostic prefix; pass x
        let tryOrRunDiagnostic prefix f x =
            try
                f x
            with ex ->
                Utils.runAssemblyDiagnostic prefix 
                reraise ()

        let processChunk list = list |> List.map TemplateParser.parse
                                |> List.map (bind (Processor.processTemplate TemplateParser.parse))
                                |> List.map (bind (runDiagnostic " Templates are processed"))
                                |> List.map (bind (tryOrRunDiagnostic "(Generation failed)" (OutputGenerator.generate parameters)))
                                |> Async.singleton
                   
        printfn "Degree of parallelism: %d" parallelism
        printfn "Starting..."

        let createOutput = results |> getTemplateNames
                           >>= Utils.checkTemplatesExist
                           |> lift (List.splitInto parallelism)
                           |> lift (List.map processChunk)
                           |> lift (Async.Parallel >> Async.RunSynchronously)
                           |> lift (List.ofArray >> List.concat)
                           >>= collect
        let resultCode = 
            match createOutput with
            | Ok _ -> printfn "All templates processed successfully."; 0
            | Bad reasons -> reasons |> List.iter (eprintfn "%s"); 1
        if System.Diagnostics.Debugger.IsAttached then
            Console.ReadLine() |> ignore
        resultCode