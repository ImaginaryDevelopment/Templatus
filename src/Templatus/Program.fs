﻿namespace Templatus

open Templatus.Core
open Chessie.ErrorHandling

module Main =
    let checkArgumentCount args =
        match Array.length args with
        | 0 -> fail "No file provided."
        | _ -> pass args

    let getParsedTemplate (args: string []) =
        match TemplateParser.parse args.[0] with
        | Ok (result, _) -> pass result
        | Bad reasons -> "Template parsing failed: " :: reasons |> Bad

    let processParsedTemplate parsed =
        match Processor.processTemplate parsed with
        | Ok (result, _) -> pass result
        | Bad reasons -> "Template processing failed: " :: reasons |> Bad

    let generateOutput processed =
        match OutputGenerator.generate processed with
        | Ok _ -> pass ()
        | Bad reasons -> "Output generation failed: " :: reasons |> Bad

    [<EntryPoint>]
    let main argv = 
        let doStuff = argv |> checkArgumentCount >>= getParsedTemplate >>= processParsedTemplate >>= generateOutput

        match doStuff with
        | Ok _ -> printfn "Aw yisss!"; 0
        | Bad reasons -> reasons |> List.iter (fun r -> r.Trim() |> eprintfn "%s"); 1