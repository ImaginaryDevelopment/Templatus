# Templatus
[![Build Status](https://magnum.travis-ci.com/kerams/Templatus.svg?token=MzTGNBqs9peqx7A5xToB&branch=master)](https://magnum.travis-ci.com/kerams/Templatus)

## What?
Templatus is a templating tool that works (and currently also looks) a lot like T4. The major difference is the fact that blocks that emit text are written in F# instead of C#.

## Why?
T4 does not support F# :^). Using T4 templates at compile-time is also rather problematic.

## How?
Template parsing is implemented using the excellent library [FParsec](http://www.quanttec.com/fparsec/) and the execution of text-emitting code is done through the hosting of F# Interactive and [F# Compiler Services](https://github.com/fsharp/FSharp.Compiler.Service).

## Features?
- Design-time processing on save (the default for T4) is not yet supported. You are required to execute the processor in your build pipeline (MSBuild, [FAKE](https://github.com/fsharp/FAKE), etc.).
- Text-emitting code can access parameters passed into the processor by their name. If these parameters are not later passed into the processor, an error is raised.
- `popIndent`, `clearIndent`, `pushIndent` providing the same functionality as in T4.
- Referencing assemblies
- Template nesting

## Getting started
### Template parts
- Directives - delimited by `<#@` and `#>`
  - Output directive `<#@ output filename="..\gen\output.txt" #>` - Specifies the file to generate
  - Include directive `<#@ include file="sub\include.ttus" #>` - Specifies a nested template whose output will be embedded in place of the directive
  - Assembly reference directive `<#@ assembly name="TestLib.dll" #>` - Specifies an assembly (either in GAC or local to the template) to reference
- Controls
  - Expression `<#= DateTime.Now #>` - Expressions that are evaluated, converted to string (using the `%O` format specifier) and printed out
  - Block `<# [1..2] |> List.iter tprintn #>` - Contains arbitrary logic (including function/module declarations that will be available in any control from that point onwards) and can use any of the `tprint` functions to print into the output file
- Literals
  - Any text that is neither a directive nor a control
  - Printed into the output "as is"

*Note*: Bodies of any control are whitespace-sensitive just like regular F# code, meaning you have to abide by F#'s whitespace rules. Also, tabs are automatically converted to 4 spaces.

### Convenience functions
- `tprint` - Takes any object and after calling ToString on it prints the result into the output
- `tprintn` - The same as above but with a trailing newline
- `tprintf` - The `sprintf` equivalent for printing into the output
- `tprintfn` - The same as above but with a trailing newline
- `pushIndent` - Takes a string and puts it on the stack of indent strings. The indent strings are used as a prefix for everything that `tprintn` and `tprintfn` output.
- `popIndent` - Removes a string from the top of the stack of indent strings. Does not throw an exception when the stack is empty.
- `clearIndent` - Clears the entire stack of indent strings

### Template processor
Templatus.exe is a command-line template processor that takes a template and an optional list of arguments to make accessible in the template. The following flags are available:
- `-t ..\..\myTemplate.ttus` - Specifies the template to be processed
- `-p name=Timmy;age=3` - Defines `name` and `age` variables that you can directly refer to in the template. Note that the variables are always defined as strings.