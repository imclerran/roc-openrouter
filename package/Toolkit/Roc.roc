module { cmdNew, cmdArg, cmdOutput } -> [roc, rocCheck, rocTest, rocStart]

import json.Json
import InternalTools exposing [Tool, buildTool]

Output : {
    stdout : List U8,
    stderr : List U8,
}

CommandErr : [
    ExitCode I32,
    KilledBySignal,
    IOError Str,
]

roc = {
    name: rocTool.function.name,
    handler: rocHandler,
    tool: rocTool,
}

rocTool : Tool
rocTool = 
    rocFileParam = {
        name: "rocFile",
        type: "string",
        description: "The path to the .roc file to be executed. IE: `./path/to/file.roc`",
        required: Bool.true,
    }
    buildTool "roc" "Build a roc application from a .roc file, and run it if there are no errors." [rocFileParam]

rocHandler : Str -> Task Str _
rocHandler = \args ->
    decoded : Decode.DecodeResult { rocFile : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { rocFile } ->
            if rocFile |> Str.contains ".." then
                Task.ok "Invalid path: `..` is not allowed"
            else if rocFile |> Str.startsWith "/" then
                Task.ok "Invalid path: must be a relative path"
            else
                cmdNew "roc" 
                    |> cmdArg rocFile
                    |> cmdOutput
                    |> Task.result!
                    |> cmdOutputResultToStr
                    |> Task.ok

rocCheck = {
    name: rocCheckTool.function.name,
    handler: rocCheckHandler,
    tool: rocCheckTool,
}

rocCheckTool : Tool
rocCheckTool = 
    rocFileParam = {
        name: "rocFile",
        type: "string",
        description: "The path to the .roc file to be executed. IE: `./path/to/file.roc`",
        required: Bool.true,
    }
    buildTool "rocCheck" "Check a Roc file for syntax errors" [rocFileParam]

rocCheckHandler : Str -> Task Str _
rocCheckHandler = \args ->
    decoded : Decode.DecodeResult { rocFile : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { rocFile } ->
            if rocFile |> Str.contains ".." then
                Task.ok "Invalid path: `..` is not allowed"
            else if rocFile |> Str.startsWith "/" then
                Task.ok "Invalid path: must be a relative path"
            else
                cmdNew "roc" 
                    |> cmdArg "check"
                    |> cmdArg rocFile
                    |> cmdOutput
                    |> Task.result!
                    |> cmdOutputResultToStr
                    |> Task.ok

rocTest = {
    name: rocTestTool.function.name,
    handler: rocTestHandler,
    tool: rocTestTool,
}

rocTestTool : Tool
rocTestTool = 
    rocFileParam = {
        name: "rocFile",
        type: "string",
        description: "The path to the .roc file to be tested. IE: `./path/to/file.roc`",
        required: Bool.true,
    }
    buildTool "rocTest" "Test the expect statements in a specified roc file." [rocFileParam]

rocTestHandler : Str -> Task Str _
rocTestHandler = \args ->
    decoded : Decode.DecodeResult { rocFile : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { rocFile } ->
            if rocFile |> Str.contains ".." then
                Task.ok "Invalid path: `..` is not allowed"
            else if rocFile |> Str.startsWith "/" then
                Task.ok "Invalid path: must be a relative path"
            else
                cmdNew "roc" 
                    |> cmdArg "test"
                    |> cmdArg rocFile
                    |> cmdOutput
                    |> Task.result!
                    |> cmdOutputResultToStr
                    |> Task.ok

rocStart = {
    name: rocStartTool.function.name,
    handler: rocStartHandler,
    tool: rocStartTool,
}

rocStartTool : Tool
rocStartTool = 
    appNameParam = {
        name: "appName",
        type: "string",
        description: "The name of the .roc application to be initialized. IE: `myApp` will generate `myApp.roc`",
        required: Bool.true,
    }
    platformParam = {
        name: "platform",
        type: "string",
        description: "The platform to use in the new roc application. May be one of: `basic-cli`, or `basic-webserer`",
        required: Bool.true,
    }
    buildTool 
        "rocStart" 
        """
        Start a new Roc application with the specified name and platform. 
        You should always use this tool when creating a new roc program,
        and make sure to read read the generated output file before changing
        it, so that the correct package and platform urls can be maintained.
        """
        [appNameParam, platformParam]

rocStartHandler : Str -> Task Str _
rocStartHandler = \args ->
    decoded : Decode.DecodeResult { appName : Str, platform : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { appName, platform } ->
            if appName |> Str.contains ".." then
                Task.ok "Invalid appName: `..` is not allowed"
            else if appName |> Str.contains "/" then
                Task.ok "Invalid appName: name may not be a path"
            else if appName |> Str.contains ".roc" then
                Task.ok "Invalid appName: name may not contain file extension."
            else if platform != "basic-cli" && platform != "basic-webserver" then
                Task.ok "Invalid platform: must be one of 'basic-cli' or 'basic-webserver'"
            else
                cmdNew "roc-start" 
                    |> cmdArg "app"
                    |> cmdArg appName
                    |> cmdArg platform
                    |> cmdOutput
                    |> Task.result!
                    |> cmdOutputResultToStr
                    |> Task.ok

cmdOutputResultToStr : Result Output [CmdOutputError (Output, CommandErr)] -> Str
cmdOutputResultToStr = \outputRes ->
    when outputRes is
        Ok { stdout, stderr: _ } -> 
            output = 
                stdout 
                    |> stripAnsiControl
                    |> Str.fromUtf8 
                    |> Result.withDefault "Could not parse output: Bad UTF-8"
                    
            "Command output:\n\n$(output)"
        Err (CmdOutputError outErr) -> "Error: $(outputErrToStr outErr)"

## Convert a CommandOutputError to a Str
outputErrToStr : (Output, CommandErr) -> Str
outputErrToStr = \(output, err) ->
    when err is
        ExitCode code -> 
            stderr = output.stderr |> stripAnsiControl |> Str.fromUtf8 |> Result.withDefault ""
            stdout = output.stdout |> stripAnsiControl |> Str.fromUtf8 |> Result.withDefault ""
            "Child exited with non-zero code: $(Num.toStr code)\n$(stdout)\n$(stderr)"
        KilledBySignal -> "Child was killed by signal"
        IOError ioErr -> "IOError executing: $(ioErr)"

## Strip ANSI control sequences from a list of bytes. (Ensures proper JSON serialization)
stripAnsiControl : List U8 -> List U8
stripAnsiControl = \bytes ->
    when List.findFirstIndex bytes (\b -> b == 27) is
        Ok escapeIndex -> 
            { before: lhs, others: remainder } = List.split bytes escapeIndex
            when List.findFirstIndex remainder (\b -> (b >= 'A' && b <= 'Z') || (b >= 'a' && b <= 'z')) is
                Ok controlIndex -> 
                    { before: _, others: rhs } = List.split remainder (controlIndex + 1)
                    List.concat lhs (stripAnsiControl rhs)
                Err _ -> bytes
        Err _ -> bytes
