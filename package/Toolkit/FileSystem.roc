module { pathFromStr, pathToStr, listDir, isDir, readFile} -> [listDirectory, listFileTree, readFileContents]

import json.Json
import InternalTools exposing [Tool, buildTool]

listDirectory = {
    name: listDirectoryTool.function.name,
    handler: listDirectoryHandler,
    tool: listDirectoryTool,
}

listDirectoryTool : Tool
listDirectoryTool = 
    pathParam = {
        name: "path",
        type: "Str",
        description: "The unix style path to a directory. `..` is not allowed.",
        required: Bool.true,
    }
    buildTool "listDirectory" "List the contents of a directory" [pathParam]


listDirectoryHandler : Str -> Task Str _
listDirectoryHandler = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { path } ->
            if path |> Str.contains ".." then
                Task.ok "Invalid path: `..` is not allowed"
            else
                listDir (pathFromStr path)
                    |> Task.result!
                    |> Result.withDefault []
                    |> List.map pathToStr
                    |> Str.joinWith "\n"
                    |> Task.ok

listFileTree = {
    name: listFileTreeTool.function.name,
    handler: listFileTreeHandler,
    tool: listFileTreeTool,
}

listFileTreeTool : Tool
listFileTreeTool = 
    pathParam = {
        name: "path",
        type: "Str",
        description: "The unix style path to a directory. `..` is not allowed.",
        required: Bool.true,
    }
    buildTool "listFileTree" "List the contents of a directory and all subdirectories" [pathParam]

listFileTreeHandler : Str -> Task Str _
listFileTreeHandler = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { path } ->
            if path |> Str.contains ".." then
                Task.ok "Invalid path: `..` is not allowed"
            else
                fileTreeHelper (path |> pathFromStr |> listDir!) "" 0

fileTreeHelper : List path, Str, U64 -> Task Str _
fileTreeHelper = \paths, accumulation, depth ->
    prependNewline = \str -> if Str.isEmpty str then str else Str.concat "\n" str
    appendNewline = \str -> if Str.isEmpty str then str else Str.concat str "\n"
    buildStr = \previous, current, subcontents -> "$(appendNewline previous)$(current)$(subcontents)"

    when paths is
        [] -> 
            Task.ok accumulation

        [path, .. as pathsTail] ->
            if isDir! path then
                subcontents = fileTreeHelper! (listDir! path) "" (depth + 1) |> prependNewline
                newString = buildStr accumulation (pathToStr path) subcontents
                fileTreeHelper pathsTail newString depth
            else
                newString = buildStr accumulation (pathToStr path) ""
                fileTreeHelper pathsTail newString depth

readFileContents = {
    name: readFileContentsTool.function.name,
    handler: readFileContentsHandler,
    tool: readFileContentsTool,
}

readFileContentsTool : Tool
readFileContentsTool =
    pathParam = {
        name: "path",
        type: "Str",
        description: "The unix style path to a file. `..` is not allowed.",
        required: Bool.true,
    }
    buildTool "readFileContents" "Read the contents of a file. Must be a plain text file (any extension)." [pathParam]

readFileContentsHandler : Str -> Task Str _
readFileContentsHandler = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { path } ->
            if path |> Str.contains ".." then
                Task.ok "Invalid path: `..` is not allowed"
            else
                pathFromStr path
                    |> readFile
                    |> Task.result!
                    |> Result.mapErr (\_ -> "Failed to read file")
                    |> Result.map pathToStr
                    |> Result.withDefault "Failed to read file"
                    |> Task.ok