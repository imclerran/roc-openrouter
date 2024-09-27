module { getUtcNow, utcToNanos } -> [utcNow]

import InternalTools exposing [Tool, buildTool]
import iso.DateTime

## Expose name, handler and tool for utcNow
utcNow = {
    name: tool.function.name,
    handler,
    tool,
}

## Tool definition for the utcNow function
tool : Tool
tool = buildTool "utcNow" "Get the current UTC time as an ISO 8601 string" []

## Handler for the utcNow tool
handler : Str -> Task Str _
handler = \_args ->
    getUtcNow! {}
        |> utcToNanos
        |> DateTime.fromNanosSinceEpoch
        |> DateTime.toIsoStr
        |> Task.ok