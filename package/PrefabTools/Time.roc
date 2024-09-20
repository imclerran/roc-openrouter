module { getUtcNow, utcToNanos } -> [utcNowTool, utcNow]

import InternalTools exposing [Tool, buildTool]
import iso.DateTime

## tool for the utcNow function
utcNowTool : Tool
utcNowTool = buildTool "utcNow" "Get the current UTC time as an ISO 8601 string" []

## Handler for the utcNow tool
utcNow : Str -> Task Str _
utcNow = \_args ->
    getUtcNow! {}
        |> utcToNanos
        |> DateTime.fromNanosSinceEpoch
        |> DateTime.toIsoStr
        |> Task.ok