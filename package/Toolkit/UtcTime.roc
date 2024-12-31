## A prebuilt tool for getting the current UTC time.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [utcNow]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [(utcNow.name, utcNow.handler)]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
##
## #...
##
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { getUtcNow, utcToNanos } -> [utcNow]

import InternalTools exposing [Tool, buildTool]
import iso.DateTime

## Expose name, handler and tool for utcNow.
##
## This tool allows the model to get the current UTC time as an ISO 8601 string.
utcNow : { name : Str, handler : Str -> Task Str *, tool : Tool }
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