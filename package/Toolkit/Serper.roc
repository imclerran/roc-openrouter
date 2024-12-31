## A prebuilt tool for interacting with the serper.dev google search API.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [serper]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [(serper.name, serper.handler)]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
##
## #...
##
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages |> Tools.handleToolCalls! client toolHandlerMap
## ```

module { sendHttpReq, getEnvVar } -> [serper]

import InternalTools exposing [Tool, buildTool]

## Expose name, handler and tool for serper.
##
## This tool allows the model to search google using the serper.dev API.
serper = {
    name: tool.function.name,
    handler,
    tool,
}

## Tool definition for the serper function
tool : Tool
tool = 
    queryParam = {
        name: "q",
        type: "string",
        description: "The search query to send to the serper.dev API",
        required: Bool.true,
    }
    buildTool "serper" "Access to the serper.dev google search API" [queryParam]

## Handler for the serper tool
handler : Str -> Task Str _
handler = \args ->
    apiKey = getEnvVar! "SERPER_API_KEY"
    request = {
        method: Post,
        headers: [{ key: "X-API-KEY", value: apiKey }],
        url: "https://google.serper.dev/search",
        mimeType: "application/json",
        body: args |> Str.toUtf8,
        timeout: NoTimeout,
    }
    when sendHttpReq request |> Task.result! is
        Ok response -> 
            response.body 
            |> Str.fromUtf8 
            |> Result.withDefault "Failed to decode API response"
            |> Task.ok
        Err _ -> 
            "Failed to get response from serper.dev"
            |> Task.ok