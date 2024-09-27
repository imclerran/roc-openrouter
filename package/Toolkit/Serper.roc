module { sendHttpReq, getEnvVar } -> [serper]

import InternalTools exposing [Tool, buildTool]

## Expose name, handler and tool for serper
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