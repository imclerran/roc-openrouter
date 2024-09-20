module { sendHttpReq, getEnvVar } -> [serper, serperTool]

import InternalTools exposing [Tool, buildTool]

serperTool : Tool
serperTool = 
    queryParam = {
        name: "q",
        type: "string",
        description: "The search query to send to the serper.dev API",
        required: Bool.true,
    }
    buildTool "serper" "Access to the serper.dev google search API" [queryParam]

serper : Str -> Task Str _
serper = \args ->
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