module { sendHttpReq, getEnvVar } -> [shortAnswer]

import InternalTools exposing [Tool]

shortAnswer = {
    name: tool.function.name,
    handler: shortAnswerHandler,
    tool: shortAnswerTool,
}

ShortAnswerTool : Tool
shortAnswerTool =
    inputParam = {
        name: "input",
        type: "string",
        description: 
            """
            The question to ask Wolfram Alpha. Must Be URL encoded.
            """,
        required: Bool.true,
    }
    InternalTools.buildTool 
        "wolframShortAnswer" 
        """
        Ask Wolfram Alpha a question and get a short answer. 
        Wolfram can answer questions in many categories, including but not limited to:
        Mathematical computations, unit conversions, fact-based queries, scientific 
        questions, weather and location based data, date and time queries, financial 
        and economic data, historical events, and general knowledge questions.
        """ 
        [inputParam]

shortAnswerHandler : Str -> Task Str _
shortAnswerHandler = \args ->
    decoded : Decode.DecodeResult { input : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Task.ok "Failed to decode args"

        Ok { input } ->
            appId = getEnvVar! "WOLFRAMALPHA_APP_ID"
            request = {
                method: Get,
                headers: [],
                url: "http://api.wolframalpha.com/v1/result?i=$(input)&appid=$(appId)",
                mimeType: "application/json",
                body: [],
                timeout: NoTimeout,
            }
            when sendHttpReq request |> Task.result! is
                Ok response ->
                    response.body
                    |> Str.fromUtf8
                    |> Result.withDefault "Failed to decode API response"
                    |> Task.ok

                Err _ ->
                    "Failed to get response from Wolfram Alpha"
                    |> Task.ok