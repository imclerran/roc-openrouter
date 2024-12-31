## A prebuilt tool for interacting with Wolfram Alpha.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [wolframShortAnswer]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [
##     (wolframShortAnswer.name, wolframShortAnswer.handler),
## ]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
##
## #...
##
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { sendHttpReq, getEnvVar } -> [wolframShortAnswer]

import json.Json
import InternalTools exposing [Tool]
import Shared exposing [urlEncode]

## Expose name, handler and tool for shortAnswer.
##
## This tool allows the model to ask Wolfram Alpha a question and get a short answer.
wolframShortAnswer : { name : Str, handler : Str -> Task Str *, tool : Tool }
wolframShortAnswer = {
    name: shortAnswerTool.function.name,
    handler: shortAnswerHandler,
    tool: shortAnswerTool,
}

## Tool definition for the shortAnswer function
shortAnswerTool : Tool
shortAnswerTool =
    inputParam = {
        name: "input",
        type: "string",
        description:
        """
        The question to ask Wolfram Alpha.
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

## Handler for the shortAnswer tool
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
                url: "http://api.wolframalpha.com/v1/result?i=$(urlEncode input)&appid=$(appId)",
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
