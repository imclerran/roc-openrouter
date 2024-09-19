app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.2/FH4N0Sw-JSFXJfG3j54VEDPtXOoN-6I9v_IA8S18IGk.tar.br",
    iso: "https://github.com/imclerran/roc-isodate/releases/download/v0.5.0/ptg0ElRLlIqsxMDZTTvQHgUSkNrUSymQaGwTfv0UEmk.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.6/tzLZlg6lNmgU4uYtQ_zCmSr1AptHxZ8VBfE-O9JCudw.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Stdin
import cli.Http
import cli.Env
import cli.Utc

import ai.Chat exposing [Client, Message]
import ai.Tools exposing [Tool]
import ansi.Core as Ansi
import iso.DateTime
import json.Json
import json.Option

main : Task {} _
main =
    apiKey = getApiKey!
    client = Chat.initClient { apiKey, model: "openai/gpt-4", tools: [utcNowTool, toCstTool, toCdtTool] }
    Stdout.line! ("Assistant: Ask me about the time!\n" |> Ansi.color { fg: Standard Cyan })
    Task.loop! { client, previousMessages: [] } loop

## The main loop of the program
loop : { client : Client, previousMessages : List Message } -> Task [Done {}, Step _] _
loop = \{ client, previousMessages } ->
    Stdout.write! "You: "
    query = Stdin.line!
    messages = Chat.appendUserMessage previousMessages query
    when query |> strToLower is
        "goodbye" -> Task.ok (Done {})
        "goodbye." -> Task.ok (Done {})
        "goodbye!" -> Task.ok (Done {})
        _ -> handlePrompt client messages

## Handle the prompt and send the request to the OpenRouter API
handlePrompt : Client, List Message -> Task [Step { client : Client, previousMessages : List Message }] _
handlePrompt = \client, messages ->
    response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
    updatedMessages = getMessagesFromResponse messages response |> handleToolCalls! client
    printLastMessage! updatedMessages
    Task.ok (Step { client, previousMessages: updatedMessages })

## Handle tool calls in the messages
handleToolCalls : List Message, Client -> Task (List Message) _
handleToolCalls = \messages, client ->
    when List.last messages is
        Ok { role, toolCalls: tcs } if role == "assistant" ->
            when Option.get tcs is
                Some toolCalls ->
                    toolMessages = Tools.callTools! toolCalls toolHandlerMap
                    messagesWithTools = List.join [messages, toolMessages]
                    response = Http.send (Chat.buildHttpRequest client messagesWithTools {}) |> Task.result!
                    messagesWithResponse = getMessagesFromResponse messagesWithTools response
                    handleToolCalls messagesWithResponse client

                None -> Task.ok messages

        _ -> Task.ok messages

# Print the last message in the list of messages. Will only print assistant messages.
printLastMessage : List Message -> Task {} _
printLastMessage = \messages ->
    when List.last messages is
        Ok { role, content } if role == "assistant" ->
            Stdout.line! ("\nAssistant: $(content)\n" |> Ansi.color { fg: Standard Magenta })

        _ -> Task.ok {}

## decode the response from the OpenRouter API and append the first message to the list of messages
getMessagesFromResponse : List Message, Result Http.Response _ -> List Message
getMessagesFromResponse = \messages, responseRes ->
    when responseRes is
        Ok response ->
            when Chat.decodeTopMessageChoice response.body is
                Ok message -> List.append messages message
                Err (ApiError err) -> Chat.appendSystemMessage messages "API error: $(err.message)"
                Err NoChoices -> Chat.appendSystemMessage messages "No choices in API response"
                Err (BadJson str) -> Chat.appendSystemMessage messages "Could not decode JSON response:\n$(str)"
                Err DecodingError -> Chat.appendSystemMessage messages "Error decoding API response"

        Err (HttpErr err) ->
            Chat.appendSystemMessage messages (Http.errorToString err)

## Convert a string to lowercase
strToLower : Str -> Str
strToLower = \str ->
    str
    |> Str.toUtf8
    |> List.walk [] \acc, elem ->
        acc |> List.append (if elem >= 65 && elem <= 90 then elem + 32 else elem)
    |> Str.fromUtf8
    |> Result.withDefault str

## Get the API key from the environmental variable
getApiKey : Task Str _
getApiKey =
    Task.attempt (Env.var "OPENROUTER_API_KEY") \keyResult ->
        when keyResult is
            Ok key -> Task.ok key
            Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"

## tool for the utcNow function
utcNowTool : Tool
utcNowTool =
    Tools.buildTool
        "utcNow"
        "Get the current UTC time as an ISO 8601 string"
        []

## Handler for the utcNow tool
utcNow : Str -> Task Str _
utcNow = \_args ->
    now = Utc.now! {}
    now
    |> Utc.toNanosSinceEpoch
    |> DateTime.fromNanosSinceEpoch
    |> DateTime.toIsoStr
    |> Task.ok

## tool for the toCdt function
toCdtTool : Tools.Tool
toCdtTool =
    Tools.buildTool
        "toCdt"
        "Convert a UTC time to a CDT time"
        [
            {
                name: "utcTime",
                type: "string",
                description: "An ISO 8601 formatted time to convert from UTC to CDT",
                required: Bool.true,
            },
        ]

## Handler for the toCdt tool
toCdt : Str -> Task Str _
toCdt = \args ->
    { utcTime } =
        args
            |> Str.toUtf8
            |> Decode.fromBytes Json.utf8
            |> Task.fromResult!
    utcTime
        |> DateTime.fromIsoStr
        |> Task.fromResult!
        |> DateTime.addHours -6
        |> DateTime.toIsoStr
        |> Task.ok

## tool for the toCst function
toCstTool : Tools.Tool
toCstTool =
    Tools.buildTool
        "toCst"
        "Convert a UTC time to a CST time"
        [
            {
                name: "utcTime",
                type: "string",
                description: "An ISO 8601 formatted time to convert from UTC to CST",
                required: Bool.true,
            },
        ]

## Handler for the toCst tool
toCst : Str -> Task Str _
toCst = \args ->
    { utcTime } =
        args
            |> Str.toUtf8
            |> Decode.fromBytes Json.utf8
            |> Task.fromResult!
    utcTime
        |> DateTime.fromIsoStr
        |> Task.fromResult!
        |> DateTime.addHours -5
        |> DateTime.toIsoStr
        |> Task.ok

## Map of tool names to tool handlers
toolHandlerMap : Dict Str (Str -> Task Str _)
toolHandlerMap =
    Dict.empty {}
    |> Dict.insert "utcNow" utcNow
    |> Dict.insert "toCdt" toCdt
    |> Dict.insert "toCst" toCst
