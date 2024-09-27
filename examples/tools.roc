app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.6/tzLZlg6lNmgU4uYtQ_zCmSr1AptHxZ8VBfE-O9JCudw.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Stdin
import cli.Http
import cli.Env

import ai.Chat exposing [Message]
import ai.Tools { sendHttpReq: Http.send }
import ai.Toolkit.Serper { sendHttpReq: Http.send, getEnvVar: Env.var } exposing [serper]
import ansi.Core as Ansi

main : Task {} _
main =
    apiKey = getApiKey!
    client = Chat.initClient { apiKey, model: "openai/gpt-4o", tools: [serper.tool] }
    Stdout.line! ("Assistant: Ask me about the weather, or anything on the web!\n" |> Ansi.color { fg: Standard Cyan })
    Task.loop! { previousMessages: [] } \{ previousMessages } -> ## Task.loop function must be inline due to roc issue #7116
        Stdout.write! "You: "
        query = Stdin.line!
        when query is
            "goodbye" | "quit" | "exit" -> Task.ok (Done {})
            _ ->
                messages = Chat.appendUserMessage previousMessages query
                response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
                updatedMessages = getMessagesFromResponse messages response |> Tools.handleToolCalls! client toolHandlerMap
                printLastMessage! updatedMessages
                Task.ok (Step { previousMessages: updatedMessages })

## Get the API key from the environmental variable
getApiKey : Task Str _
getApiKey =
    Task.attempt (Env.var "OPENROUTER_API_KEY") \keyResult ->
        when keyResult is
            Ok key -> Task.ok key
            Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"

# Print the last message in the list of messages. Will only print assistant and system messages.
printLastMessage : List Message -> Task {} _
printLastMessage = \messages ->
    when List.last messages is
        Ok { role, content } if role == "assistant" ->
            Stdout.line! ("\nAssistant: $(content)\n" |> Ansi.color { fg: Standard Magenta })

        Ok { role, content } if role == "system" ->
            Stdout.line! ("\nAssistant: $(content)\n" |> Ansi.color { fg: Standard Cyan })

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

## Map of tool names to tool handlers
toolHandlerMap : Dict Str (Str -> Task Str _)
toolHandlerMap = Dict.fromList [(serper.name, serper.handler)]
