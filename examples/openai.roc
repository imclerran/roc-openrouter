app [main] {
    #cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    cli: platform "../../basic-cli/platform/main.roc",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Http
import cli.Task exposing [Task]
import cli.Env
import ai.Chat
import ai.Client

main =
    apiKey = getApiKey!
    client =
        Client.init { apiKey }
        |> Client.setUrl "https://api.openai.com/v1/chat/completions"
        |> Client.setModel "gpt-4o"
    messages = [] |> Chat.appendUserMessage "Hello, computer!"
    response = Http.send (Chat.buildHttpRequest client messages)
        |> Task.mapErr! \err -> 
                when err is HttpErr error -> crash (Http.errorToString error) #\err -> 
            # when err is
            #     HttpErr (BadBody str) -> crash "BadBody: $(str)"
            #     HttpErr (BadRequest str) -> crash "BadRequest: $(str)"
            #     HttpErr (BadStatus {code, body}) -> 
            #         when body |> Str.fromUtf8 is
            #             Ok bodyStr -> crash "BadStatus: $(bodyStr)"
            #             Err _ -> crash "BadStatus: $(Num.toStr code)"
            #     HttpErr NetworkError -> crash "NetworkError"
            #     HttpErr (Timeout ms) -> crash "Timeout after $(Num.toStr ms)ms"
    responseBody =
        when response |> Http.handleStringResponse is
            Err err -> crash (Http.errorToString err)
            Ok body -> body |> Str.toUtf8

    when Chat.decodeResponse responseBody is
        Ok body ->
            when List.first body.choices is
                Ok choice -> Stdout.line (choice.message.content |> Str.trim)
                Err _ -> Stdout.line "No choices found in API response"

        Err _ ->
            when Chat.decodeErrorResponse responseBody is
                Ok { error } -> Stdout.line error.message
                Err _ -> Stdout.line "Failed to decode API response"

## Get the API key from the environmental variable
getApiKey =
    keyResult <- Task.attempt (Env.var "OPENAI_API_KEY")
    when keyResult is
        Ok key -> Task.ok key
        Err VarNotFound -> crash "OPENAI_API_KEY environment variable not set"
