app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.2/FH4N0Sw-JSFXJfG3j54VEDPtXOoN-6I9v_IA8S18IGk.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Http
import cli.Env
import ai.Prompt
import ai.Client

main =
    apiKey = getApiKey!
    client =
        Client.init { apiKey }
        |> Client.setModel "mistralai/mixtral-8x7b-instruct"
        |> Client.setProviderOrder ["Fireworks", "Together", "Lepton"]
        |> Client.setTemperature 0.0
        |> Client.setTopP 1.0
        |> Client.setMaxTokens 8
    query = Prompt.formatLLamaPrompt { prompt: "Hello, computer!" }
    response = Http.send! (Prompt.buildHttpRequest client query)
    when Prompt.decodeTopTextChoice response.body is
        Ok text -> Stdout.line (text |> Str.trim)
        Err (ApiError error) -> Stdout.line error.message
        Err NoChoices -> Stdout.line "No choices found in API response"
        Err (BadJson str) -> Stdout.line "Invalid JSON response:\n$(str)"
        Err DecodingError -> Stdout.line "Invalid API response"

## Get the API key from the environmental variable
getApiKey =
    Task.attempt (Env.var "OPENROUTER_API_KEY") \keyResult ->
        when keyResult is
            Ok key -> Task.ok key
            Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"
