app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Stdin
import cli.Http
import cli.Task exposing [Task]
import cli.Env  
import ai.Api as AI

main =
    apiKey = getApiKey!
    model = getModelChoice!
    providers = Dict.get preferredProviders model |> Result.withDefault []
    client = AI.init { apiKey }
        |> AI.setModel model
        |> AI.setProviderOrder providers
        |> AI.setTemperature 0.7
    query = "What is the meaning of life?"
    messages = initializeMessages |> AI.appendUserMessage query
    request = AI.createRequest client messages
    response = Http.send! request
    responseBody =
        when response |> Http.handleStringResponse is
            Err err -> crash (Http.errorToString err)
            Ok body -> body |> Str.toUtf8
    
    when AI.decodeApiResponse responseBody is
        Ok body ->
            when List.first body.choices is
                Ok choice -> Stdout.line choice.message.content
                Err _ -> Stdout.line "No choices found in API response"
        Err _ -> 
            when AI.decodeErrorResponse responseBody is
                Ok { error } -> Stdout.line error.message
                Err _ -> Stdout.line "Failed to decode API response"

## Get the API key from the environmental variable
getApiKey =
    keyResult <- Task.attempt (Env.var "OPENROUTER_API_KEY")
    when keyResult is
        Ok key -> Task.ok key
        Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"

## Prompt the user to choose a model and return the selected model
getModelChoice : Task Str _
getModelChoice =
    Stdout.line! modelMenuString
    Stdout.write! "Choose a model (or press enter): "
    choiceStr <- Stdin.line |> Task.map
    Dict.get modelChoices choiceStr
    |> Result.withDefault defaultModel

## Initialize the message list with a system message
initializeMessages = 
    [] |> AI.appendSystemMessage 
    """
    You are a helpful assistant, who answers questions in a concise and friendly manner. 
    If you do not have knowledge about the on the users inquires about, you should politely tell them you cannot help.
    """

## The default model selection
defaultModel = "meta-llama/llama-3-8b-instruct:free"

## Define the model choices
modelChoices =
    Dict.empty {}
    |> Dict.insert "1" defaultModel
    |> Dict.insert "2" "mistralai/mixtral-8x7b-instruct"
    |> Dict.insert "3" "mistralai/mistral-small"
    |> Dict.insert "4" "mistralai/mistral-large"
    |> Dict.insert "5" "gryphe/mythomax-l2-13b"
    |> Dict.insert "6" "microsoft/wizardlm-2-8x22b"
    |> Dict.insert "7" "openai/gpt-3.5-turbo"
    |> Dict.insert "8" "openai/gpt-4o"
    |> Dict.insert "9" "google/gemini-pro-1.5"

preferredProviders =
    Dict.empty {}
    |> Dict.insert "meta-llama/llama-3-8b-instruct:free" []
    |> Dict.insert "mistralai/mixtral-8x7b-instruct" ["Fireworks", "Together", "Lepton"]
    |> Dict.insert "mistralai/mistral-small" []
    |> Dict.insert "mistralai/mistral-large" []
    |> Dict.insert "gryphe/mythomax-l2-13b" ["DeepInfra", "Fireworks", "Together"]
    |> Dict.insert "microsoft/wizardlm-2-8x22b" []
    |> Dict.insert "openai/gpt-3.5-turbo" []
    |> Dict.insert "openai/gpt-4o" []
    |> Dict.insert "google/gemini-pro-1.5" []


## Generate a string to print for the model selection menu
modelMenuString =
    modelChoices
    |> Dict.walk "" \string, key, value ->
        string
        |> Str.concat key
        |> Str.concat ") "
        |> Str.concat value
        |> Str.concat (if key == "1" then " (default)\n" else "\n")


