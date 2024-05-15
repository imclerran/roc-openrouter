app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Env
import cli.Http
import cli.Task
import ai.OpenRouter as AI

main = 
    apiKey = getApiKey!
    client = AI.init { apiKey, model: "openai/gpt-4o" }
        |> AI.setTools [getNameTool]
    query = "What is your name?"
    systemPrompt = "You are an AI chatbot. You should use the provided tool to get your name. Please choose a one syllable noun and use the tool to get your name."
    messages = [] 
        |> AI.appendSystemMessage systemPrompt
        |> AI.appendUserMessage query
    response = Http.send! (AI.buildChatRequest client messages)
    responseBody =
        when response |> Http.handleStringResponse is
            Err err -> crash (Http.errorToString err)
            Ok body -> body |> Str.toUtf8
    when AI.decodeChatResponse responseBody is
        Ok body ->
            when List.get body.choices 0 is
                Ok choice -> Stdout.line choice.message.content
                Err _ -> Stdout.line "No choices found in API response"
        Err _ ->
            when AI.decodeErrorResponse responseBody is
                Ok { error } -> Stdout.line error.message
                Err _ -> Stdout.line "Failed to decode API response"
    


getNameTool = 
    AI.initializeTool "getName" "Get the name of the AI chatbot."
    |> AI.addToolParameter "noun" { type: "string", description: "A single syllable noun" } Required

# getName =\noun -> "$(noun)Mc$(noun)Face"

## Get the API key from the environmental variable
getApiKey =
    keyResult <- Task.attempt (Env.var "OPENROUTER_API_KEY")
    when keyResult is
        Ok key -> Task.ok key
        Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"