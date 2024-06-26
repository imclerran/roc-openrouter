app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
    ai: "../package/main.roc",
}

import ai.Chat exposing [Message]
import cli.Env
import cli.Http
import cli.Stdin
import cli.Stdout
import cli.Task exposing [Task]

main =
    apiKey = getApiKey!
    model = getModelChoice!
    providerOrder = Dict.get preferredProviders model |> Result.withDefault []
    client = Chat.initClient { apiKey, model, providerOrder } 
    Stdout.line! "Using model: $(model)\n"
    Stdout.line! "Enter your questions below, or type 'Goodbye' to exit"
    Task.loop! { client, previousMessages: initializeMessages } loop
    Stdout.line (colorizeRole (Assistant "\nAssistant:  I have been a good chatbot. Goodbye! 😊"))

## The main loop for the chatbot
loop = \{ client, previousMessages } ->
    Stdout.write! "You: "
    query = Stdin.line!
    messages = Chat.appendUserMessage previousMessages query
    when query |> strToLower is
        "goodbye" -> Task.ok (Done { client, previousMessages: messages })
        "goodbye." -> Task.ok (Done { client, previousMessages: messages })
        "goodbye!" -> Task.ok (Done { client, previousMessages: messages })
        _ -> handlePrompt client messages

## Send the messages to the API, print the response, and return the updated messages in a Step
handlePrompt = \client, messages ->
    response = Http.send! (Chat.buildHttpRequest client messages)
    updatedMessages = getMessagesFromResponse messages response
    when List.last updatedMessages is
        Ok { role, content } if role == "assistant" ->
            Stdout.line! (colorizeRole (Assistant "\nAssistant: $(content)\n"))
            Task.ok (Step { client, previousMessages: updatedMessages })

        Ok { role, content } if role == "system" ->
            Stdout.line! (colorizeRole (System "\nSystem: $(content)\n"))
            Task.ok (Step { client, previousMessages: updatedMessages })

        _ ->
            Task.ok (Step { client, previousMessages: updatedMessages })

## decode the response from the OpenRouter API and append the first message to the list of messages
getMessagesFromResponse : List Message, Http.Response -> List Message
getMessagesFromResponse = \messages, response ->
    when Chat.decodeTopMessageChoice response.body is 
        Ok message -> List.append messages message
        Err (HttpError err) -> Chat.appendSystemMessage messages err.message
        Err _ -> Chat.appendSystemMessage messages "Error decoding API response"

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
    []
    |> Chat.appendSystemMessage
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

## Define the preferred providers for each model
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

## Convert a string to lowercase
strToLower : Str -> Str
strToLower = \str ->
    str
    |> Str.toUtf8
    |> List.walk [] \acc, elem ->
        acc |> List.append (if elem >= 65 && elem <= 90 then elem + 32 else elem)
    |> Str.fromUtf8
    |> Result.withDefault str

colorizeRole : [Assistant Str, System Str, User Str] -> Str
colorizeRole = \role ->
    when role is
        Assistant msg -> "\u(001b)[35m$(msg)\u(001b)[0m"
        System msg -> "\u(001b)[34m$(msg)\u(001b)[0m"
        User msg -> "\u(001b)[0m$(msg)\u(001b)[0m"
