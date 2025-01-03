app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.6/tzLZlg6lNmgU4uYtQ_zCmSr1AptHxZ8VBfE-O9JCudw.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.2/FH4N0Sw-JSFXJfG3j54VEDPtXOoN-6I9v_IA8S18IGk.tar.br",
    ai: "../package/main.roc",
}

import ai.Chat exposing [Message]
import ai.Client
import ansi.Core as Ansi
import cli.Env
import cli.Http
import cli.Stdin
import cli.Stdout

main =
    apiKey = getApiKey!
    model = getModelChoice!
    providerOrder = Dict.get preferredProviders model |> Result.withDefault []
    client = Client.init { apiKey, model, providerOrder }
    Stdout.line! "Using model: $(model |> Ansi.color { fg: Standard Magenta })\n"
    Stdout.line! "Enter your questions below, or type 'Goodbye' to exit"
    Task.loop! { client, previousMessages: initializeMessages } loop
    "\nAssistant:  I have been a good chatbot. Goodbye! 😊"
    |> Ansi.color { fg: Standard Magenta }
    |> Stdout.line

## The main loop for the chatbot
loop = \{ client, previousMessages } ->
    Stdout.write! "You: "
    query = Stdin.line!
    messages = Chat.appendUserMessage previousMessages query {}
    when query |> strToLower is
        "goodbye" | "quit" | "exit" -> Task.ok (Done {})
        "change model" ->
            Stdout.line! ""
            model = getModelChoice!
            providerOrder = Dict.get preferredProviders model |> Result.withDefault []
            newClient =
                client
                |> Client.setModel model
                |> Client.setProviderOrder providerOrder
            Stdout.line! "Using model: $(model |> Ansi.color { fg: Standard Magenta })\n"
            Task.ok (Step { client: newClient, previousMessages })

        _ ->
            response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
            updatedMessages = Chat.updateMessageList response messages 
            printLastMessage! updatedMessages
            Task.ok (Step { client, previousMessages: updatedMessages })

## Get the API key from the environmental variable
getApiKey =
    Task.attempt (Env.var "OPENROUTER_API_KEY") \keyResult ->
        when keyResult is
            Ok key -> Task.ok key
            Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"

# Print the last message in the list of messages. Will only print assistant and system messages.
printLastMessage = \messages ->
    when List.last messages is
        Ok { role, content } if role == "assistant" ->
            Stdout.line! ("\nAssistant: $(content)\n" |> Ansi.color { fg: Standard Magenta })

        Ok { role, content } if role == "system" ->
            Stdout.line! ("\nSystem: $(content)\n" |> Ansi.color { fg: Standard Blue })

        _ -> Task.ok {}

## Prompt the user to choose a model and return the selected model
getModelChoice : Task Str _
getModelChoice =
    Task.loop {} \{} ->
        Stdout.line! modelMenuString
        Stdout.write! "Choose a model (or press enter): "
        choiceStr =
            Stdin.line!
                |> \str -> if str == "" then "1" else str
        if Dict.contains modelChoices choiceStr then
            Dict.get modelChoices choiceStr
            |> Result.withDefault defaultModel
            |> Done
            |> Task.ok
        else
            "Oops! Invalid model choice.\n" |> Ansi.color { fg: Standard Yellow } |> Stdout.line!
            Task.ok (Step {})

## Initialize the message list with a system message
initializeMessages =
    []
    |> Chat.appendSystemMessage
        """
        You are a helpful assistant, who answers questions in a concise and friendly manner. 
        If you do not have knowledge about the on the users inquires about, you should politely tell them you cannot help.
        """
        {}

## The default model selection
defaultModel = "google/gemini-flash-1.5-8b"

## Define the model choices
modelChoices =
    Dict.empty {}
    |> Dict.insert "1" defaultModel
    |> Dict.insert "2" "mistralai/mixtral-8x7b-instruct"
    |> Dict.insert "3" "x-ai/grok-beta"
    |> Dict.insert "4" "mistralai/mistral-large"
    |> Dict.insert "5" "gryphe/mythomax-l2-13b"
    |> Dict.insert "6" "microsoft/wizardlm-2-8x22b"
    |> Dict.insert "7" "openai/gpt-3.5-turbo"
    |> Dict.insert "8" "openai/gpt-4o"
    |> Dict.insert "9" "google/gemini-2.0-flash-thinking-exp:free"

## Define the preferred providers for each model
preferredProviders =
    Dict.empty {}
    |> Dict.insert defaultModel []
    |> Dict.insert "mistralai/mixtral-8x7b-instruct" ["Fireworks", "Together", "Lepton"]
    |> Dict.insert "x-ai/grok-beta" []
    |> Dict.insert "mistralai/mistral-large" []
    |> Dict.insert "gryphe/mythomax-l2-13b" ["DeepInfra", "Fireworks", "Together"]
    |> Dict.insert "microsoft/wizardlm-2-8x22b" []
    |> Dict.insert "openai/gpt-3.5-turbo" []
    |> Dict.insert "openai/gpt-4o" []
    |> Dict.insert "google/gemini-2.0-flash-thinking-exp:free" []

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
