module [
    PromptRequestBody, 
    PromptResponseBody,
    buildHttpRequest, 
    buildRequestBody, 
    decodeErrorResponse, 
    decodeErrorResponse, 
    decodeResponse, 
    decodeTopTextChoice,
    encodeRequestBody, 
    formatLLamaPrompt, 
    formatLLamaPromptWithHistory, 
    initClient, 
    updateLLamaConversationHistory,
]

import json.Json
import json.Option exposing [Option]

import Client exposing [Client]
import Shared exposing [RequestObject]

## The structure of the request body to be sent in the Http request
PromptRequestBody : {
    prompt: Str,
    model : Str,
    temperature: F32,
    topA: F32,
    topP: F32,
    topK: U64,
    frequencyPenalty: F32,
    presencePenalty: F32,
    repetitionPenalty: F32,
    minP: F32,
    seed: Option U64,
    maxTokens: Option U64,
    provider: {
        order: Option (List Str),
    },
    responseFormat: { type: Str },
    models: Option (List Str),
    route: Option Str,
}

## The structure of the JSON response body received from the OpenRouter API
PromptResponseBody : {
    id: Str,
    model: Str,
    object: Str,
    created: U64,
    choices: List { text: Str, finishReason: Str },
    usage: {
        promptTokens : U64,
        completionTokens : U64,
        totalTokens : U64,
    },
}

## Initialize the OpenRouter API client with the required API key.
## Other parameters may optionally be set during initialization, or assigned later using the Client module setters.
initClient = Client.init

## Create a request object to be sent with basic-cli's Http.send using a prompt string
buildHttpRequest : Client, Str -> RequestObject
buildHttpRequest = \client, prompt -> 
    body = buildRequestBody client prompt
    {
        method: Post,
        headers: [Header "Authorization" "Bearer $(client.apiKey)"],
        url: client.url,
        mimeType: "application/json",
        body: encodeRequestBody body,
        timeout: client.requestTimeout,
    }

## Build the request body to be sent in the Http request using a prompt string
buildRequestBody : Client, Str -> PromptRequestBody
buildRequestBody = \client, prompt -> 
    {   
        prompt,
        model: client.model,
        temperature: client.temperature,
        topA: client.topA,
        topP: client.topP,
        topK: client.topK,
        frequencyPenalty: client.frequencyPenalty,
        presencePenalty: client.presencePenalty,
        repetitionPenalty: client.repetitionPenalty,
        minP: client.minP,
        seed: client.seed,
        maxTokens: client.maxTokens,
        provider: { order: client.providerOrder },
        responseFormat: client.responseFormat,
        models: client.models,
        route: client.route,
    }

## Encode the request body to be sent in the Http request
encodeRequestBody : PromptRequestBody -> List U8
encodeRequestBody = \body -> 
    Encode.toBytes body (Json.utf8With { 
        fieldNameMapping: SnakeCase, 
        emptyEncodeAsNull: Json.encodeAsNullOption { record: Bool.false },
    })

## Decode the JSON response to a prompt string request
decodeResponse : List U8 -> Result PromptResponseBody _
decodeResponse = \bodyBytes -> 
    cleanedBody = Shared.dropLeadingGarbage bodyBytes
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult PromptResponseBody
    decoded = Decode.fromBytesPartial cleanedBody decoder
    decoded.result

## Decode the JSON response body to the first message in the list of choices
decodeTopTextChoice : List U8 -> Result Str [InvalidResponse, NoChoices]
decodeTopTextChoice = \responseBodyBytes -> 
    when decodeResponse responseBodyBytes is
        Ok body -> 
            when List.get body.choices 0 is
                Ok choice -> Ok choice.text
                Err _ -> Err NoChoices
        Err _ -> Err InvalidResponse

## Decode the JSON response body of an API error message
decodeErrorResponse = Shared.decodeErrorResponse

llamaPromptStartTag = "[INST] "
llamaPromptEndTag = " [/INST]"
llamaSysMessageStartTag = "<<SYS>>\n"
llamaSysMessageEndTag = "\n<<SYS>>\n\n"
llamaExchangeStartTag = "<s>"
llamaExchangeEndTag = "</s>\n"

## ```
## [INST]
## <<SYS>>
## system message here
## <<SYS>>
## prompt here
## [/INST]
## ```
formatLLamaPrompt : { prompt: Str, sysMessage? Str } -> Str
formatLLamaPrompt = \{ prompt, sysMessage? "" } -> 
    when sysMessage is
        "" -> 
            llamaPromptStartTag
            |> Str.concat prompt
            |> Str.concat llamaPromptEndTag
        _ -> 
            llamaPromptStartTag
            |> Str.concat llamaSysMessageStartTag
            |> Str.concat sysMessage
            |> Str.concat llamaSysMessageEndTag
            |> Str.concat prompt
            |> Str.concat llamaPromptEndTag

## ```
## <s>1st exchange</s>
## <s>...</s>
## <s>Nth exchange</s>
## <s>[INST] 
## <<SYS>>
## system message here
## <<SYS>>
## prompt here
## [/INST]
## ```
formatLLamaPromptWithHistory : Str, Str -> Str
formatLLamaPromptWithHistory = \prompt, conversationHistory -> 
    conversationHistory
    |> Str.concat llamaExchangeStartTag
    |> Str.concat prompt

## ```
## <s>[INST]
## <<SYS>>
## system message here
## <<SYS>>
## first prompt here
## [/INST]first bot reply here</s>
## <s>2nd exchange</s>
## <s>...</s>
## <s>Nth exchange</s>
## ```
updateLLamaConversationHistory : { promptStr: Str, botReply: Str, conversationHistory? Str} -> Str
updateLLamaConversationHistory = \{ promptStr, botReply, conversationHistory? "" } -> 
    conversationHistory
    |> Str.concat llamaExchangeStartTag
    |> Str.concat promptStr
    |> Str.concat botReply
    |> Str.concat llamaExchangeEndTag