module [
    ChatResponse, 
    PromptResponse,
    ErrorResponse,
    RequestObject, 
    Message,
    initClient,
    buildChatRequest,
    buildPromptRequest, 
    appendAssistantMessage, 
    appendSystemMessage, 
    appendUserMessage,
    decodeChatResponse,
    decodePromptResponse,
    decodeErrorResponse,
    formatLLamaPromptStr,
    formatLLamaPromptWithHistory,
    updateLLamaConversationHistory,
]

import json.Json
import json.Option exposing [Option]

import Client exposing [Client]

TimeoutConfig : [TimeoutMilliseconds U64, NoTimeout]

## The request object to be sent with basic-cli's Http.send 
RequestObject : {
    method : [Post],
    headers : List [Header Str Str],
    mimeType : Str,
    url : Str,
    body : List U8,
    timeout : TimeoutConfig,
}

## The OpenAI ChatML standard message used to query the AI model.
Message : {
    role: Str,
    content: Str,
}

## The structure of the request body to be sent in the Http request
RequestBody : {
    model : Str,
    messages: Option (List Message),
    prompt: Option Str,
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
    # responseFormat: Option { type: Str },
    models: Option (List Str),
    route: Option Str,
}

## The structure of the JSON response from the OpenAI API
ChatResponse : {
    id : Str,
    model : Str,
    object : Str,
    created : U64,
    choices : List {
        index : U8,
        message : Message,
        finishReason : Str,
    },
    usage : {
        promptTokens : U64,
        completionTokens : U64,
        totalTokens : U64,
    },
}

PromptResponse : {
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

## The structure of the JSON error response from the OpenAI API
ErrorResponse : {
    error : {
        code : U16,
        message : Str,
    },
}

initClient = Client.init

## Create a request object to be sent with basic-cli's Http.send using ChatML messages
buildChatRequest : Client, List Message -> RequestObject
buildChatRequest = \client, messages -> 
    body = buildChatRequestBody client messages
    {
        method: Post, 
        headers: [Header "Authorization" "Bearer $(client.apiKey)"],
        url: client.url,
        mimeType: "application/json",
        body: encodeRequestBody body,
        timeout: client.requestTimeout,
    }

## Build the request body to be sent in the Http request using ChatML messages
buildChatRequestBody : Client, List Message -> RequestBody
buildChatRequestBody = \client, messages -> 
    {   
        model: client.model, 
        messages: Option.some messages, 
        prompt: Option.none {},
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
        # responseFormat: client.responseFormat,
        models: client.models,
        route: client.route,
    }

## Create a request object to be sent with basic-cli's Http.send using a prompt string
buildPromptRequest : Client, Str -> RequestObject
buildPromptRequest = \client, prompt -> 
    body = buildPromptRequestBody client prompt
    {
        method: Post,
        headers: [Header "Authorization" "Bearer $(client.apiKey)"],
        url: client.url,
        mimeType: "application/json",
        body: encodeRequestBody body,
        timeout: client.requestTimeout,
    }

## Build the request body to be sent in the Http request using a prompt string
buildPromptRequestBody : Client, Str -> RequestBody
buildPromptRequestBody = \client, prompt -> 
    {   
        model: client.model, 
        messages: Option.none {},
        prompt: Option.some prompt,
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
        # responseFormat: client.responseFormat,
        models: client.models,
        route: client.route,
    }

## Decode the JSON response to a ChatML style request
decodeChatResponse : List U8 -> Result ChatResponse _
decodeChatResponse = \bodyBytes -> 
    cleanedBody = dropLeadingGarbage bodyBytes
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult ChatResponse
    decoded = Decode.fromBytesPartial cleanedBody decoder
    decoded.result

## Decode the JSON response to a prompt string request
decodePromptResponse : List U8 -> Result PromptResponse _
decodePromptResponse = \bodyBytes -> 
    cleanedBody = dropLeadingGarbage bodyBytes
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult PromptResponse
    decoded = Decode.fromBytesPartial cleanedBody decoder
    decoded.result

## Decode the JSON error response from the OpenRouter API
decodeErrorResponse : List U8 -> Result ErrorResponse _
decodeErrorResponse = \bodyBytes -> 
    cleanedBody = dropLeadingGarbage bodyBytes
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult ErrorResponse
    decoded = Decode.fromBytesPartial cleanedBody decoder
    decoded.result

dropLeadingGarbage : List U8 -> List U8
dropLeadingGarbage = \bytes -> 
    when List.findFirstIndex bytes \elem -> elem == '{' is
        Ok idx -> List.dropFirst bytes idx
        Err _ -> bytes 

## Encode the request body to be sent in the Http request
encodeRequestBody : RequestBody -> List U8
encodeRequestBody = \body -> 
    Encode.toBytes body (Json.utf8With { 
        fieldNameMapping: SnakeCase, 
        emptyEncodeAsNull: Json.encodeAsNullOption { record: Bool.false },
    })

llamaPromptStartTag = "[INST] "
llamaPromptEndTag = " [/INST]"
llamaSysMessageStartTag = "<<SYS>>\n"
llamaSysMessageEndTag = "\n<<SYS>>\n\n"
llamaExchangeStartTag = "<s>"
llamaExchangeEndTag = "</s>\n"

formatLLamaPromptStr : { prompt: Str, sysMessage? Str } -> Str
formatLLamaPromptStr = \{ prompt, sysMessage? "" } -> 
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

formatLLamaPromptWithHistory : Str, Str -> Str
formatLLamaPromptWithHistory = \prompt, conversationHistory -> 
    conversationHistory
    |> Str.concat llamaExchangeStartTag
    |> Str.concat prompt
    
updateLLamaConversationHistory : { promptStr: Str, botReply: Str, conversationHistory? Str} -> Str
updateLLamaConversationHistory = \{ promptStr, botReply, conversationHistory? "" } -> 
    conversationHistory
    |> Str.concat llamaExchangeStartTag
    |> Str.concat promptStr
    |> Str.concat botReply
    |> Str.concat llamaExchangeEndTag

## Append a system message to the list of messages
appendSystemMessage : List Message, Str -> List Message
appendSystemMessage = \messages, content -> List.append messages { role: "system", content }

## Append a user message to the list of messages
appendUserMessage : List Message, Str -> List Message
appendUserMessage = \messages, content -> List.append messages { role: "user", content }

## Append an assistant message to the list of messages
appendAssistantMessage : List Message, Str -> List Message
appendAssistantMessage = \messages, content -> List.append messages { role: "assistant", content }
