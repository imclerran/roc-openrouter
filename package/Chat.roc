module [
    ChatRequestBody,
    ChatResponseBody,
    Message,
    Client,
    appendAssistantMessage,
    appendSystemMessage,
    appendUserMessage,
    buildHttpRequest,
    decodeErrorResponse,
    decodeResponse,
    decodeTopMessageChoice,
    encodeRequestBody,
    initClient,
]

import json.Json
import json.Option exposing [Option]

import Client
import Shared exposing [RequestObject, ApiError, dropLeadingGarbage, optionToStr]
import InternalTools exposing [ToolCall, ToolChoice]

Client : Client.Client

## The OpenAI ChatML standard message used to query the AI model.
Message : {
    role : Str,
    content : Str,
    toolCalls : Option (List ToolCall),
    name : Option Str,
    toolCallId : Option Str,
    cached: Bool,
}

## Internal ChatML message to decode messages from JSON. Allows optional fields.
DecodeMessage : {
    role : Str,
    content : Option Str,
    toolCalls : Option (List ToolCall),
    name : Option Str,
    toolCallId : Option Str,
}

## The structure of cached messages to be encoded to JSON for the API.
EncodeCacheMessage : {
    role : Str,
    content : List CacheContent,
    toolCalls : Option (List ToolCall),
    name : Option Str,
    toolCallId : Option Str,
}

## The structure of non-cached messages to be encoded to JSON for the API.
EncodeBasicMessage : {
    role : Str,
    content : Str,
    toolCalls : Option (List ToolCall),
    name : Option Str,
    toolCallId : Option Str,
}

## The message content of a cacheable message.
CacheContent : {
    type: Str,
    text: Str,
    cacheControl: Option { type: Str },
}

## The structure of the request body to be sent in the Http request
ChatRequestBody : {
    model : Str,
    messages : List Message,
    temperature : F32,
    topA : F32,
    topP : F32,
    topK : U64,
    frequencyPenalty : F32,
    presencePenalty : F32,
    repetitionPenalty : F32,
    minP : F32,
    seed : Option U64,
    maxTokens : Option U64,
    provider : {
        order : Option (List Str),
    },
    responseFormat : { type : Str },
    models : Option (List Str),
    route : Option Str,
    # tools: Option (List Tools.Tool),
    # toolChoice: Option Tools.ToolChoice,
}

## The structure of the JSON response body received from the OpenRouter API
ChatResponseBody : {
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

## Internal version of the chat response body to decode JSON responses
DecodeChatResponseBody : {
    id : Str,
    model : Str,
    object : Str,
    created : U64,
    choices : List {
        index : U8,
        message : DecodeMessage,
        finishReason : Option Str,
    },
    usage : {
        promptTokens : U64,
        completionTokens : U64,
        totalTokens : U64,
    },
}

## Initialize the OpenRouter API client with the required API key.
## Other parameters may optionally be set during initialization, or assigned later using the Client module setters.
initClient = Client.init

## Create a request object to be sent with basic-cli's Http.send using ChatML messages
buildHttpRequest : Client, List Message, { toolChoice ? ToolChoice } -> RequestObject
buildHttpRequest = \client, messages, { toolChoice ? Auto } ->
    body = buildRequestBody client
    tools =
        when Option.get client.tools is
            Some toolList -> toolList
            None -> []
    {
        method: Post,
        headers: [{ key: "Authorization", value: "Bearer $(client.apiKey)" }],
        url: client.url,
        mimeType: "application/json",
        body: encodeRequestBody body
        |> injectMessages messages
        |> InternalTools.injectTools tools
        |> InternalTools.injectToolChoice toolChoice,
        timeout: client.requestTimeout,
    }

## Build the request body to be sent in the Http request using ChatML messages
buildRequestBody : Client -> ChatRequestBody
buildRequestBody = \client -> {
    messages: [],
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

## Decode the JSON response body to a ChatML style request
decodeResponse : List U8 -> Result ChatResponseBody _
decodeResponse = \bodyBytes ->
    cleanedBody = dropLeadingGarbage bodyBytes
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult DecodeChatResponseBody
    decoded = Decode.fromBytesPartial cleanedBody decoder
    decoded.result
    |> Result.map \internalResponse -> {
        id: internalResponse.id,
        model: internalResponse.model,
        object: internalResponse.object,
        created: internalResponse.created,
        choices: internalResponse.choices
        |> List.map \{ index, message: internalMessage, finishReason: internalFinishReason } -> {
            index,
            message: convertInternalMessage internalMessage,
            finishReason: optionToStr internalFinishReason,
        },
        usage: internalResponse.usage,
    }

## Convert an DecodeMessage to a Message
convertInternalMessage : DecodeMessage -> Message
convertInternalMessage = \internalMessage -> {
    role: internalMessage.role,
    content:
    when Option.get internalMessage.content is
        Some text -> text
        None -> "",
    toolCalls: internalMessage.toolCalls,
    toolCallId: internalMessage.toolCallId,
    name: internalMessage.name,
    cached: Bool.false,
}

## Build a CacheContent object for a message
buildMessageContent : Str, Bool -> CacheContent
buildMessageContent = \text, cached -> { 
    type: "text", 
    text, 
    cacheControl: if cached then Option.some { type: "ephemeral" } else Option.none {},
}

## Decode the JSON response body to the first message in the list of choices
decodeTopMessageChoice : List U8 -> Result Message [ApiError ApiError, DecodingError, NoChoices, BadJson Str]
decodeTopMessageChoice = \responseBodyBytes ->
    when decodeResponse responseBodyBytes is
        Ok body ->
            when List.get body.choices 0 is
                Ok choice -> Ok choice.message
                Err _ -> Err NoChoices

        Err _ ->
            when decodeErrorResponse responseBodyBytes is
                Ok err -> Err (ApiError err.error)
                Err _ ->
                    when responseBodyBytes |> Str.fromUtf8 is
                        Ok str -> Err (BadJson str)
                        Err _ -> Err DecodingError

## Decode the JSON response body of an API error message
decodeErrorResponse = Shared.decodeErrorResponse

## Encode the request body to be sent in the Http request
encodeRequestBody : ChatRequestBody -> List U8
encodeRequestBody = \body ->
    Encode.toBytes
        body
        (
            Json.utf8With {
                fieldNameMapping: SnakeCase,
                emptyEncodeAsNull: Json.encodeAsNullOption { record: Bool.false },
            }
        )

## Inject the messages list into the request body, by encoding 
## the message to the correct format based on the cached flag.
injectMessages : List U8, List Message -> List U8
injectMessages = \bodyBytes, messages ->
    injectAt = List.walkWithIndexUntil bodyBytes 0 \_, _, i ->
        when List.dropFirst bodyBytes i is
            ['m', 'e', 's', 's', 'a', 'g', 'e', 's', '"', ':', '[', ..] -> Break (i + 11)
            ['m', 'e', 's', 's', 'a', 'g', 'e', 's', '"', ':', ' ', '[', ..] -> Break (i + 12)
            _ -> Continue 0

    if injectAt == 0 then
        bodyBytes
    else
        { before, others } = List.split bodyBytes injectAt
        messageBytes = messages |> List.map \message ->
            if message.cached && message.toolCallId == Option.none {} then
                messageToCacheMessage message
                |> Encode.toBytes (Json.utf8With { fieldNameMapping: SnakeCase, emptyEncodeAsNull: Json.encodeAsNullOption { record: Bool.false } })
                |> List.append ','
            else
                messageToBasicMessage message
                |> Encode.toBytes (Json.utf8With { fieldNameMapping: SnakeCase, emptyEncodeAsNull: Json.encodeAsNullOption { record: Bool.false } })
                |> List.append ','
            |> List.join
            |> List.dropLast 1
        List.join [before, messageBytes, others]

## Convert a Message to an EncodeCacheMessage
messageToCacheMessage : Message -> EncodeCacheMessage
messageToCacheMessage = \message -> {
    role: message.role,
    content: [buildMessageContent message.content message.cached],
    toolCalls: message.toolCalls,
    toolCallId: message.toolCallId,
    name: message.name,
}

## Convert a Message to an EncodeBasicMessage
messageToBasicMessage : Message -> EncodeBasicMessage
messageToBasicMessage = \message -> {
    role: message.role,
    content: message.content,
    toolCalls: message.toolCalls,
    toolCallId: message.toolCallId,
    name: message.name,
}

## Append a system message to the list of messages
appendSystemMessage : List Message, Str, { cached ? Bool } -> List Message
appendSystemMessage = \messages, text, { cached ? Bool.false } ->
    List.append messages { role: "system", content: text, toolCalls: Option.none {}, toolCallId: Option.none {}, name: Option.none {}, cached }

## Append a user message to the list of messages
appendUserMessage : List Message, Str, { cached ? Bool } -> List Message
appendUserMessage = \messages, text, { cached ? Bool.false } ->
    List.append messages { role: "user", content: text, toolCalls: Option.none {}, toolCallId: Option.none {}, name: Option.none {}, cached }

## Append an assistant message to the list of messages
appendAssistantMessage : List Message, Str, { cached ? Bool } -> List Message
appendAssistantMessage = \messages, text, { cached ? Bool.false } ->
    List.append messages { role: "assistant", content: text, toolCalls: Option.none {}, toolCallId: Option.none {}, name: Option.none {}, cached }
