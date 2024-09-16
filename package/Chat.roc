module [
    ChatRequestBody,
    ChatResponseBody,
    Message,
    appendAssistantMessage, 
    appendSystemMessage, 
    appendUserMessage,
    buildHttpRequest,
    buildRequestBody,
    decodeErrorResponse,
    decodeResponse,
    decodeTopMessageChoice,
    encodeRequestBody,
    initClient,
]

import json.Json
import json.Option exposing [Option]

import Shared exposing [RequestObject, ApiError, dropLeadingGarbage]
import Client exposing [Client]

## The OpenAI ChatML standard message used to query the AI model.
Message : {
    role: Str,
    content: Str,
}

## The structure of the request body to be sent in the Http request
ChatRequestBody : {
    model : Str,
    messages: List Message,
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
ChatResponseBody : {
    id : Str,
    model : Str,
    object : Str,
    created : U64,
    choices : List {
        index : U8,
        message : Message,
        #finishReason : Str, # finish reason is not always returned by some models, causing json decoding errors
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
buildHttpRequest : Client, List Message -> RequestObject
buildHttpRequest = \client, messages -> 
    body = buildRequestBody client messages
    {
        method: Post, 
        headers: [{ key: "Authorization", value: "Bearer $(client.apiKey)" }],
        url: client.url,
        mimeType: "application/json",
        body: encodeRequestBody body,
        timeout: client.requestTimeout,
    }

## Build the request body to be sent in the Http request using ChatML messages
buildRequestBody : Client, List Message -> ChatRequestBody
buildRequestBody = \client, messages -> 
    {   
        messages, 
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
    decoded : Decode.DecodeResult ChatResponseBody
    decoded = Decode.fromBytesPartial cleanedBody decoder
    decoded.result

## Decode the JSON response body to the first message in the list of choices
decodeTopMessageChoice : List U8 -> Result Message [HttpError ApiError, InvalidResponse, NoChoices, BadJson Str]
decodeTopMessageChoice = \responseBodyBytes -> 
    when decodeResponse responseBodyBytes is
        Ok body -> 
            when List.get body.choices 0 is
                Ok choice -> Ok choice.message
                Err _ -> Err NoChoices
        Err _ -> when Shared.decodeErrorResponse responseBodyBytes is
            Ok err -> Err (HttpError err.error)
            Err _ -> 
                when responseBodyBytes |> Str.fromUtf8 is
                Ok str -> Err (BadJson str)
                Err _ -> Err InvalidResponse

## Decode the JSON response body of an API error message
decodeErrorResponse = Shared.decodeErrorResponse

## Encode the request body to be sent in the Http request
encodeRequestBody : ChatRequestBody -> List U8
encodeRequestBody = \body -> 
    Encode.toBytes body (Json.utf8With { 
        fieldNameMapping: SnakeCase, 
        emptyEncodeAsNull: Json.encodeAsNullOption { record: Bool.false },
    })

## Append a system message to the list of messages
appendSystemMessage : List Message, Str -> List Message
appendSystemMessage = \messages, content -> List.append messages { role: "system", content }

## Append a user message to the list of messages
appendUserMessage : List Message, Str -> List Message
appendUserMessage = \messages, content -> List.append messages { role: "user", content }

## Append an assistant message to the list of messages
appendAssistantMessage : List Message, Str -> List Message
appendAssistantMessage = \messages, content -> List.append messages { role: "assistant", content }
