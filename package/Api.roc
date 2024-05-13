module [
    ApiResponse, 
    ErrorResponse,
    Client, 
    RequestObject, 
    Message,
    init,
    setModel,
    setProviderOrder,
    setRequestTimeout,
    setTemperature,
    setUrl,
    setTopP,
    setTopK,
    setFrequencyPenalty,
    setPresencePenalty,
    setRepetitionPenalty,
    setMinP,
    setTopA,
    setSeed,
    setMaxTokens,
    createRequest, 
    appendAssistantMessage, 
    appendSystemMessage, 
    appendUserMessage,
    decodeApiResponse,
    decodeErrorResponse,
]

import json.Json
import json.Option exposing [Option]

TimeoutConfig : [TimeoutMilliseconds U64, NoTimeout]

Client : {
    apiKey : Str,
    model : Str,
    url: Str,
    requestTimeout: TimeoutConfig,
    providerOrder : Option (List Str),
    temperature: F32,
    topP: F32,
    topK: U64,
    frequencyPenalty: F32,
    presencePenalty: F32,
    repetitionPenalty: F32,
    minP: F32,
    topA: F32,
    seed: Option U64,
    maxTokens: Option U64,
}

RequestObject : {
    method : [Post],
    headers : List [Header Str Str],
    mimeType : Str,
    url : Str,
    body : List U8,
    timeout : TimeoutConfig,
}

Message : {
    role: Str,
    content: Str,
}

RequestBody : {
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
}

ApiResponse : {
    id : Str,
    model : Str,
    object : Str,
    created : I64,
    choices : List {
        index : U8,
        message : Message,
        finishReason : Str,
    },
    usage : {
        promptTokens : U64,
        completionTokens : U64,
        totalTokens : U64,
        totalCost : F32,
    },
}

ErrorResponse : {
    error : {
        code : U16,
        message : Str,
    },
}

defaultModel = "openrouter/auto"
defaultUrl = "https://openrouter.ai/api/v1/chat/completions"

init : { 
        apiKey : Str, 
        model ? Str, 
        url ? Str, 
        requestTimeout ? TimeoutConfig,
        providerOrder ? List Str,
        temperature ? F32,
        topP ? F32,
        topK ? U64,
        frequencyPenalty ? F32,
        presencePenalty ? F32,
        repetitionPenalty ? F32,
        minP ? F32,
        topA ? F32,
        seed ? U64,
        maxTokens ? U64,
    } -> Client
init = \{
        apiKey,
        model ? defaultModel,
        url ? defaultUrl,
        requestTimeout ? NoTimeout,
        providerOrder ? [],
        temperature ? 1.0,
        topP ? 1.0,
        topK ? 0,
        frequencyPenalty ? 0.0,
        presencePenalty ? 0.0,
        repetitionPenalty ? 1.0,
        minP ? 0.0,
        topA ? 0.0,
        seed ? 0,
        maxTokens ? 0,
    } -> 
    { 
        apiKey, 
        model, 
        url, 
        requestTimeout, 
        providerOrder: Option.none {}, 
        temperature, 
        topP,
        topK,
        frequencyPenalty,
        presencePenalty,
        repetitionPenalty,
        minP,
        topA,
        seed: Option.none {},
        maxTokens: Option.none {},
    }
    |> setProviderOrder providerOrder
    |> setSeed seed
    |> setMaxTokens maxTokens

setModel : Client, Str -> Client
setModel = \client, model -> { client & model }

setUrl : Client, Str -> Client
setUrl = \client, url -> { client & url }

setRequestTimeout : Client, TimeoutConfig -> Client
setRequestTimeout = \client, requestTimeout -> { client & requestTimeout }

setProviderOrder : Client, List Str -> Client
setProviderOrder = \client, providerOrder -> 
    providerOrderOption = when providerOrder is
        [] -> Option.none {}
        [..] -> Option.some providerOrder
    { client & providerOrder: providerOrderOption }

setTemperature : Client, F32 -> Client
setTemperature = \client, temperature -> { client & temperature }

setTopP : Client, F32 -> Client
setTopP = \client, topP -> { client & topP }

setTopK : Client, U64 -> Client
setTopK = \client, topK -> { client & topK }

setFrequencyPenalty : Client, F32 -> Client
setFrequencyPenalty = \client, frequencyPenalty -> { client & frequencyPenalty }

setPresencePenalty : Client, F32 -> Client
setPresencePenalty = \client, presencePenalty -> { client & presencePenalty }

setRepetitionPenalty : Client, F32 -> Client
setRepetitionPenalty = \client, repetitionPenalty -> { client & repetitionPenalty }

setMinP : Client, F32 -> Client
setMinP = \client, minP -> { client & minP }

setTopA : Client, F32 -> Client
setTopA = \client, topA -> { client & topA }

setSeed : Client, U64 -> Client
setSeed = \client, seed -> 
    seedOption = when seed is 
        0 -> Option.none {}
        _ -> Option.some seed
    { client & seed: seedOption }

setMaxTokens : Client, U64 -> Client
setMaxTokens = \client, maxTokens -> 
    maxTokensOption = when maxTokens is 
        0 -> Option.none {}
        _ -> Option.some maxTokens
    { client & maxTokens: maxTokensOption }

createRequest : Client, List Message -> RequestObject
createRequest = \client, messages -> 
    body = buildRequestBody client messages
    {
        method: Post, 
        headers: [Header "Authorization" "Bearer $(client.apiKey)"],
        url: client.url,
        mimeType: "application/json",
        body: encodeRequestBody body,
        timeout: client.requestTimeout,
    }

buildRequestBody : Client, List Message -> RequestBody
buildRequestBody = \client, messages -> 
    {   
        model: client.model, 
        messages, temperature: 
        client.temperature,
        topA: client.topA,
        topP: client.topP,
        topK: client.topK,
        frequencyPenalty: client.frequencyPenalty,
        presencePenalty: client.presencePenalty,
        repetitionPenalty: client.repetitionPenalty,
        minP: client.minP,
        seed: client.seed,
        maxTokens: client.maxTokens,
        provider: { order: client.providerOrder } 
    }

decodeApiResponse : List U8 -> Result ApiResponse _
decodeApiResponse = \bodyBytes -> 
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult ApiResponse
    decoded = Decode.fromBytesPartial bodyBytes decoder
    decoded.result

decodeErrorResponse : List U8 -> Result ErrorResponse _
decodeErrorResponse = \bodyBytes -> 
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult ErrorResponse
    decoded = Decode.fromBytesPartial bodyBytes decoder
    decoded.result

encodeRequestBody : RequestBody -> List U8
encodeRequestBody = \body -> 
    Encode.toBytes body (Json.utf8With { 
        fieldNameMapping: SnakeCase, 
        emptyEncodeAsNull: Json.encodeAsNullOption { record: Bool.false },
    })

appendSystemMessage : List Message, Str -> List Message
appendSystemMessage = \messages, content -> List.append messages { role: "system", content }

appendUserMessage : List Message, Str -> List Message
appendUserMessage = \messages, content -> List.append messages { role: "user", content }

appendAssistantMessage : List Message, Str -> List Message
appendAssistantMessage = \messages, content -> List.append messages { role: "assistant", content }
