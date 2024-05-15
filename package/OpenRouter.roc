module [
    ChatResponse, 
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
    # setResponseFormat,
    setModels,
    setRoute,
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
    updateLLamaChatHistory,
]

import json.Json
import json.Option exposing [Option]

TimeoutConfig : [TimeoutMilliseconds U64, NoTimeout]

## The record used to store configuration for the OpenRouter API client.
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
    # responseFormat : Option { type: Str },
    models : Option (List Str),
    route : Option Str,
}

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

defaultModel = "openrouter/auto"
defaultUrl = "https://openrouter.ai/api/v1/chat/completions"

## Initialize the OpenRouter API client with the required API key.
## Other parameters may optionally be set during initialization, or assigned later.
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
        # responseFormat ? Str,
        models ? List Str,
        route ? [Fallback, NoFallback],
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
        # responseFormat ? "",
        models ? [],
        route ? NoFallback,
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
        # responseFormat: Option.none {},
        models: Option.none {},
        route: Option.none {},
    }
    |> setProviderOrder providerOrder
    |> setSeed seed
    |> setMaxTokens maxTokens
    # |> setResponseFormat responseFormat
    |> setModels models
    |> setRoute route

# expect
#     init { apiKey: "test" } == {
#         apiKey: "test",
#         model: defaultModel,
#         url: defaultUrl,
#         requestTimeout: NoTimeout,
#         providerOrder: Option.none {},
#         temperature: 1.0,
#         topP: 1.0,
#         topK: 0,
#         frequencyPenalty: 0.0,
#         presencePenalty: 0.0,
#         repetitionPenalty: 1.0,
#         minP: 0.0,
#         topA: 0.0,
#         seed: Option.none {},
#         maxTokens: Option.none {},
#     }

## Set the model to be used for the API requests.
setModel : Client, Str -> Client
setModel = \client, model -> { client & model }

## Set the URL to be used for the API requests.
## (Change with care - while the openrouter.ai API is similar to OpenAI's, there may be some unexpected differences.)
setUrl : Client, Str -> Client
setUrl = \client, url -> { client & url }

## Set the request timeout for the API requests.
setRequestTimeout : Client, TimeoutConfig -> Client
setRequestTimeout = \client, requestTimeout -> { client & requestTimeout }

## Set the provider order for the API requests. (Default: [] - use all providers.)
setProviderOrder : Client, List Str -> Client
setProviderOrder = \client, providerOrder -> 
    providerOrderOption = when providerOrder is
        [] -> Option.none {}
        [..] -> Option.some providerOrder
    { client & providerOrder: providerOrderOption }

## Set the temperature for the API requests. (Default: 1.0)
setTemperature : Client, F32 -> Client
setTemperature = \client, temperature -> { client & temperature }

## Set the top_p for the API requests. (Default: 1.0)
setTopP : Client, F32 -> Client
setTopP = \client, topP -> { client & topP }

## Set the top_k for the API requests. (Default: 0)
setTopK : Client, U64 -> Client
setTopK = \client, topK -> { client & topK }

## Set the frequency penalty for the API requests. (Default: 0.0)
setFrequencyPenalty : Client, F32 -> Client
setFrequencyPenalty = \client, frequencyPenalty -> { client & frequencyPenalty }

## Set the presence penalty for the API requests. (Default: 0.0)
setPresencePenalty : Client, F32 -> Client
setPresencePenalty = \client, presencePenalty -> { client & presencePenalty }

## Set the repetition penalty for the API requests. (Default: 1.0)
setRepetitionPenalty : Client, F32 -> Client
setRepetitionPenalty = \client, repetitionPenalty -> { client & repetitionPenalty }

## Set the min_p for the API requests. (Default: 0.0)
setMinP : Client, F32 -> Client
setMinP = \client, minP -> { client & minP }

## Set the top_a for the API requests. (Default: 0.0)
setTopA : Client, F32 -> Client
setTopA = \client, topA -> { client & topA }

## Set the seed for the API requests. (Default: 0 - random seed)
setSeed : Client, U64 -> Client
setSeed = \client, seed -> 
    seedOption = when seed is 
        0 -> Option.none {}
        _ -> Option.some seed
    { client & seed: seedOption }

## Set the max_tokens for the API requests. (Default: 0 - no limit)
setMaxTokens : Client, U64 -> Client
setMaxTokens = \client, maxTokens -> 
    maxTokensOption = when maxTokens is 
        0 -> Option.none {}
        _ -> Option.some maxTokens
    { client & maxTokens: maxTokensOption }

# setResponseFormat : Client, Str -> Client
# setResponseFormat = \client, responseFormat -> 
#     responseFormatOption = if Str.isEmpty responseFormat 
#         then Option.none {}
#         else Option.some { type: responseFormat }
#     { client & responseFormat: responseFormatOption }

setModels : Client, List Str -> Client
setModels = \client, models -> 
    modelsOption = if List.isEmpty models 
        then Option.none {}
        else Option.some models
    { client & models: modelsOption }

setRoute : Client, [Fallback, NoFallback] -> Client
setRoute = \client, route ->
    routeOption = when route is
        NoFallback -> Option.none {}
        Fallback -> Option.some "fallback"
    { client & route: routeOption }

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
formatLLamaPromptWithHistory = \prompt, chatHistory -> 
    chatHistory
    |> Str.concat llamaExchangeStartTag
    |> Str.concat prompt
    
updateLLamaChatHistory : { promptStr: Str, botReply: Str, chatHistory? Str} -> Str
updateLLamaChatHistory = \{ promptStr, botReply, chatHistory? "" } -> 
    chatHistory
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
