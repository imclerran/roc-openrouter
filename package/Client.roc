module [
    Client,
    init,
    setModel,
    setUrl,
    setRequestTimeout,
    setProviderOrder,
    setTemperature,
    setTopP,
    setTopK,
    setFrequencyPenalty,
    setPresencePenalty,
    setRepetitionPenalty,
    setMinP,
    setTopA,
    setSeed,
    setMaxTokens,
    setResponseFormat,
    setModels,
    setRoute,
    setTools,
    defaultModel,
    defaultUrl,
]

import json.Option exposing [Option]
import Shared exposing [TimeoutConfig]
import InternalTools exposing [Tool]

## The record used to store configuration for the OpenRouter API client.
Client : {
    apiKey : Str,
    model : Str,
    url : Str,
    requestTimeout : TimeoutConfig,
    providerOrder : Option (List Str),
    temperature : F32,
    topP : F32,
    topK : U64,
    frequencyPenalty : F32,
    presencePenalty : F32,
    repetitionPenalty : F32,
    minP : F32,
    topA : F32,
    seed : Option U64,
    maxTokens : Option U64,
    responseFormat : { type : Str },
    models : Option (List Str),
    route : Option Str,
    tools: Option (List Tool),
}

defaultModel = "openrouter/auto"
defaultUrl = "https://openrouter.ai/api/v1/chat/completions"

## Initialize the OpenRouter API client with the required API key.
## Other parameters may optionally be set during initialization, or assigned later.
init :
    {
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
        responseFormat ? Str,
        models ? List Str,
        route ? [UseFallback, NoFallback],
        tools ? List Tool,
    }
    -> Client
init = \{ apiKey, model ? defaultModel, url ? defaultUrl, requestTimeout ? NoTimeout, providerOrder ? [], temperature ? 1.0, topP ? 1.0, topK ? 0, frequencyPenalty ? 0.0, presencePenalty ? 0.0, repetitionPenalty ? 1.0, minP ? 0.0, topA ? 0.0, seed ? 0, maxTokens ? 0, responseFormat ? "text", models ? [], route ? NoFallback, tools ? [] } ->
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
        responseFormat: { type: responseFormat },
        models: Option.none {},
        route: Option.none {},
        tools: Option.none {},
    }
    |> setProviderOrder providerOrder
    |> setSeed seed
    |> setMaxTokens maxTokens
    |> setModels models
    |> setRoute route
    |> setTools tools

## Set the model to be used for the API requests.
## Default: "openrouter/auto"
setModel : Client, Str -> Client
setModel = \client, model -> { client & model }

## Set the URL to be used for the API requests.
## (Change with care - while the openrouter.ai API is similar to OpenAI's, there may be some unexpected differences.)
setUrl : Client, Str -> Client
setUrl = \client, url -> { client & url }

## Set the request timeout for the API requests.
## Default: NoTimeout
setRequestTimeout : Client, TimeoutConfig -> Client
setRequestTimeout = \client, requestTimeout -> { client & requestTimeout }

## Set the provider order for the API requests.
## Default: [] - use all providers.
setProviderOrder : Client, List Str -> Client
setProviderOrder = \client, providerOrder ->
    providerOrderOption =
        when providerOrder is
            [] -> Option.none {}
            [..] -> Option.some providerOrder
    { client & providerOrder: providerOrderOption }

## Set the temperature for the API requests.
## Range: [0.0, 2.0]
## Default: 1.0
setTemperature : Client, F32 -> Client
setTemperature = \client, temperature -> { client & temperature }

## Set the top_p for the API requests.
## Range: [0.0, 1.0]
## Default: 1.0
setTopP : Client, F32 -> Client
setTopP = \client, topP -> { client & topP }

## Set the top_k for the API requests.
## Range: [0, Num.maxU64]
## Default: 0
setTopK : Client, U64 -> Client
setTopK = \client, topK -> { client & topK }

## Set the frequency penalty for the API requests.
## Range: [-2.0, 2.0]
## Default: 0.0
setFrequencyPenalty : Client, F32 -> Client
setFrequencyPenalty = \client, frequencyPenalty -> { client & frequencyPenalty }

## Set the presence penalty for the API requests.
## Range: [-2.0, 2.0]
## Default: 0.0
setPresencePenalty : Client, F32 -> Client
setPresencePenalty = \client, presencePenalty -> { client & presencePenalty }

## Set the repetition penalty for the API requests.
## Range: [0.0, 2.0]
## Default: 1.0
setRepetitionPenalty : Client, F32 -> Client
setRepetitionPenalty = \client, repetitionPenalty -> { client & repetitionPenalty }

## Set the min_p for the API requests.
## Range: [0.0, 1.0]
## Default: 0.0
setMinP : Client, F32 -> Client
setMinP = \client, minP -> { client & minP }

## Set the top_a for the API requests.
## Range: [0.0, 1.0]
## Default: 0.0
setTopA : Client, F32 -> Client
setTopA = \client, topA -> { client & topA }

## Set the seed for the API requests.
## OpenAI models only
## Default: 0 - random seed
setSeed : Client, U64 -> Client
setSeed = \client, seed ->
    seedOption =
        when seed is
            0 -> Option.none {}
            _ -> Option.some seed
    { client & seed: seedOption }

## Set the max_tokens for the API requests.
## Range: [1, contextLength]
## Default: 0 == no limit
setMaxTokens : Client, U64 -> Client
setMaxTokens = \client, maxTokens ->
    maxTokensOption =
        when maxTokens is
            0 -> Option.none {}
            _ -> Option.some maxTokens
    { client & maxTokens: maxTokensOption }

## Set the response format to either "text" or "json_object". Not supported by all models.
## Default: "" - no format
setResponseFormat : Client, Str -> Client
setResponseFormat = \client, responseFormat ->
    responseFormatRecord = { type: responseFormat }
    { client & responseFormat: responseFormatRecord }

## Set the models for the auto router to choose from.
## If not set, the auto router will choose from a small selection of the top performing models.
## https://openrouter.ai/models/openrouter/auto
## Default: []
setModels : Client, List Str -> Client
setModels = \client, models ->
    modelsOption =
        if
            List.isEmpty models
        then
            Option.none {}
            else

        Option.some models
    { client & models: modelsOption }

## Set the parameter which determines whether to use a fallback model if the primary model fails.
## OpenRouter will use the models provided in models, or if no models are provided,
## will try a similarly priced model to the primary.
## https://openrouter.ai/docs#model-routing
## Default: NoFallback
setRoute : Client, [UseFallback, NoFallback] -> Client
setRoute = \client, route ->
    routeOption =
        when route is
            NoFallback -> Option.none {}
            UseFallback -> Option.some "fallback"
    { client & route: routeOption }

setTools : Client, List Tool -> Client
setTools = \client, tools ->
    toolsOption =
        if List.isEmpty tools
            then Option.none {}
            else Option.some tools
    { client & tools: toolsOption }
