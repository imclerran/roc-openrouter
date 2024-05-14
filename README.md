# Roc package for the OpenRouter.ai API

This package is still in WIP stages, so the interface may be subject to change. With that said, the package currently supports:

- Creating and parsing ChatML style requests and responses.
- Creating and parsing raw prompt style requests and responses.
- easy access to over 140 up-to-date models, plus additional older models
- many most LLM parameters such as `temperature`, `topP`, topK, `repetitionPenalty`, etc.
- OpenRouter specific features like provider preferences.

## Example
```roc
import Http
import ai.Api as AI

client = AI.init { apiKey }
response = Http.send! (AI.buildChatRequest client [])
messages = AI.appendUserMessage [] "Hello, world!"
responseBody =
    when response |> Http.handleStringResponse is
        Err err -> crash (Http.errorToString err)
        Ok body -> body |> Str.toUtf8
when AI.decodeChatResponse responseBody is
    Ok body ->
        when List.get body.choices 0 is
            Ok choice -> Stdout.line! choice.message
            Err _ -> Stdout.line! "Error getting first choice from API response"
    Err _ -> Stdout.line! "Error decoding response body"
```
For complete example apps, including a full chatbot app, see the examples folder.