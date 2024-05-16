# Roc package for the OpenRouter.ai API

[![Roc-Lang][roc_badge]][roc_link]

This package is still in WIP 🛠️ stages, so the interface may be subject to change. With that said, the package currently supports:

- Creating and parsing ChatML style requests and responses.
- Creating and parsing raw prompt style requests and responses.
- Formatting prompt strings with `[INST]`, `<<SYS>>`, and `<s>` tags for models with llama style fine-tuning.
- Easy access to over 140 up-to-date models, plus additional older models.
- Most common LLM parameters such as `temperature`, `topP`, `topK`, `repetitionPenalty`, etc.
- OpenRouter specific features like fallback models and provider preferences.

## Coming (maybe not so) soon 🚀

- LLM tool use! 🔨🔧🪛⛏️🪓
  - Blocked on [support for Dict type in EncoderFormatting](https://github.com/roc-lang/roc/issues/#5294)

## Example
```roc
import cli.Stdout
import cli.Http
import cli.Task
import ai.Chat

main =
    apiKey = "<your_api_key>"
    client = Chat.initClient { apiKey }
    response = Http.send! (Chat.buildRequest client [])
    messages = Chat.appendUserMessage [] "Hello, world!"
    responseBody =
        when response |> Http.handleStringResponse is
            Err err -> crash (Http.errorToString err)
            Ok body -> body |> Str.toUtf8
    when Chat.decodeResponse responseBody is
        Ok body ->
            when List.get body.choices 0 is
                Ok choice -> Stdout.line choice.message
                Err _ -> Stdout.line "Error getting first choice from API response"
        Err _ -> Stdout.line "Error decoding response body"
```

For complete example apps, including a full chatbot app, see the examples folder.

[roc_badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Fpastebin.com%2Fraw%2FGcfjHKzb
[roc_link]: https://github.com/roc-lang/roc
