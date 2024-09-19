# Roc package for the OpenRouter.ai API

[![Roc-Lang][roc_badge]][roc_link]

This package is still in WIP 🛠️ stages, so the interface may be subject to change. With that said, the package currently supports:

- Creating and parsing ChatML style requests and responses.
- Creating and parsing raw prompt style requests and responses.
- Formatting prompt strings with `[INST]`, `<<SYS>>`, and `<s>` tags for models with llama style fine-tuning.
- Easy access to over 140 up-to-date models, plus additional older models.
- Most common LLM parameters such as `temperature`, `topP`, `topK`, `repetitionPenalty`, etc.
- OpenRouter specific features like fallback models and provider preferences.
- 🚀 __NEW!__ LLM tool use - this enables the AI model to call Roc functions and use the results in its answers. 


## Example
```roc
import cli.Stdout
import cli.Http
import ai.Chat

main =
    apiKey = "<your_api_key>"
    client = Chat.initClient { apiKey, model: "openai/gpt-4o" }
    messages = Chat.appendUserMessage [] "Hello, world!"
    response = Http.send! (Chat.buildRequest client messages)
    when Chat.decodeTopMessageChoice response.body is
        Ok message -> Stdout.line! message.content
        Err (ApiError err) -> Stdout.line! "$(Num.toStr err.code): $(err.message)"
        Err NoChoices -> Stdout.line! "No message choices in API response"
        Err (BadJson str) -> Stdout.line! "Error parsing JSON:\n$(str)"
        Err DecodingError -> Stdout.line! "Error decoding API response"
```

For complete example apps, including a full chatbot app, see the examples folder.

[roc_badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Fpastebin.com%2Fraw%2FGcfjHKzb
[roc_link]: https://github.com/roc-lang/roc
